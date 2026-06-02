use clap::builder::{PossibleValue, TypedValueParser};
use clap::value_parser;
use clap::{Arg, ArgAction, Command};
use owo_colors::OwoColorize;
use std::io::IsTerminal;
use std::path::Path;
use std::str::FromStr;
use tracing::debug;
use tracing_subscriber::{self, EnvFilter};

use technique::formatting::{self, Identity};
use technique::highlighting::{self, Terminal};
use technique::linking;
use technique::parsing;
use technique::runner::{self, Library, Outcome, RunId};
use technique::templating::{self, Checklist, NasaEsaIss, Procedure, Recipe, Source};
use technique::translation;

mod editor;
mod output;
mod problem;

#[derive(Eq, Debug, PartialEq)]
#[allow(dead_code)]
enum Output {
    Terminal,
    Native,
    Silent,
}

#[derive(Eq, Debug, PartialEq)]
enum Phase {
    Parsing,
    Translation,
    Linking,
}

// Page dimensions in millimetres
#[derive(Clone, Debug)]
struct PaperSize {
    width: f64,
    height: f64,
}

impl FromStr for PaperSize {
    type Err = String;

    fn from_str(s: &str) -> Result<Self, Self::Err> {
        let (width, height) = match s {
            "a4" => (210.0, 297.0),
            "a5" => (148.0, 210.0),
            "letter" => (215.9, 279.4),
            _ => {
                let (w, h) = s
                    .split_once(|c: char| c == 'x' || c == '×')
                    .ok_or_else(|| format!("invalid paper size '{}'", s))?;
                let w: f64 = w
                    .trim()
                    .parse()
                    .map_err(|e| format!("invalid width '{}': {}", w, e))?;
                let h: f64 = h
                    .trim()
                    .parse()
                    .map_err(|e| format!("invalid height '{}': {}", h, e))?;
                (w, h)
            }
        };
        Ok(PaperSize { width, height })
    }
}

// Custom clap parser so the named presets show up under "possible values" in
// help output, while still allowing the parser to accept free-form width x
// height dimensions.
#[derive(Clone)]
struct PaperSizeParser;

impl TypedValueParser for PaperSizeParser {
    type Value = PaperSize;

    fn parse_ref(
        &self,
        cmd: &Command,
        arg: Option<&Arg>,
        value: &std::ffi::OsStr,
    ) -> Result<Self::Value, clap::Error> {
        let s = value
            .to_str()
            .ok_or_else(|| clap::Error::new(clap::error::ErrorKind::InvalidUtf8).with_cmd(cmd))?;
        s.parse::<PaperSize>()
            .map_err(|e| {
                let mut err =
                    clap::Error::new(clap::error::ErrorKind::ValueValidation).with_cmd(cmd);
                if let Some(arg) = arg {
                    err.insert(
                        clap::error::ContextKind::InvalidArg,
                        clap::error::ContextValue::String(arg.to_string()),
                    );
                }
                err.insert(
                    clap::error::ContextKind::InvalidValue,
                    clap::error::ContextValue::String(s.to_string()),
                );
                err.insert(
                    clap::error::ContextKind::Custom,
                    clap::error::ContextValue::String(e),
                );
                err
            })
    }

    fn possible_values(&self) -> Option<Box<dyn Iterator<Item = PossibleValue> + '_>> {
        Some(Box::new(
            ["a4", "a5", "letter", "WxH"]
                .into_iter()
                .map(PossibleValue::new),
        ))
    }
}

fn main() {
    const VERSION: &str = concat!("v", env!("CARGO_PKG_VERSION"));

    // Initialize the tracing subscriber. This respects the RUST_LOG
    // environment variable if present, or sets Level::ERROR as a fallback.
    let filter = EnvFilter::from_default_env();
    tracing_subscriber::fmt()
        .with_env_filter(filter)
        .init();

    let matches = Command::new("technique")
        .version(VERSION)
        .propagate_version(true)
        .author("Andrew Cowie")
        .about("The Technique procedure language.")
        .disable_help_subcommand(true)
        .disable_help_flag(true)
        .disable_version_flag(true)
        .arg(
            Arg::new("help")
                .long("help")
                .long_help("Print help")
                .global(true)
                .hide(true)
                .action(ArgAction::Help))
        .arg(
            Arg::new("version")
                .long("version")
                .long_help("Print version")
                .global(true)
                .hide(true)
                .action(ArgAction::Version))
        .subcommand(
            Command::new("check")
                .about("Validate the syntax, structure, and types in the given Technique document.")
                .arg(
                    Arg::new("watch")
                        .long("watch")
                        .action(clap::ArgAction::SetTrue)
                        .help("Watch the given file containing a Technique document and recompile if changes are detected."),
                )
                .arg(
                    Arg::new("output")
                        .short('o')
                        .long("output")
                        .value_name("type")
                        .value_parser(["native", "none"])
                        .default_value("none")
                        .action(ArgAction::Set)
                        .help("Which kind of diagnostic output to print when checking.")
                )
                .arg(
                    Arg::new("until")
                        .long("until")
                        .value_name("phase")
                        .value_parser(["parsing", "translation", "linking"])
                        .default_value("linking")
                        .action(ArgAction::Set)
                        .help("Stop compilation early, after the given phase is complete. \
                            Use this in conjunction with the --output option so that the result can be inspected. The phases are: \
                            parsing, where the input is parsed from the surface language to an internal abstract syntax tree; \
                            translation, which resolves names, checks references, and ensures the input is valid Technique; then finally \
                            linking, which ensures functions being called are available, checks parameters being passed, and provides the context to the execution environment.")
                )
                .arg(
                    Arg::new("filename")
                        .required(true)
                        .help("The file containing the code for the Technique you want to parse and type check, or - to read from standard input."),
                ),
        )
        .subcommand(
            Command::new("format")
                .about("Format the code in the given Technique document.")
                .arg(
                    Arg::new("raw-control-chars")
                        .short('R')
                        .long("raw-control-chars")
                        .action(ArgAction::SetTrue)
                        .help("Emit ANSI escape codes for syntax highlighting even if output is redirected to a pipe or file."),
                )
                .arg(
                    Arg::new("wrap-width")
                        .short('w')
                        .long("width")
                        .value_name("COLUMN")
                        .value_parser(value_parser!(u8))
                        .action(ArgAction::Set)
                        .help("The column at which to wrap descriptive text.")
                )
                .arg(
                    Arg::new("filename")
                        .required(true)
                        .help("The file containing the code for the Technique you want to format, or - to read from standard input."),
                ),
        )
        .subcommand(
            Command::new("render")
                .about("Render the Technique document into a printable PDF.")
                .long_about("Render the Technique document into a formatted \
                PDF using a template. This allows you to transform the code of \
                the procedure into the intended layout suitable to the \
                domain of your application.")
                .arg(
                    Arg::new("output")
                        .short('o')
                        .long("output")
                        .value_name("type")
                        .value_parser(["pdf", "typst"])
                        .default_value("pdf")
                        .action(ArgAction::Set)
                        .help("Whether to write PDF to a file on disk, or print the Typst markup that would be used to create that PDF (for debugging)."),
                )
                .arg(
                    Arg::new("domain")
                        .short('d')
                        .long("domain")
                        .value_parser(["checklist", "nasa-esa-iss", "procedure", "recipe", "source"])
                        .action(ArgAction::Set)
                        .help("The kind of procedure this Technique document represents. By default the value specified in the input document's metadata will be used, falling back to source if unspecified."),
                )
                .arg(
                    Arg::new("template")
                        .short('t')
                        .long("template")
                        .value_name("filename")
                        .action(ArgAction::Set)
                        .help("Path to a Typst template file for rendering."),
                )
                .arg(
                    Arg::new("paper")
                        .short('p')
                        .long("paper")
                        .value_name("SIZE")
                        .value_parser(PaperSizeParser)
                        .action(ArgAction::Set)
                        .help("Paper size for the rendered output. You can use the name of one of the well-known standard sizes, or give explicit dimensions for the width and height in millimetres (\"140x210\", for example). If a paper size is not specified, the template's default will be used."),
                )
                .arg(
                    Arg::new("keep")
                        .short('k')
                        .long("keep")
                        .action(ArgAction::SetTrue)
                        .help("Keep the generated intermediate files in place after rendering. This allows you to do iterative development of the template and styling with the Typst compiler without having to regenerate the input document every time. The intermediate pieces are written as hidden files in the same directory as the source document."),
                )
                .arg(
                    Arg::new("filename")
                        .required(true)
                        .help("The file containing the Technique you want to render."),
                ),
        )
        .subcommand(
            Command::new("run")
                .about("Interactively work through a Technique procedure.")
                .long_about("Walk through the steps of a Technique procedure interactively, \
                    prompting you at each step and recording results locally. \
                    When a Technique document is instantiated as a running procedure \
                    it is allocated a unique identifier. That identifier can be used with \
                    `technique resume` to continue an interrupted workflow.")
                .arg(
                    Arg::new("filename")
                        .required(true)
                        .help("The file containing the Technique document to run."),
                )
                .arg(
                    Arg::new("arguments")
                        .num_args(0..)
                        .action(ArgAction::Append)
                        .help("Values here, if any, will be bound as the entry procedure's parameters."),
                ),
        )
        .subcommand(
            Command::new("resume")
                .about("Resume an interrupted procedure.")
                .arg(
                    Arg::new("id")
                        .required(true)
                        .help("The identifier of the run to continue. Can be written as `000007` or just `7`."),
                ),
        )
        .subcommand(
            Command::new("language")
                .about("Language Server Protocol integration for editors and IDEs.")
                .hide(true)
                .long_about("Run a Language Server Protocol (LSP) service \
                   for Technique documents. This accepts commands and code \
                   input via stdin and returns compilation errors and other \
                   diagnostics.")
        )
        .get_matches();

    match matches.subcommand() {
        Some(("check", submatches)) => {
            let watching = submatches
                .get_one::<bool>("watch")
                .unwrap(); // flags are always present since SetTrue implies default_value

            debug!(watching);

            let output = submatches
                .get_one::<String>("output")
                .unwrap();
            let output = match output.as_str() {
                "native" => Output::Native,
                "none" => Output::Terminal,
                _ => panic!("Unrecognized --output value"),
            };

            debug!(?output);

            let until = submatches
                .get_one::<String>("until")
                .unwrap();
            let until = match until.as_str() {
                "parsing" => Phase::Parsing,
                "translation" => Phase::Translation,
                "linking" => Phase::Linking,
                _ => panic!("Unrecognized --until value"),
            };

            debug!(?until);

            let filename = submatches
                .get_one::<String>("filename")
                .unwrap(); // argument are required by definition so always present

            debug!(filename);

            let filename = Path::new(filename);
            let content = match parsing::load(&filename) {
                Ok(data) => data,
                Err(error) => {
                    eprintln!("{}", problem::concise_loading_error(&error));
                    std::process::exit(1);
                }
            };

            let technique = match parsing::parse(&filename, &content) {
                Ok(document) => document,
                Err(errors) => {
                    for (i, error) in errors
                        .iter()
                        .enumerate()
                    {
                        if i > 0 {
                            eprintln!();
                        }
                        eprintln!(
                            "{}",
                            problem::full_parsing_error(&error, &filename, &content, &Terminal)
                        );
                    }
                    std::process::exit(1);
                }
            };

            if let Phase::Parsing = until {
                match output {
                    Output::Terminal => {
                        eprintln!("{}", "ok".bright_green());
                    }
                    Output::Native => {
                        println!("{:#?}", technique);
                    }
                    Output::Silent => {}
                }
                std::process::exit(0);
            }

            let mut program = match translation::translate(&technique) {
                Ok(program) => program,
                Err(errors) => {
                    for (i, error) in errors
                        .iter()
                        .enumerate()
                    {
                        if i > 0 {
                            eprintln!();
                        }
                        eprintln!(
                            "{}",
                            problem::full_translation_error(&error, &filename, &content, &Terminal)
                        );
                    }
                    std::process::exit(1);
                }
            };

            if let Phase::Translation = until {
                match output {
                    Output::Terminal => {
                        eprintln!("{}", "ok".bright_green());
                    }
                    Output::Native => {
                        println!("{:#?}", program);
                    }
                    Output::Silent => {}
                }
                std::process::exit(0);
            }

            let library = Library::core();
            if let Err(errors) = linking::link(&mut program, &library) {
                for (i, error) in errors
                    .iter()
                    .enumerate()
                {
                    if i > 0 {
                        eprintln!();
                    }
                    eprintln!(
                        "{}",
                        problem::full_linking_error(&error, &filename, &content, &Terminal)
                    );
                }
                std::process::exit(1);
            }

            if let Phase::Linking = until {
                match output {
                    Output::Terminal => {
                        eprintln!("{}", "ok".bright_green());
                    }
                    Output::Native => {
                        println!("{:#?}", program);
                    }
                    Output::Silent => {}
                }
                std::process::exit(0);
            }
        }
        Some(("format", submatches)) => {
            let raw_output = *submatches
                .get_one::<bool>("raw-control-chars")
                .unwrap(); // flags are always present since SetTrue implies default_value

            debug!(raw_output);

            let wrap_width = *submatches
                .get_one::<u8>("wrap-width")
                .unwrap_or(&78);

            debug!(wrap_width);

            let filename = submatches
                .get_one::<String>("filename")
                .unwrap(); // argument are required by definition so always present

            debug!(filename);

            let filename = Path::new(filename);
            let content = match parsing::load(&filename) {
                Ok(data) => data,
                Err(error) => {
                    eprintln!("{}", problem::concise_loading_error(&error));
                    std::process::exit(1);
                }
            };

            let technique = match parsing::parse(&filename, &content) {
                Ok(document) => document,
                Err(errors) => {
                    for (i, error) in errors
                        .iter()
                        .enumerate()
                    {
                        if i > 0 {
                            eprintln!();
                        }
                        eprintln!(
                            "{}",
                            problem::concise_parsing_error(&error, &filename, &content, &Terminal)
                        );
                    }

                    eprintln!(
                        "\nUnable to parse input file. Try `technique check {}` for details.",
                        &filename.to_string_lossy()
                    );
                    std::process::exit(1);
                }
            };

            let result;
            if raw_output || std::io::stdout().is_terminal() {
                result = highlighting::render(&Terminal, &technique, wrap_width);
            } else {
                result = highlighting::render(&Identity, &technique, wrap_width);
            }

            print!("{}", result);
        }
        Some(("render", submatches)) => {
            let output = submatches
                .get_one::<String>("output")
                .unwrap();

            debug!(output);

            let filename = submatches
                .get_one::<String>("filename")
                .unwrap(); // argument are required by definition so always present

            debug!(filename);

            let filename = Path::new(filename);
            let content = match parsing::load(&filename) {
                Ok(data) => data,
                Err(error) => {
                    eprintln!("{}", problem::concise_loading_error(&error));
                    std::process::exit(1);
                }
            };

            let technique = match parsing::parse(&filename, &content) {
                Ok(document) => document,
                Err(errors) => {
                    // It is possible that we will want to render the error
                    // into the PDF document rather than crashing here. We'll
                    // see in the future.

                    for (i, error) in errors
                        .iter()
                        .enumerate()
                    {
                        if i > 0 {
                            eprintln!();
                        }

                        eprintln!(
                            "{}",
                            problem::concise_parsing_error(&error, &filename, &content, &Terminal)
                        );
                    }
                    std::process::exit(1);
                }
            };

            // If present the value of the --domain option will override the
            // document's metadata domain line. If neither is specified then
            // the fallback default is "source".

            let domain = submatches.get_one::<String>("domain");
            let domain: &str = match domain {
                Some(value) => value,
                None => technique
                    .header
                    .as_ref()
                    .and_then(|m| m.domain)
                    .unwrap_or("source"),
            };

            debug!(domain);

            // Select domain
            let template: &dyn templating::Template = match domain {
                "source" => &Source,
                "checklist" => &Checklist,
                "nasa-esa-iss" => &NasaEsaIss,
                "procedure" => &Procedure,
                "recipe" => &Recipe,
                other => {
                    eprintln!(
                        "{}: unrecognized domain \"{}\"",
                        "error".bright_red(),
                        other
                    );
                    std::process::exit(1);
                }
            };

            let custom = match submatches.get_one::<String>("template") {
                Some(path) => {
                    if !Path::new(path).exists() {
                        eprintln!(
                            "{}: template file not found: {}",
                            "error".bright_red(),
                            path
                        );
                        std::process::exit(1);
                    }
                    Some(path.as_str())
                }
                None => None,
            };

            let (paper_width, paper_height) = match submatches.get_one::<PaperSize>("paper") {
                Some(p) => (p.width, p.height),
                None => template
                    .default_paper()
                    .unwrap_or((210.0, 297.0)),
            };

            debug!(paper_width, paper_height);

            let markup = template.markup(&technique);
            let document = templating::assemble(
                template.domain(),
                &markup,
                custom,
                paper_width,
                paper_height,
            );

            let keep = *submatches
                .get_one::<bool>("keep")
                .unwrap();

            match output.as_str() {
                "typst" => {
                    print!("{}", document);
                }
                "pdf" => {
                    output::via_typst(
                        filename,
                        template.typst(),
                        template.domain(),
                        &document,
                        keep,
                    );
                }
                _ => panic!("Unrecognized --output value"),
            }
        }
        Some(("run", submatches)) => {
            let filename = submatches
                .get_one::<String>("filename")
                .unwrap();

            debug!(filename);

            let arguments: Vec<String> = submatches
                .get_many::<String>("arguments")
                .map(|values| {
                    values
                        .cloned()
                        .collect()
                })
                .unwrap_or_default();

            debug!(?arguments);

            let filename = Path::new(filename);
            let content = match parsing::load(&filename) {
                Ok(data) => data,
                Err(error) => {
                    eprintln!("{}", problem::concise_loading_error(&error));
                    std::process::exit(1);
                }
            };

            let technique = match parsing::parse(&filename, &content) {
                Ok(document) => document,
                Err(errors) => {
                    for (i, error) in errors
                        .iter()
                        .enumerate()
                    {
                        if i > 0 {
                            eprintln!();
                        }
                        eprintln!(
                            "{}",
                            problem::concise_parsing_error(&error, &filename, &content, &Terminal)
                        );
                    }

                    eprintln!(
                        "\nUnable to parse input file. Try `technique check {}` for details.",
                        &filename.to_string_lossy()
                    );
                    std::process::exit(1);
                }
            };

            let mut program = match translation::translate(&technique) {
                Ok(program) => program,
                Err(errors) => {
                    for (i, error) in errors
                        .iter()
                        .enumerate()
                    {
                        if i > 0 {
                            eprintln!();
                        }
                        eprintln!(
                            "{}",
                            problem::concise_translation_error(
                                &error, &filename, &content, &Terminal
                            )
                        );
                    }
                    std::process::exit(1);
                }
            };

            let library = Library::core();
            if let Err(errors) = linking::link(&mut program, &library) {
                for (i, error) in errors
                    .iter()
                    .enumerate()
                {
                    if i > 0 {
                        eprintln!();
                    }
                    eprintln!(
                        "{}",
                        problem::concise_linking_error(&error, &filename, &content, &Terminal)
                    );
                }
                std::process::exit(1);
            }

            match runner::start(filename, &program, &arguments, library) {
                Ok((run_id, Outcome::Quit)) => {
                    eprintln!("paused; resume with `technique resume {}`", run_id.render());
                    std::process::exit(0);
                }
                Ok((_, _)) => std::process::exit(0),
                Err(error) => {
                    eprintln!("{}", problem::concise_runner_error(&error, &Terminal));
                    std::process::exit(1);
                }
            }
        }
        Some(("resume", submatches)) => {
            let id = submatches
                .get_one::<String>("id")
                .unwrap();

            debug!(id);

            let run_id = match RunId::parse(id) {
                Ok(run_id) => run_id,
                Err(error) => {
                    eprintln!("{}", problem::concise_runner_error(&error, &Terminal));
                    std::process::exit(1);
                }
            };

            let filename = match runner::locate(run_id) {
                Ok(path) => path,
                Err(error) => {
                    eprintln!("{}", problem::concise_runner_error(&error, &Terminal));
                    std::process::exit(1);
                }
            };

            let content = match parsing::load(&filename) {
                Ok(data) => data,
                Err(error) => {
                    eprintln!("{}", problem::concise_loading_error(&error));
                    std::process::exit(1);
                }
            };

            let technique = match parsing::parse(&filename, &content) {
                Ok(document) => document,
                Err(errors) => {
                    for (i, error) in errors
                        .iter()
                        .enumerate()
                    {
                        if i > 0 {
                            eprintln!();
                        }
                        eprintln!(
                            "{}",
                            problem::full_parsing_error(&error, &filename, &content, &Terminal)
                        );
                    }
                    std::process::exit(1);
                }
            };

            let mut program = match translation::translate(&technique) {
                Ok(program) => program,
                Err(errors) => {
                    for (i, error) in errors
                        .iter()
                        .enumerate()
                    {
                        if i > 0 {
                            eprintln!();
                        }
                        eprintln!(
                            "{}",
                            problem::concise_translation_error(
                                &error, &filename, &content, &Terminal
                            )
                        );
                    }
                    std::process::exit(1);
                }
            };

            let library = Library::core();
            if let Err(errors) = linking::link(&mut program, &library) {
                for (i, error) in errors
                    .iter()
                    .enumerate()
                {
                    if i > 0 {
                        eprintln!();
                    }
                    eprintln!(
                        "{}",
                        problem::concise_linking_error(&error, &filename, &content, &Terminal)
                    );
                }
                std::process::exit(1);
            }

            match runner::resume(run_id, &program, library) {
                Ok(Outcome::Quit) => {
                    eprintln!(
                        "paused; continue with `technique resume {}`",
                        run_id.render()
                    );
                    std::process::exit(0);
                }
                Ok(_) => std::process::exit(0),
                Err(error) => {
                    eprintln!("{}", problem::concise_runner_error(&error, &Terminal));
                    std::process::exit(1);
                }
            }
        }
        Some(("language", _)) => {
            debug!("Starting Language Server");

            editor::run_language_server();
        }
        Some(_) => {
            println!("No valid subcommand was used")
        }
        None => {
            println!("usage: technique [COMMAND] ...");
            println!("Try '--help' for more information.");
        }
    }
}
