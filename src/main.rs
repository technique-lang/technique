use clap::value_parser;
use clap::{Arg, ArgAction, Command};
use owo_colors::OwoColorize;
use std::io::IsTerminal;
use std::path::Path;
use tracing::debug;
use tracing_subscriber::{self, EnvFilter};

use technique::formatting::{self, Identity};
use technique::highlighting::{self, Terminal};
use technique::parsing;
use technique::templating::{self, Checklist, Procedure, Source};

mod editor;
mod output;
mod problem;

#[derive(Eq, Debug, PartialEq)]
enum Output {
    Native,
    Silent,
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
        .about("The Technique Procedures Language.")
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
                domain you're app.")
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
                        .value_parser(["checklist", "procedure", "recipe", "source"])
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
                    Arg::new("filename")
                        .required(true)
                        .help("The file containing the Technique you want to render."),
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
                "none" => Output::Silent,
                _ => panic!("Unrecognized --output value"),
            };

            debug!(?output);

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

            // TODO continue with validation of the returned technique

            eprintln!("{}", "ok".bright_green());

            if let Output::Native = output {
                println!("{:#?}", technique);
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
                "procedure" => &Procedure,
                other => {
                    eprintln!(
                        "{}: unrecognized domain \"{}\"",
                        "error".bright_red(),
                        other
                    );
                    std::process::exit(1);
                }
            };

            let data = template.data(&technique);

            // If --template is given, use the user-supplied file (expected to
            // be a .typ file containing Typst template code) ; otherwise
            // inline the built-in template.
            let preamble: String = match submatches.get_one::<String>("template") {
                Some(path) => {
                    if !Path::new(path).exists() {
                        eprintln!(
                            "{}: template file not found: {}",
                            "error".bright_red(),
                            path
                        );
                        std::process::exit(1);
                    }
                    format!("#import \"{}\": render", path)
                }
                None => template
                    .typst()
                    .to_string(),
            };

            match output.as_str() {
                "typst" => {
                    println!("{}", preamble);
                    print!("{}", data);
                    println!("\n#render(technique)");
                }
                "pdf" => {
                    output::via_typst(filename, &preamble, &data);
                }
                _ => panic!("Unrecognized --output value"),
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
