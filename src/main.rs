use clap::{Arg, ArgAction, Command};
use std::path::Path;
use tracing::debug;
use tracing_subscriber;

use technique::parsing;
use technique::formatting;

mod rendering;

fn main() {
    const VERSION: &str = concat!("v", env!("CARGO_PKG_VERSION"));

    // Initialize the tracing subscriber
    tracing_subscriber::fmt::init();

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
                .about("Syntax- and type-check the given procedure")
                .arg(
                    Arg::new("watch")
                        .long("watch")
                        .action(clap::ArgAction::SetTrue)
                        .help("Watch the given procedure file and recompile if changes are detected."),
                )
                .arg(
                    Arg::new("filename")
                        .required(true)
                        .help("The file containing the code for the procedure you want to type-check."),
                ),
        )
        .subcommand(
            Command::new("format")
                .about("Code format the given procedure")
                .arg(
                    Arg::new("raw-control-chars")
                        .short('R')
                        .long("raw-control-chars")
                        .action(ArgAction::SetTrue)
                        .help("Emit ANSI escape codes for syntax highlighting even if output is redirected to a pipe or file."),
                )
                .arg(
                    Arg::new("filename")
                        .required(true)
                        .help("The file containing the code for the procedure you want to format."),
                ),
        )
        .subcommand(
            Command::new("render")
                .about("Render the Technique procedure into a printable PDF")
                .long_about("Render the Technique procedure into a printable \
                    PDF. By default this will highlight the source of the \
                    input file for the purposes of reviewing the raw \
                    procedure.")
                .arg(
                    Arg::new("filename")
                        .required(true)
                        .help("The file containing the code for the procedure you want to print."),
                ),
        )
        .get_matches();

    match matches.subcommand() {
        Some(("check", submatches)) => {
            let watching = submatches
                .get_one::<bool>("watch")
                .unwrap(); // flags are always present since SetTrue implies default_value

            debug!(watching);

            let filename = submatches
                .get_one::<String>("filename")
                .unwrap(); // argument are required by definition so always present

            debug!(filename);

            let content = parsing::load(&Path::new(filename));
            let technique = parsing::parse(&content);
            // TODO continue with validation of the returned technique

            println!("{:?}", technique);
        }
        Some(("format", submatches)) => {
            let raw_output = submatches
                .get_one::<bool>("raw-control-chars")
                .unwrap(); // flags are always present since SetTrue implies default_value

            debug!(raw_output);

            let filename = submatches
                .get_one::<String>("filename")
                .unwrap(); // argument are required by definition so always present

            debug!(filename);

            let content = parsing::load(&Path::new(filename));
            let technique = parsing::parse(&content);

            let result = formatting::format(&technique);
            print!("{}", result);
        }
        Some(("render", submatches)) => {
            let filename = submatches
                .get_one::<String>("filename")
                .unwrap(); // argument are required by definition so always present

            debug!(filename);

            rendering::via_typst(&Path::new(filename));
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
