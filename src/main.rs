use clap::{Arg, ArgAction, Command};

fn main() {
    const VERSION: &str = concat!("v", env!("CARGO_PKG_VERSION"));

    let matches = Command::new("technique")
        .version(VERSION)
        .propagate_version(true)
        .author("Andrew Cowie")
        .about("The Technique Procedures Language.")
        .disable_help_subcommand(true)
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
        .get_matches();

    match matches.subcommand() {
        Some(("check", submatches)) => {
            if submatches.contains_id("watch") {
                println!("Check command executed with watch option");
            }
            if let Some(filename) = submatches.get_one::<String>("filename") {
                println!("Check command executed with filename: {}", filename);
            }
        }
        Some(("format", submatches)) => {
            if submatches.contains_id("raw-control-chars") {
                println!("Format command executed with raw-control-chars option");
            }
            if let Some(filename) = submatches.get_one::<String>("filename") {
                println!("Format command executed with filename: {}", filename);
            }
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
