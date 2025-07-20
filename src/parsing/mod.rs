// parser for the Technique language
use std::path::Path;

mod parser;
mod scope;

pub fn load(source: &Path) {
    // read source to a str
    let content = std::fs::read_to_string(source).expect("Failed to read the source file");

    let result = parser::parse_via_taking(content.as_str());

    match result {
        Ok(technique) => {
            if let Some(procedures) = &technique.body {
                println!("Found {} procedure(s):", procedures.len());
                for procedure in procedures {
                    println!(
                        "  - {}",
                        procedure
                            .name
                            .0
                    );
                }
                println!();
            } else {
                println!("No procedures found");
            }

            println!("{:#?}", technique);
        }
        Err(error) => {
            eprintln!("error: {}", error);
            std::process::exit(1);
        }
    };
}
