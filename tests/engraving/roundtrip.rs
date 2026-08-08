use std::fs;
use std::path::Path;

use technique::engraving::parse_records;
use technique::reporting::render_pfftt;

use crate::common::list_files;

/// Every recorded trail must survive being parsed back into records and
/// rendered again, byte for byte. This is what lets `technique log
/// --output=pfftt` claim to be the stored format when the records no longer
/// come from a file: a codec that drops a field, reorders one, or loses an
/// escape shows up here.
#[test]
fn ensure_records_render_as_stored() {
    let dir = Path::new("tests/golden/runner/");
    let files = list_files(dir, "pfftt");

    let mut failures = Vec::new();

    for file in &files {
        let content = fs::read_to_string(file)
            .unwrap_or_else(|e| panic!("Failed to read {:?}: {:?}", file, e));

        let records = match parse_records(&content) {
            Ok(records) => records,
            Err(e) => {
                println!("File {:?} failed to parse: {:?}", file, e);
                failures.push(file.clone());
                continue;
            }
        };

        if render_pfftt(&records) != content {
            println!("File {:?} did not render back as stored", file);
            failures.push(file.clone());
        }
    }

    assert!(
        failures.is_empty(),
        "trails failed to round-trip: {:?}",
        failures
    );
}
