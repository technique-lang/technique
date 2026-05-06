// Error-case checks for translation. Source strings parsed through the
// real parser inline match what the runner sees in production.

use std::path::Path;

use crate::language;
use crate::parsing;
use crate::translation::{translate, TranslationError};

#[test]
fn translation_error_variants_construct() {
    let _ = TranslationError::OrphanResponse;
}

#[test]
fn duplicate_procedure_name_is_error() {
    let source = r#"
% technique v1

make_coffee :

make_coffee :
        "#
    .trim_ascii();
    let path = Path::new("test.tq");
    let document = parsing::parse(path, source).expect("parse");
    let errors = translate(&document).expect_err("translate should fail");

    assert_eq!(errors.len(), 1);
    assert_eq!(
        errors[0],
        TranslationError::DuplicateProcedure(language::Identifier("make_coffee"))
    );
}
