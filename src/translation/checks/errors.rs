// Error-case checks for translation. Currently a placeholder; populated as
// translation steps land their corresponding error variants
// (duplicate procedure name, orphan response, etc.).

use super::*;

#[test]
fn placeholder() {
    // Ensures the module compiles before real error tests are added.
    let _ = TranslationError::Placeholder(std::marker::PhantomData);
}
