// Error-case checks for translation. Populated as translation steps land
// their corresponding error variants.

use super::*;

#[test]
fn translation_error_variants_construct() {
    let _ = TranslationError::OrphanResponse;
}
