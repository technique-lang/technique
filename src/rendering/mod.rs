use std::path::Path;
use tracing::info;

pub(crate) fn via_typst(filename: &Path) {
    info!("Printing file: {:?}", filename);
}
