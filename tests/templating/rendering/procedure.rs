use std::path::Path;

use technique::templating::Procedure;

#[test]
fn ensure_render() {
    super::check_directory(Path::new("examples/minimal/"), &Procedure);
    super::check_directory(Path::new("examples/prototype/"), &Procedure);
}
