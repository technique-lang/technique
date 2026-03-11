use std::path::Path;

use technique::templating::Checklist;

#[test]
fn ensure_render() {
    super::check_directory(Path::new("examples/minimal/"), &Checklist);
    super::check_directory(Path::new("examples/prototype/"), &Checklist);
}
