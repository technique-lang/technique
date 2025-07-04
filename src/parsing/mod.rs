// parser for the Technique language
use std::path::Path;

mod parser;
mod scope;
mod tree_sitter;

pub fn load(source: &Path) {
    // read source to a str
    let content = std::fs::read_to_string(source).expect("Failed to read the source file");

    let tree = tree_sitter::parse_tree(content.as_str());
    println!(
        "{}",
        tree.root_node()
            .to_sexp()
    );
}
