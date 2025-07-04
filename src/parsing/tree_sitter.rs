use tree_sitter::{Parser, Tree};

extern "C" {
    fn tree_sitter_technique() -> tree_sitter::Language;
}

pub fn parse_tree(source_code: &str) -> Tree {
    let mut parser = Parser::new();
    let language = unsafe { tree_sitter_technique() };
    parser
        .set_language(&language)
        .unwrap();
    parser
        .parse(source_code, None)
        .unwrap()
}
