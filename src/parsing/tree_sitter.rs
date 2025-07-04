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

#[cfg(test)]
mod check {
    use super::*;

    fn trim(s: &str) -> &str {
        s.strip_prefix('\n')
            .unwrap_or(s)
    }

    fn assert_no_error(tree: &tree_sitter::Tree, input: &str) {
        let root = tree.root_node();
        assert!(
            !root.has_error(),
            "Parse error in:\n{}\nTree: {:#?}",
            input,
            root.to_sexp()
        );
    }

    #[test]
    fn single_declaration() {
        let input = trim(
            r#"
make_coffee : Beans -> Coffee
            "#,
        );
        let tree = parse_tree(input);
        assert_no_error(&tree, input);
    }

    #[test]
    fn multiple_declarations() {
        let input = trim(
            r#"
make_coffee : Beans -> Coffee
make_tea : Leaves -> Tea
brew_chocolate : Powder -> Chocolate
            "#,
        );
        let tree = parse_tree(input);
        assert_no_error(&tree, input);
    }

    #[test]
    fn genus_forms() {
        let input = trim(
            r#"
f :
g : A -> B
h : (A,B) -> B
i : [A] -> A
j : (A,B,C) -> (A,B)
k : [A] -> [B]
l : [A] -> (A,B)
n : [A] -> ()
o : () -> A
            "#,
        );
        let tree = parse_tree(input);
        assert_no_error(&tree, input);
    }

    #[test]
    fn header_block() {
        let input = trim(
            r#"
% technique v42
! CC BY-SA 3.0 [IGO]; © 2024 ACME, Inc.
& checklist-template

make_coffee :
brew_tea : Leaves -> Tea
            "#,
        );
        let tree = parse_tree(input);
        assert_no_error(&tree, input);
    }
}
