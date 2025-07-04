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
    fn simple_declaration() {
        let input = trim(
            r#"
make_coffee : Beans -> Coffee
            "#,
        );
        let tree = parse_tree(input);
        assert_no_error(&tree, input);
    }

    #[test]
    fn full_declaration() {
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
    fn declaration_at_left_margin() {
        let input = trim(
            r#"
    make_coffee : Beans -> Coffee
            "#,
        );
        let tree = parse_tree(input);
        assert_no_error(&tree, input);
    }

    #[test]
    fn declaration_with_leading_whitespace() {
        let input = trim(
            r#"
        make_coffee : Beans -> Coffee
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
    fn magic_only() {
        let input = trim(
            r#"
% technique v1
            "#,
        );
        let tree = parse_tree(input);
        assert_no_error(&tree, input);
    }

    #[test]
    fn header_block() {
        let input = trim(
            r#"
% technique v1
! CC BY-SA 3.0 [IGO]; © 2024 ACME, Inc.
& checklist-template
            "#,
        );
        let tree = parse_tree(input);
        assert_no_error(&tree, input);
    }

    #[test]
    fn header_and_delcarations() {
        let input = trim(
            r#"
% technique v1
! CC BY-SA 3.0 [IGO]; © 2024 ACME, Inc.
& checklist-template

make_coffee :
brew_tea : Leaves -> Tea
            "#,
        );
        let tree = parse_tree(input);
        assert_no_error(&tree, input);
    }

    #[test]
    fn procedure_with_title_and_description() {
        let input = trim(
            r#"
    my_proc:

    # My Procedure Title

    This is the first line of the description.
    This is the second line.
                "#,
        );
        let tree = parse_tree(input);
        assert_no_error(&tree, input);
    }

    #[test]
    fn procedure_with_dependent_steps() {
        let input = trim(
            r#"
    my_proc:
        1. First step.
        2. Second step.
                "#,
        );
        let tree = parse_tree(input);
        assert_no_error(&tree, input);
    }

    #[test]
    fn description_lines_with_periods_are_not_steps() {
        let input = trim(
            r#"
    This is a line ending with one.
    And another line but not a step.

    one. is not a step
    "#,
        );
        let tree = parse_tree(input);
        assert_no_error(&tree, input);
    }

    #[test]
    fn numeric_and_alphabetic_steps_are_recognized() {
        let input = trim(
            r#"
    1. This is a numeric step
    a. This is an alphabetic step
    i. This is also an alphabetic step
    "#,
        );
        let tree = parse_tree(input);
        assert_no_error(&tree, input);
    }

    #[test]
    fn mixed_description_and_steps() {
        let input = trim(
            r#"
    This is a description.

    a. First step
    b. Second step

    This is another paragraph.

    1. Numeric step
    2. Another numeric step
    "#,
        );
        let tree = parse_tree(input);
        assert_no_error(&tree, input);
    }

    #[test]
    fn step_like_text_in_description_is_not_step() {
        let input = trim(
            r#"
    This is not a step a. is just a word here.
    But this is a step
    b. Now this is a step
    "#,
        );
        let tree = parse_tree(input);
        assert_no_error(&tree, input);
    }

    #[test]
    fn procedure_with_parallel_steps() {
        let input = trim(
            r#"
    my_proc:
        - A parallel step.
        * Another parallel step.
                "#,
        );
        let tree = parse_tree(input);
        assert_no_error(&tree, input);
    }

    #[test]
    fn network_probe_procedure() {
        let input = trim(
            r#"
    connectivity_check(e,s) : (LocalEnvironment, TargetService) -> NetworkHealth

    # Network Connectivity Check

    We check the health of the network path between a user's machine running
    in a branch office and a service running in a datacenter by establishing
    functionality at each layer between our device and the remote server.

        1.  Local network connectivity
        2.  Reachability of site border
        3.  Check internet connectivity
        4.  Reachability of away border
        5.  Traversal of away-side load balancer
        6.  Traversal of away-side local network
        7.  Check response from remote service
                "#,
        );
        let tree = parse_tree(input);
        assert_no_error(&tree, input);
    }
}
