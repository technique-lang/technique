use crate::language::{Attribute, Identifier, Span};
use crate::runner::path::{PathSegment, QualifiedPath};

#[test]
fn empty_stack_renders_root() {
    let stack = QualifiedPath::new();
    assert_eq!(stack.render(), "/");
}

#[test]
fn single_section_renders_numeral() {
    let mut stack = QualifiedPath::new();
    stack.push(PathSegment::Section("I"));
    assert_eq!(stack.render(), "/I");
}

#[test]
fn section_then_step_joins_with_slash() {
    let mut stack = QualifiedPath::new();
    stack.push(PathSegment::Section("I"));
    stack.push(PathSegment::DependentStep("2"));
    assert_eq!(stack.render(), "/I/2");
}

#[test]
fn dependent_substep_chain() {
    let mut stack = QualifiedPath::new();
    stack.push(PathSegment::DependentStep("2"));
    stack.push(PathSegment::DependentStep("a"));
    assert_eq!(stack.render(), "/2/a");
}

#[test]
fn parallel_step_uses_dash_prefix() {
    let mut stack = QualifiedPath::new();
    stack.push(PathSegment::ParallelStep(3));
    assert_eq!(stack.render(), "/-3");
}

#[test]
fn attribute_frame_composes_role_and_place() {
    let frame = vec![
        Attribute::Role(Identifier::new("chef"), Span::default()),
        Attribute::Place(Identifier::new("kitchen"), Span::default()),
    ];
    let mut stack = QualifiedPath::new();
    stack.push(PathSegment::Attributes(&frame));
    stack.push(PathSegment::DependentStep("a"));
    assert_eq!(stack.render(), "/@chef^kitchen/a");
}

#[test]
fn nested_attribute_frames_each_contribute_a_segment() {
    let outer = vec![Attribute::Role(Identifier::new("team"), Span::default())];
    let inner = vec![
        Attribute::Role(Identifier::new("chef"), Span::default()),
        Attribute::Place(Identifier::new("kitchen"), Span::default()),
    ];
    let mut stack = QualifiedPath::new();
    stack.push(PathSegment::DependentStep("2"));
    stack.push(PathSegment::Attributes(&outer));
    stack.push(PathSegment::Attributes(&inner));
    stack.push(PathSegment::DependentStep("a"));
    assert_eq!(stack.render(), "/2/@team/@chef^kitchen/a");
}

#[test]
fn reset_role() {
    // `@*` alone suppresses the whole attribute frame's contribution.
    let frame = vec![Attribute::Role(Identifier::new("*"), Span::default())];
    let mut stack = QualifiedPath::new();
    stack.push(PathSegment::DependentStep("1"));
    stack.push(PathSegment::Attributes(&frame));
    stack.push(PathSegment::DependentStep("a"));
    assert_eq!(stack.render(), "/1/a");

    // A sibling Place attribute in the same frame still renders.
    let frame = vec![
        Attribute::Role(Identifier::new("*"), Span::default()),
        Attribute::Place(Identifier::new("kitchen"), Span::default()),
    ];
    let mut stack = QualifiedPath::new();
    stack.push(PathSegment::DependentStep("1"));
    stack.push(PathSegment::Attributes(&frame));
    stack.push(PathSegment::DependentStep("a"));
    assert_eq!(stack.render(), "/1/^kitchen/a");
}

#[test]
fn procedure_segment() {
    // Procedure alone renders as `/name:`
    let mut stack = QualifiedPath::new();
    stack.push(PathSegment::Procedure("local_network"));
    assert_eq!(stack.render(), "/local_network:");

    // Procedure with a step: `/name:N`.
    let mut stack = QualifiedPath::new();
    stack.push(PathSegment::Procedure("make_coffee"));
    stack.push(PathSegment::DependentStep("2"));
    assert_eq!(stack.render(), "/make_coffee:2");

    // Nested procedure: the inner frame replaces the outer prefix entirely.
    let mut stack = QualifiedPath::new();
    stack.push(PathSegment::Procedure("outer"));
    stack.push(PathSegment::DependentStep("1"));
    stack.push(PathSegment::Procedure("inner"));
    stack.push(PathSegment::DependentStep("2"));
    assert_eq!(stack.render(), "/inner:2");
}

#[test]
fn full_qualified_example_from_objective() {
    // /2/@barista/a/-1 — dependent step 2, role @barista, substep a, first parallel sub-substep
    let frame = vec![Attribute::Role(Identifier::new("barista"), Span::default())];
    let mut stack = QualifiedPath::new();
    stack.push(PathSegment::DependentStep("2"));
    stack.push(PathSegment::Attributes(&frame));
    stack.push(PathSegment::DependentStep("a"));
    stack.push(PathSegment::ParallelStep(1));
    assert_eq!(stack.render(), "/2/@barista/a/-1");
}
