// Hand-written check suite for translation phase
//
// Modelled on `src/parsing/checks/parser.rs`. Source strings parsed through
// the real parser inline matches what the runner sees in production.

use std::path::Path;

use crate::language;
use crate::parsing;
use crate::translation::{translate, Fragment, Operation, Ordinal, SubroutineRef};

#[test]
fn empty_input_yields_empty_program() {
    let source = "";
    let path = Path::new("test.tq");
    let document = parsing::parse(path, source).expect("parse");
    let program = translate(&document).expect("translate");
    assert!(program
        .subroutines
        .is_empty());
}

#[test]
fn single_procedure_registered() {
    let source = r#"
% technique v1

make_coffee :
        "#
    .trim_ascii();
    let path = Path::new("test.tq");
    let document = parsing::parse(path, source).expect("parse");
    let program = translate(&document).expect("translate");

    assert_eq!(
        program
            .subroutines
            .len(),
        1
    );
    assert_eq!(
        program.subroutines[0].name,
        Some(language::Identifier::new("make_coffee"))
    );
}

#[test]
fn multiple_procedures_registered_in_order() {
    let source = r#"
% technique v1

first :

second :

third :
        "#
    .trim_ascii();
    let path = Path::new("test.tq");
    let document = parsing::parse(path, source).expect("parse");
    let program = translate(&document).expect("translate");

    let names: Vec<_> = program
        .subroutines
        .iter()
        .map(|p| {
            p.name
                .as_ref()
                .map(|id| id.value)
        })
        .collect();
    assert_eq!(names, vec![Some("first"), Some("second"), Some("third")]);
}

#[test]
fn procedure_inside_section_registered() {
    let source = r#"
% technique v1

outer :

I. Section One

inner : () -> ()
        "#
    .trim_ascii();
    let path = Path::new("test.tq");
    let document = parsing::parse(path, source).expect("parse");
    let program = translate(&document).expect("translate");

    let names: Vec<_> = program
        .subroutines
        .iter()
        .map(|p| {
            p.name
                .as_ref()
                .map(|id| id.value)
        })
        .collect();
    assert_eq!(names, vec![Some("outer"), Some("inner")]);
}

#[test]
fn procedure_title_extracted() {
    let source = r#"
% technique v1

make_coffee :

# Coffee Time
        "#
    .trim_ascii();
    let path = Path::new("Test.tq");
    let document = parsing::parse(path, source).expect("parse");
    let program = translate(&document).expect("translate");

    assert_eq!(program.subroutines[0].title, Some("Coffee Time"));
}

#[test]
fn procedure_description_extracted() {
    let source = r#"
% technique v1

make_coffee :

# Coffee Time

This is how to make coffee.
        "#
    .trim_ascii();
    let path = Path::new("Test.tq");
    let document = parsing::parse(path, source).expect("parse");
    let program = translate(&document).expect("translate");

    assert_eq!(
        program.subroutines[0]
            .description
            .len(),
        1
    );
}

#[test]
fn procedure_parameters_borrowed() {
    let source = r#"
% technique v1

make_coffee(beans, water) :
        "#
    .trim_ascii();
    let path = Path::new("Test.tq");
    let document = parsing::parse(path, source).expect("parse");
    let program = translate(&document).expect("translate");

    let params = program.subroutines[0]
        .parameters
        .expect("parameters present");
    assert_eq!(params.len(), 2);
    assert_eq!(params[0].value, "beans");
    assert_eq!(params[1].value, "water");
}

#[test]
fn procedure_signature_borrowed() {
    let source = r#"
% technique v1

make_coffee : Beans -> Coffee
        "#
    .trim_ascii();
    let path = Path::new("Test.tq");
    let document = parsing::parse(path, source).expect("parse");
    let program = translate(&document).expect("translate");

    assert!(program.subroutines[0]
        .signature
        .is_some());
}

#[test]
fn anonymous_wrapper_for_top_level_steps() {
    let source = r#"
1.  First step

2.  Second step
        "#
    .trim_ascii();
    let path = Path::new("Test.tq");
    let document = parsing::parse(path, source).expect("parse");
    let program = translate(&document).expect("translate");

    assert_eq!(
        program
            .subroutines
            .len(),
        1
    );
    assert_eq!(program.subroutines[0].name, None);
}

#[test]
fn section_with_empty_body_translated() {
    let source = r#"
% technique v1

outer :

I. First section
        "#
    .trim_ascii();
    let path = Path::new("Test.tq");
    let document = parsing::parse(path, source).expect("parse");
    let program = translate(&document).expect("translate");

    let body = &program.subroutines[0].body;
    let Operation::Sequence(ops) = body else {
        panic!("expected Sequence, got {:?}", body);
    };
    assert_eq!(ops.len(), 1);
    let Operation::Section {
        numeral,
        title,
        body: section_body,
    } = &ops[0]
    else {
        panic!("expected Section, got {:?}", ops[0]);
    };
    assert_eq!(*numeral, "I");
    assert!(title.is_some());
    let Operation::Sequence(inner) = section_body.as_ref() else {
        panic!("expected inner Sequence");
    };
    assert!(inner.is_empty());
}

#[test]
fn section_holding_procedures_yields_empty_body() {
    let source = r#"
% technique v1

outer :

I. Section One

inner : () -> ()
        "#
    .trim_ascii();
    let path = Path::new("Test.tq");
    let document = parsing::parse(path, source).expect("parse");
    let program = translate(&document).expect("translate");

    // The inner procedure was hoisted into the flat list.
    assert_eq!(
        program
            .subroutines
            .len(),
        2
    );

    let Operation::Sequence(ops) = &program.subroutines[0].body else {
        panic!("expected Sequence");
    };
    assert_eq!(ops.len(), 1);
    let Operation::Section {
        body: section_body, ..
    } = &ops[0]
    else {
        panic!("expected Section");
    };
    let Operation::Sequence(inner) = section_body.as_ref() else {
        panic!("expected inner Sequence");
    };
    // Procedures-bodied section carries no executable operations of its own.
    assert!(inner.is_empty());
}

#[test]
fn nested_sections_translate_recursively() {
    let source = r#"
% technique v1

outer :

I. Outer

II. Sibling
        "#
    .trim_ascii();
    let path = Path::new("Test.tq");
    let document = parsing::parse(path, source).expect("parse");
    let program = translate(&document).expect("translate");

    let Operation::Sequence(ops) = &program.subroutines[0].body else {
        panic!("expected Sequence");
    };
    assert_eq!(ops.len(), 2);
    let numerals: Vec<&str> = ops
        .iter()
        .map(|op| match op {
            Operation::Section { numeral, .. } => *numeral,
            _ => panic!("expected Section"),
        })
        .collect();
    assert_eq!(numerals, vec!["I", "II"]);
}

#[test]
fn dependent_step_translated() {
    let source = r#"
% technique v1

make_coffee :

1.  Grind the beans.

2.  Pour the water.
        "#
    .trim_ascii();
    let path = Path::new("Test.tq");
    let document = parsing::parse(path, source).expect("parse");
    let program = translate(&document).expect("translate");

    let Operation::Sequence(ops) = &program.subroutines[0].body else {
        panic!("expected Sequence");
    };
    assert_eq!(ops.len(), 2);
    let ordinals: Vec<&str> = ops
        .iter()
        .map(|op| match op {
            Operation::Step {
                ordinal: Ordinal::Dependent(n),
                ..
            } => *n,
            _ => panic!("expected dependent step, got {:?}", op),
        })
        .collect();
    assert_eq!(ordinals, vec!["1", "2"]);
}

#[test]
fn parallel_step_translated() {
    let source = r#"
% technique v1

make_coffee :

-   Order beans.

-   Boil water.
        "#
    .trim_ascii();
    let path = Path::new("Test.tq");
    let document = parsing::parse(path, source).expect("parse");
    let program = translate(&document).expect("translate");

    let Operation::Sequence(ops) = &program.subroutines[0].body else {
        panic!("expected Sequence");
    };
    assert_eq!(ops.len(), 2);
    for op in ops {
        let Operation::Step {
            ordinal: Ordinal::Parallel,
            ..
        } = op
        else {
            panic!("expected parallel step, got {:?}", op);
        };
    }
}

#[test]
fn substeps_recurse_into_step_body() {
    let source = r#"
% technique v1

make_coffee :

1.  Outer step.

    a.  First substep.

    b.  Second substep.
        "#
    .trim_ascii();
    let path = Path::new("Test.tq");
    let document = parsing::parse(path, source).expect("parse");
    let program = translate(&document).expect("translate");

    let Operation::Sequence(ops) = &program.subroutines[0].body else {
        panic!("expected Sequence");
    };
    assert_eq!(ops.len(), 1);
    let Operation::Step {
        ordinal: Ordinal::Dependent(outer),
        body: outer_body,
        ..
    } = &ops[0]
    else {
        panic!("expected outer dependent step");
    };
    assert_eq!(*outer, "1");

    let Operation::Sequence(inner) = outer_body.as_ref() else {
        panic!("expected inner Sequence");
    };
    let inner_ordinals: Vec<&str> = inner
        .iter()
        .map(|op| match op {
            Operation::Step {
                ordinal: Ordinal::Dependent(n),
                ..
            } => *n,
            _ => panic!("expected dependent substep"),
        })
        .collect();
    assert_eq!(inner_ordinals, vec!["a", "b"]);
}

#[test]
fn top_level_steps_populate_anonymous_wrapper_body() {
    let source = r#"
1.  First step.

2.  Second step.
        "#
    .trim_ascii();
    let path = Path::new("Test.tq");
    let document = parsing::parse(path, source).expect("parse");
    let program = translate(&document).expect("translate");

    assert_eq!(program.subroutines[0].name, None);
    let Operation::Sequence(ops) = &program.subroutines[0].body else {
        panic!("expected Sequence");
    };
    assert_eq!(ops.len(), 2);
}

#[test]
fn attribute_lifts_onto_enclosed_step() {
    let source = r#"
% technique v1

make_coffee :

@chef
    1.  Grind beans.
        "#
    .trim_ascii();
    let path = Path::new("Test.tq");
    let document = parsing::parse(path, source).expect("parse");
    let program = translate(&document).expect("translate");

    let Operation::Sequence(ops) = &program.subroutines[0].body else {
        panic!("expected Sequence");
    };
    // AttributeBlock disappears: only the Step appears at this level.
    assert_eq!(ops.len(), 1);
    let Operation::Step { attributes, .. } = &ops[0] else {
        panic!("expected Step, got {:?}", ops[0]);
    };
    assert_eq!(attributes.len(), 1);
    assert_eq!(attributes[0].len(), 1);
    let language::Attribute::Role(id, _) = &attributes[0][0] else {
        panic!("expected Role attribute");
    };
    assert_eq!(id.value, "chef");
}

#[test]
fn composite_attributes_form_single_frame() {
    let source = r#"
% technique v1

make_coffee :

@chef + ^kitchen
    1.  Grind beans.
        "#
    .trim_ascii();
    let path = Path::new("Test.tq");
    let document = parsing::parse(path, source).expect("parse");
    let program = translate(&document).expect("translate");

    let Operation::Sequence(ops) = &program.subroutines[0].body else {
        panic!("expected Sequence");
    };
    let Operation::Step { attributes, .. } = &ops[0] else {
        panic!("expected Step");
    };
    assert_eq!(attributes.len(), 1, "one frame");
    assert_eq!(attributes[0].len(), 2, "two attributes in the frame");
}

#[test]
fn step_without_attribute_block_has_empty_attributes() {
    let source = r#"
% technique v1

make_coffee :

1.  Plain step.
        "#
    .trim_ascii();
    let path = Path::new("Test.tq");
    let document = parsing::parse(path, source).expect("parse");
    let program = translate(&document).expect("translate");

    let Operation::Sequence(ops) = &program.subroutines[0].body else {
        panic!("expected Sequence");
    };
    let Operation::Step { attributes, .. } = &ops[0] else {
        panic!("expected Step");
    };
    assert!(attributes.is_empty());
}

#[test]
fn substep_inherits_attribute_through_step_body() {
    // The parser nests an AttributeBlock under a Step's body, so the
    // attribute applies only to the enclosed substep, not the outer step.
    let source = r#"
% technique v1

make_coffee :

1.  Outer step.

    @chef
        a.  Substep under chef.
        "#
    .trim_ascii();
    let path = Path::new("Test.tq");
    let document = parsing::parse(path, source).expect("parse");
    let program = translate(&document).expect("translate");

    let Operation::Sequence(ops) = &program.subroutines[0].body else {
        panic!("expected Sequence");
    };
    let Operation::Step {
        attributes: outer_attrs,
        body: outer_body,
        ..
    } = &ops[0]
    else {
        panic!("expected outer Step");
    };
    assert!(outer_attrs.is_empty(), "outer step has no attributes");

    let Operation::Sequence(inner) = outer_body.as_ref() else {
        panic!("expected inner Sequence");
    };
    let Operation::Step {
        attributes: substep_attrs,
        ..
    } = &inner[0]
    else {
        panic!("expected substep");
    };
    assert_eq!(substep_attrs.len(), 1);
    let language::Attribute::Role(id, _) = &substep_attrs[0][0] else {
        panic!("expected Role");
    };
    assert_eq!(id.value, "chef");
}

#[test]
fn response_block_attaches_to_parent_step() {
    let source = r#"
% technique v1

check :

1.  Is everything ready?

        'Yes' | 'No'
        "#
    .trim_ascii();
    let path = Path::new("Test.tq");
    let document = parsing::parse(path, source).expect("parse");
    let program = translate(&document).expect("translate");

    let Operation::Sequence(ops) = &program.subroutines[0].body else {
        panic!("expected Sequence");
    };
    let Operation::Step { expects, .. } = &ops[0] else {
        panic!("expected Step");
    };
    let responses = expects.expect("expects present");
    assert_eq!(responses.len(), 2);
    assert_eq!(responses[0].value, "Yes");
    assert_eq!(responses[1].value, "No");
}

#[test]
fn step_without_response_block_has_none_expects() {
    let source = r#"
% technique v1

check :

1.  Plain step.
        "#
    .trim_ascii();
    let path = Path::new("Test.tq");
    let document = parsing::parse(path, source).expect("parse");
    let program = translate(&document).expect("translate");

    let Operation::Sequence(ops) = &program.subroutines[0].body else {
        panic!("expected Sequence");
    };
    let Operation::Step { expects, .. } = &ops[0] else {
        panic!("expected Step");
    };
    assert!(expects.is_none());
}

#[test]
fn expression_variable_translates() {
    let source = r#"
% technique v1

run :

{
    x
}
        "#
    .trim_ascii();
    let path = Path::new("Test.tq");
    let document = parsing::parse(path, source).expect("parse");
    let program = translate(&document).expect("translate");

    let Operation::Sequence(ops) = &program.subroutines[0].body else {
        panic!("expected Sequence");
    };
    let Operation::Variable(id) = &ops[0] else {
        panic!("expected Variable, got {:?}", ops[0]);
    };
    assert_eq!(id.value, "x");
}

#[test]
fn expression_number_translates() {
    let source = r#"
% technique v1

run :

{
    42
}
        "#
    .trim_ascii();
    let path = Path::new("Test.tq");
    let document = parsing::parse(path, source).expect("parse");
    let program = translate(&document).expect("translate");

    let Operation::Sequence(ops) = &program.subroutines[0].body else {
        panic!("expected Sequence");
    };
    let Operation::Number(numeric) = &ops[0] else {
        panic!("expected Number, got {:?}", ops[0]);
    };
    let language::Numeric::Integral(n) = numeric else {
        panic!("expected Integral");
    };
    assert_eq!(*n, 42);
}

#[test]
fn expression_string_translates() {
    let source = r#"
% technique v1

run :

{
    journal("hello")
}
        "#
    .trim_ascii();
    let path = Path::new("Test.tq");
    let document = parsing::parse(path, source).expect("parse");
    let program = translate(&document).expect("translate");

    let Operation::Sequence(ops) = &program.subroutines[0].body else {
        panic!("expected Sequence");
    };
    let Operation::Execution { arguments, .. } = &ops[0] else {
        panic!("expected Execution");
    };
    let Operation::String(fragments) = &arguments[0] else {
        panic!("expected String");
    };
    assert_eq!(fragments.len(), 1);
    let Fragment::Text(text) = &fragments[0] else {
        panic!("expected Text fragment");
    };
    assert_eq!(*text, "hello");
}

#[test]
fn expression_execution_translates() {
    let source = r#"
% technique v1

run :

{
    sum(1, 2)
}
        "#
    .trim_ascii();
    let path = Path::new("Test.tq");
    let document = parsing::parse(path, source).expect("parse");
    let program = translate(&document).expect("translate");

    let Operation::Sequence(ops) = &program.subroutines[0].body else {
        panic!("expected Sequence");
    };
    let Operation::Execution { target, arguments } = &ops[0] else {
        panic!("expected Execution, got {:?}", ops[0]);
    };
    assert_eq!(target.value, "sum");
    assert_eq!(arguments.len(), 2);
}

#[test]
fn expression_application_translates_as_unresolved_invoke() {
    let source = r#"
% technique v1

run :

{
    <other>(x)
}

other : X -> Y
        "#
    .trim_ascii();
    let path = Path::new("Test.tq");
    let document = parsing::parse(path, source).expect("parse");
    let program = translate(&document).expect("translate");

    let Operation::Sequence(ops) = &program.subroutines[0].body else {
        panic!("expected Sequence");
    };
    let Operation::Invoke(invoke) = &ops[0] else {
        panic!("expected Invoke, got {:?}", ops[0]);
    };
    let SubroutineRef::Unresolved(id) = &invoke.target else {
        panic!("expected Unresolved (resolution is a later pass)");
    };
    assert_eq!(id.value, "other");
    assert_eq!(
        invoke
            .arguments
            .len(),
        1
    );
}

#[test]
fn expression_binding_translates() {
    let source = r#"
% technique v1

run :

{
    42 ~ answer
}
        "#
    .trim_ascii();
    let path = Path::new("Test.tq");
    let document = parsing::parse(path, source).expect("parse");
    let program = translate(&document).expect("translate");

    let Operation::Sequence(ops) = &program.subroutines[0].body else {
        panic!("expected Sequence");
    };
    let Operation::Bind { names, value } = &ops[0] else {
        panic!("expected Bind, got {:?}", ops[0]);
    };
    assert_eq!(names.len(), 1);
    assert_eq!(names[0].value, "answer");
    let Operation::Number(_) = value.as_ref() else {
        panic!("expected Number value");
    };
}

#[test]
fn expression_tablet_translates() {
    let source = r#"
% technique v1

run :

{
    [
        "speed" = 3.0 × 10⁸ m/s
        "weight" = 84.0 kg
    ]
}
        "#
    .trim_ascii();
    let path = Path::new("Test.tq");
    let document = parsing::parse(path, source).expect("parse");
    let program = translate(&document).expect("translate");

    let Operation::Sequence(ops) = &program.subroutines[0].body else {
        panic!("expected Sequence");
    };
    let Operation::Tablet(entries) = &ops[0] else {
        panic!("expected Tablet, got {:?}", ops[0]);
    };
    assert_eq!(entries.len(), 2);
    assert_eq!(entries[0].label, "speed");
    assert_eq!(entries[1].label, "weight");
}

#[test]
fn foreach_codeblock_becomes_loop_with_subscopes_as_body() {
    let source = r#"
% technique v1

run :

@worker
    { foreach node in seq(1, 6) }
        1.  Check Availability
        2.  Confirm.
        "#
    .trim_ascii();
    let path = Path::new("Test.tq");
    let document = parsing::parse(path, source).expect("parse");
    let program = translate(&document).expect("translate");

    let Operation::Sequence(ops) = &program.subroutines[0].body else {
        panic!("expected Sequence");
    };
    let Operation::Loop { names, over, body } = &ops[0] else {
        panic!("expected Loop, got {:?}", ops[0]);
    };
    assert_eq!(names.len(), 1);
    assert_eq!(names[0].value, "node");
    assert!(over.is_some(), "foreach has a source");

    let Operation::Sequence(inner) = body.as_ref() else {
        panic!("expected inner Sequence");
    };
    assert_eq!(inner.len(), 2);
}

#[test]
fn foreach_with_tuple_names_borrows_all() {
    let source = r#"
% technique v1

run :

@worker
    { foreach (design, component) in zip(designs, components) }
        a.  process
        "#
    .trim_ascii();
    let path = Path::new("Test.tq");
    let document = parsing::parse(path, source).expect("parse");
    let program = translate(&document).expect("translate");

    let Operation::Sequence(ops) = &program.subroutines[0].body else {
        panic!("expected Sequence");
    };
    let Operation::Loop { names, .. } = &ops[0] else {
        panic!("expected Loop");
    };
    assert_eq!(names.len(), 2);
    assert_eq!(names[0].value, "design");
    assert_eq!(names[1].value, "component");
}
