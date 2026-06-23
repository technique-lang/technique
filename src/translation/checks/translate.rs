// Hand-written check suite for translation phase
//
// Modelled on `src/parsing/checks/parser.rs`. Source strings parsed through
// the real parser inline matches what the runner sees in production.

use std::path::Path;

use crate::language;
use crate::parsing;
use crate::program::{ExecutableRef, Fragment, Operation, Ordinal, SubroutineId, SubroutineRef};
use crate::resolution::resolve;
use crate::translation::translate;

#[test]
fn empty_input_yields_empty_program() {
    let source = "";
    let path = Path::new("Test.tq");
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
    let path = Path::new("Test.tq");
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
    let path = Path::new("Test.tq");
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

    let signature = program.subroutines[0]
        .signature
        .expect("signature present");
    let language::Genus::Single(input) = &signature.requires else {
        panic!("expected single Forma input");
    };
    assert_eq!(input.value, "Beans");
    let language::Genus::Single(output) = &signature.provides else {
        panic!("expected single Forma output");
    };
    assert_eq!(output.value, "Coffee");
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
        ..
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
fn section_holding_procedures_invokes_first() {
    let source = r#"
% technique v1

outer :

I. Section One

inner : () -> ()
        "#
    .trim_ascii();
    let path = Path::new("Test.tq");
    let document = parsing::parse(path, source).expect("parse");
    let mut program = translate(&document).expect("translate");
    resolve(&mut program).expect("resolve");

    // The inner procedure was hoisted into the flat list, after the outer.
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
    // A procedures-bodied section descends into its first procedure.
    assert_eq!(inner.len(), 1);
    let Operation::Invoke(invocable) = &inner[0] else {
        panic!("expected Invoke, got {:?}", inner[0]);
    };
    assert_eq!(invocable.target, SubroutineRef::Resolved(SubroutineId(1)));
}

#[test]
fn sibling_sections_translate_in_order() {
    let source = r#"
% technique v1

outer :

I. First

II. Second
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
        .filter_map(|op| match op {
            Operation::Step {
                ordinal: Ordinal::Dependent(n),
                ..
            } => Some(*n),
            _ => None,
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
    // inner[0] is the outer step's own prose; the substep follows.
    let Operation::Step {
        attributes: substep_attrs,
        ..
    } = &inner[1]
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
fn expression_string_text_fragment_translates() {
    // A plain string literal becomes a single Text fragment.
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
    let Operation::Execute(executable) = &ops[0] else {
        panic!("expected Execute");
    };
    let Operation::String(fragments) = &executable.arguments[0] else {
        panic!("expected String");
    };
    assert_eq!(fragments.len(), 1);
    let Fragment::Text(text) = &fragments[0] else {
        panic!("expected Text fragment");
    };
    assert_eq!(*text, "hello");
}

#[test]
fn expression_string_with_interpolation_translates() {
    // A string with `{ x }` interpolations produces alternating Text and
    // Interpolation fragments. The interpolated expression is itself an
    // Operation, recursively translated.
    let source = r#"
% technique v1

run :

{
    journal("hello { name }!")
}
        "#
    .trim_ascii();
    let path = Path::new("Test.tq");
    let document = parsing::parse(path, source).expect("parse");
    let program = translate(&document).expect("translate");

    let Operation::Sequence(ops) = &program.subroutines[0].body else {
        panic!("expected Sequence");
    };
    let Operation::Execute(executable) = &ops[0] else {
        panic!("expected Execute");
    };
    let Operation::String(fragments) = &executable.arguments[0] else {
        panic!("expected String");
    };
    assert_eq!(fragments.len(), 3);
    let Fragment::Text(prefix) = &fragments[0] else {
        panic!("expected Text fragment");
    };
    assert_eq!(*prefix, "hello ");
    let Fragment::Interpolation(inner) = &fragments[1] else {
        panic!("expected Interpolation fragment, got {:?}", fragments[1]);
    };
    let Operation::Variable(id) = inner else {
        panic!("expected Variable inside interpolation");
    };
    assert_eq!(id.value, "name");
    let Fragment::Text(suffix) = &fragments[2] else {
        panic!("expected trailing Text fragment");
    };
    assert_eq!(*suffix, "!");
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
    let Operation::Execute(executable) = &ops[0] else {
        panic!("expected Execute, got {:?}", ops[0]);
    };
    let ExecutableRef::Unresolved(target) = &executable.target else {
        panic!("expected Unresolved, got {:?}", executable.target);
    };
    assert_eq!(target.value, "sum");
    assert_eq!(
        executable
            .arguments
            .len(),
        2
    );
}

#[test]
fn expression_application_translates_as_invoke() {
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
    let Operation::Invoke(invocable) = &ops[0] else {
        panic!("expected Invoke, got {:?}", ops[0]);
    };
    assert_eq!(
        invocable
            .arguments
            .len(),
        1
    );
}

#[test]
fn hole_argument_satisfies_arity_and_translates() {
    // `?` fills the single argument slot the signature requires, so the call
    // type-checks, and the argument lowers to Operation::Hole.
    let source = r#"
% technique v1

run :

{
    <other>(?)
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
    let Operation::Invoke(invocable) = &ops[0] else {
        panic!("expected Invoke, got {:?}", ops[0]);
    };
    let [Operation::Hole] = invocable
        .arguments
        .as_slice()
    else {
        panic!(
            "expected a single Hole argument, got {:?}",
            invocable.arguments
        );
    };
}

#[test]
fn bare_call_is_exempt_from_arity() {
    // A bare `<other>` (no argument list) defers all arguments, so it passes
    // against an arity-1 procedure and is marked elided with no arguments.
    let source = r#"
% technique v1

run :

{
    <other>
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
    let Operation::Invoke(invocable) = &ops[0] else {
        panic!("expected Invoke, got {:?}", ops[0]);
    };
    assert!(invocable.elided, "a bare call is elided");
    assert!(invocable
        .arguments
        .is_empty());
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
    let Operation::Bind { names, value, .. } = &ops[0] else {
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
fn expression_list_translates() {
    let source = r#"
% technique v1

run :

{
    [ 1, 4, 9 ]
}
        "#
    .trim_ascii();
    let path = Path::new("Test.tq");
    let document = parsing::parse(path, source).expect("parse");
    let program = translate(&document).expect("translate");

    let Operation::Sequence(ops) = &program.subroutines[0].body else {
        panic!("expected Sequence");
    };
    let Operation::List(items) = &ops[0] else {
        panic!("expected List, got {:?}", ops[0]);
    };
    assert_eq!(items.len(), 3);
    for item in items {
        let Operation::Number(_) = item else {
            panic!("expected Number element, got {:?}", item);
        };
    }
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
    let Operation::Loop {
        names, over, body, ..
    } = &ops[0]
    else {
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
fn top_level_foreach_codeblock_nests_subscopes() {
    // A foreach code block at procedure-body level (no enclosing attribute)
    // owns the steps below it as its loop body, the same as the scoped case.
    let source = r#"
% technique v1

run :

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
    let Operation::Loop { names, body, .. } = &ops[0] else {
        panic!("expected Loop, got {:?}", ops[0]);
    };
    assert_eq!(names[0].value, "node");
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

#[test]
fn step_text_binding_hoists_into_body() {
    // `text ~ name` in a step's description hoists out as a Bind operation
    // in the step's body. The inner Text has no executable form, so the
    // bound value is a placeholder empty Sequence.
    let source = r#"
% technique v1

run :

1.  What is the situation now? ~ situation
        "#
    .trim_ascii();
    let path = Path::new("Test.tq");
    let document = parsing::parse(path, source).expect("parse");
    let program = translate(&document).expect("translate");

    let Operation::Sequence(ops) = &program.subroutines[0].body else {
        panic!("expected Sequence");
    };
    let Operation::Step { body, .. } = &ops[0] else {
        panic!("expected Step");
    };
    let Operation::Sequence(body_ops) = body.as_ref() else {
        panic!("expected Sequence");
    };
    assert_eq!(body_ops.len(), 1);
    let Operation::Bind { names, value, .. } = &body_ops[0] else {
        panic!("expected Bind, got {:?}", body_ops[0]);
    };
    assert_eq!(names.len(), 1);
    assert_eq!(names[0].value, "situation");
    let Operation::Sequence(inner) = value.as_ref() else {
        panic!("expected empty Sequence as binding value");
    };
    assert!(inner.is_empty());
}

#[test]
fn step_application_binding_hoists_with_invoke_value() {
    // `<call> ~ name` hoists as Bind whose value is the Invoke.
    let source = r#"
% technique v1

run :

1.  Get the result <helper> ~ outcome

helper : () -> Result
        "#
    .trim_ascii();
    let path = Path::new("Test.tq");
    let document = parsing::parse(path, source).expect("parse");
    let program = translate(&document).expect("translate");

    let Operation::Sequence(ops) = &program.subroutines[0].body else {
        panic!("expected Sequence");
    };
    let Operation::Step { body, .. } = &ops[0] else {
        panic!("expected Step");
    };
    let Operation::Sequence(body_ops) = body.as_ref() else {
        panic!("expected Sequence");
    };
    assert_eq!(body_ops.len(), 2);
    let Operation::Bind { names, value, .. } = &body_ops[1] else {
        panic!("expected Bind");
    };
    assert_eq!(names[0].value, "outcome");
    let Operation::Invoke(_) = value.as_ref() else {
        panic!("expected Invoke as binding value");
    };
}

#[test]
fn step_inline_application_hoists_as_invoke() {
    // A bare `<call>` (no binding) inside a step's description hoists as
    // an Invoke operation in the step body.
    let source = r#"
% technique v1

run :

1.  Do <helper> first.

helper : () -> Result
        "#
    .trim_ascii();
    let path = Path::new("Test.tq");
    let document = parsing::parse(path, source).expect("parse");
    let program = translate(&document).expect("translate");

    let Operation::Sequence(ops) = &program.subroutines[0].body else {
        panic!("expected Sequence");
    };
    let Operation::Step { body, .. } = &ops[0] else {
        panic!("expected Step");
    };
    let Operation::Sequence(body_ops) = body.as_ref() else {
        panic!("expected Sequence");
    };
    // [Prose("Do"), Invoke, Prose("first.")] — the invoke sits between prose.
    assert_eq!(body_ops.len(), 3);
    let Operation::Invoke(_) = &body_ops[1] else {
        panic!("expected Invoke, got {:?}", body_ops[1]);
    };
}

#[test]
fn step_plain_text_becomes_prose() {
    // Plain text in a step description is kept as an inert Prose operation, so
    // the step's last value reflects source order (here, no value: Done unit).
    let source = r#"
% technique v1

run :

1.  Just a plain step with no executable bits.
        "#
    .trim_ascii();
    let path = Path::new("Test.tq");
    let document = parsing::parse(path, source).expect("parse");
    let program = translate(&document).expect("translate");

    let Operation::Sequence(ops) = &program.subroutines[0].body else {
        panic!("expected Sequence");
    };
    let Operation::Step { body, .. } = &ops[0] else {
        panic!("expected Step");
    };
    let Operation::Sequence(body_ops) = body.as_ref() else {
        panic!("expected Sequence");
    };
    assert_eq!(body_ops.len(), 1);
    let Operation::Prose(text) = &body_ops[0] else {
        panic!("expected Prose, got {:?}", body_ops[0]);
    };
    assert_eq!(*text, "Just a plain step with no executable bits.");
}

#[test]
fn step_inline_codeblock_with_side_effect_hoists() {
    // `{ exec_call(args) }` written inline in a step description hoists
    // out as an Execute operation in the step body.
    let source = r#"
% technique v1

run :

1.  Log the start { journal("started") }
        "#
    .trim_ascii();
    let path = Path::new("Test.tq");
    let document = parsing::parse(path, source).expect("parse");
    let program = translate(&document).expect("translate");

    let Operation::Sequence(ops) = &program.subroutines[0].body else {
        panic!("expected Sequence");
    };
    let Operation::Step { body, .. } = &ops[0] else {
        panic!("expected Step");
    };
    let Operation::Sequence(body_ops) = body.as_ref() else {
        panic!("expected Sequence");
    };
    assert_eq!(body_ops.len(), 2);
    let Operation::Execute(_) = &body_ops[1] else {
        panic!("expected Execute, got {:?}", body_ops[1]);
    };
}

#[test]
fn step_inline_variable_hoists_as_variable_op() {
    // `{ x }` requires runtime evaluation: the runner must wait for `x` to
    // be bound and substitute its value into the rendered description. So
    // the CodeInline hoists as Operation::Variable in the step body.
    let source = r#"
% technique v1

run :

1.  Display { x }
        "#
    .trim_ascii();
    let path = Path::new("Test.tq");
    let document = parsing::parse(path, source).expect("parse");
    let program = translate(&document).expect("translate");

    let Operation::Sequence(ops) = &program.subroutines[0].body else {
        panic!("expected Sequence");
    };
    let Operation::Step { body, .. } = &ops[0] else {
        panic!("expected Step");
    };
    let Operation::Sequence(body_ops) = body.as_ref() else {
        panic!("expected Sequence");
    };
    assert_eq!(body_ops.len(), 2);
    let Operation::Variable(id) = &body_ops[1] else {
        panic!("expected Variable, got {:?}", body_ops[1]);
    };
    assert_eq!(id.value, "x");
}

#[test]
fn procedure_description_executables_hoist_as_prologue() {
    // An executable Descriptive in the procedure's description forms an
    // anonymous step-0 Prologue scope ahead of the explicit Steps.
    let source = r#"
% technique v1

run :

Initialise <init>

1.  Continue from there.

init : () -> ()
        "#
    .trim_ascii();
    let path = Path::new("Test.tq");
    let document = parsing::parse(path, source).expect("parse");
    let program = translate(&document).expect("translate");

    let Operation::Sequence(ops) = &program.subroutines[0].body else {
        panic!("expected Sequence");
    };
    // Prologue from the description, then the explicit Step.
    assert_eq!(ops.len(), 2);
    let Operation::Prologue(prologue) = &ops[0] else {
        panic!("expected Prologue, got {:?}", ops[0]);
    };
    let invokes = prologue
        .iter()
        .filter(|op| {
            if let Operation::Invoke(_) = op {
                true
            } else {
                false
            }
        })
        .count();
    assert_eq!(invokes, 1);
    let Operation::Step { .. } = &ops[1] else {
        panic!("expected Step, got {:?}", ops[1]);
    };
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
    let Operation::Step { responses, .. } = &ops[0] else {
        panic!("expected Step");
    };
    assert_eq!(responses.len(), 2);
    assert_eq!(responses[0].value, "Yes");
    assert_eq!(responses[1].value, "No");
}

#[test]
fn step_without_response_block_has_empty_responses() {
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
    let Operation::Step { responses, .. } = &ops[0] else {
        panic!("expected Step");
    };
    assert!(responses.is_empty());
}

#[test]
fn response_under_attribute_block_attaches_to_enclosing_step() {
    // AttributeBlock vanishes; its ResponseBlock subscope attaches to the
    // enclosing Step's responses just as if it were a peer.
    let source = r#"
% technique v1

check :

1.  Outer step.

    @chef
        'Yes' | 'No'
        "#
    .trim_ascii();
    let path = Path::new("Test.tq");
    let document = parsing::parse(path, source).expect("parse");
    let program = translate(&document).expect("translate");

    let Operation::Sequence(ops) = &program.subroutines[0].body else {
        panic!("expected Sequence");
    };
    let Operation::Step { responses, .. } = &ops[0] else {
        panic!("expected Step");
    };
    assert_eq!(responses.len(), 2);
    assert_eq!(responses[0].value, "Yes");
    assert_eq!(responses[1].value, "No");
}

#[test]
fn response_under_foreach_attaches_to_loop() {
    // 'Reachable' is a per-iteration response of the foreach Loop, not a
    // response of the enclosing Step. NetworkProbe.tq pattern.
    let source = r#"
% technique v1

run :

1.  Probe global DNS responding.
        { foreach nameserver in globals }
                'Reachable' | 'Unreachable'
        "#
    .trim_ascii();
    let path = Path::new("Test.tq");
    let document = parsing::parse(path, source).expect("parse");
    let program = translate(&document).expect("translate");

    let Operation::Sequence(ops) = &program.subroutines[0].body else {
        panic!("expected Sequence");
    };
    let Operation::Step {
        responses: step_responses,
        body,
        ..
    } = &ops[0]
    else {
        panic!("expected Step");
    };
    assert!(
        step_responses.is_empty(),
        "responses do not lift onto the enclosing Step"
    );

    let Operation::Sequence(step_body) = body.as_ref() else {
        panic!("expected Sequence");
    };
    let Operation::Loop {
        responses: loop_responses,
        ..
    } = &step_body[1]
    else {
        panic!("expected Loop");
    };
    assert_eq!(loop_responses.len(), 2);
    assert_eq!(loop_responses[0].value, "Reachable");
    assert_eq!(loop_responses[1].value, "Unreachable");
}

#[test]
fn expression_multiline_translates() {
    // exec(```bash ... ```) carries a Multiline through as the language tag
    // and the lines of content. NetworkProbe.tq pattern.
    let source = r#"
% technique v1

run :

{
    exec(```bash
        ip addr
    ```)
}
        "#
    .trim_ascii();
    let path = Path::new("Test.tq");
    let document = parsing::parse(path, source).expect("parse");
    let program = translate(&document).expect("translate");

    let Operation::Sequence(ops) = &program.subroutines[0].body else {
        panic!("expected Sequence");
    };
    let Operation::Execute(executable) = &ops[0] else {
        panic!("expected Execute");
    };
    let Operation::Multiline(lang, lines) = &executable.arguments[0] else {
        panic!("expected Multiline, got {:?}", executable.arguments[0]);
    };
    assert_eq!(*lang, Some("bash"));
    assert_eq!(lines.len(), 1);
    assert_eq!(lines[0], "ip addr");
}

#[test]
fn expression_repeat_translates() {
    // `repeat <thing>` loops over `<thing>`: the inline expression is the loop
    // body. Here `repeat 5` translates to a Loop with names=[], over=None, and
    // a body holding the repeated expression.
    let source = r#"
% technique v1

run :

{
    repeat 5
}
        "#
    .trim_ascii();
    let path = Path::new("Test.tq");
    let document = parsing::parse(path, source).expect("parse");
    let program = translate(&document).expect("translate");

    let Operation::Sequence(ops) = &program.subroutines[0].body else {
        panic!("expected Sequence");
    };
    let Operation::Loop {
        names,
        over,
        body,
        responses,
    } = &ops[0]
    else {
        panic!("expected Loop, got {:?}", ops[0]);
    };
    assert!(names.is_empty());
    assert!(over.is_none(), "repeat has no `over` source");
    assert!(responses.is_empty());
    let Operation::Sequence(inner) = body.as_ref() else {
        panic!("expected Sequence body");
    };
    let [Operation::Number(_)] = inner.as_slice() else {
        panic!(
            "expected the repeated expression in the body, got {:?}",
            inner
        );
    };
}

#[test]
fn expression_foreach_bare_has_empty_body() {
    // A `foreach` Expression on its own (not as the head of a CodeBlock with
    // subscopes supplying its body) translates to a Loop with an empty
    // Sequence body. The CodeBlock-with-subscopes path provides the body
    // separately; see `foreach_codeblock_becomes_loop_with_subscopes_as_body`.
    let source = r#"
% technique v1

run :

{
    foreach x in xs
}
        "#
    .trim_ascii();
    let path = Path::new("Test.tq");
    let document = parsing::parse(path, source).expect("parse");
    let program = translate(&document).expect("translate");

    let Operation::Sequence(ops) = &program.subroutines[0].body else {
        panic!("expected Sequence");
    };
    let Operation::Loop {
        names, over, body, ..
    } = &ops[0]
    else {
        panic!("expected Loop");
    };
    assert_eq!(names.len(), 1);
    assert_eq!(names[0].value, "x");
    let Some(over) = over else {
        panic!("foreach has an `over` source");
    };
    let Operation::Variable(id) = over.as_ref() else {
        panic!("expected Variable as `over`");
    };
    assert_eq!(id.value, "xs");
    let Operation::Sequence(inner) = body.as_ref() else {
        panic!("expected empty Sequence body");
    };
    assert!(inner.is_empty());
}

#[test]
fn codeblock_preserves_multi_expressions() {
    // A code block can contain multiple expressions.
    let source = r#"
% technique v1

delete_rds_instance :

1.  Disable termination protection
    {
        click("Modify")
        navigate("bottom")
        deselect("Enable deletion protection")
        click("Continue")
        select("Apply Immediately")
        click("Modify DB instance")
    }
        "#
    .trim_ascii();
    let path = Path::new("Test.tq");
    let document = parsing::parse(path, source).expect("parse");
    let program = translate(&document).expect("translate");

    let Operation::Sequence(ops) = &program.subroutines[0].body else {
        panic!("expected Sequence");
    };
    let Operation::Step { body, .. } = &ops[0] else {
        panic!("expected Step");
    };
    let Operation::Sequence(step_body) = body.as_ref() else {
        panic!("expected Step body Sequence");
    };
    // A plain code block is inline: its expressions hoist directly into the
    // Step body, one Execute per call.
    let names: Vec<&str> = step_body
        .iter()
        .filter_map(|op| match op {
            Operation::Execute(executable) => {
                let ExecutableRef::Unresolved(target) = &executable.target else {
                    panic!("expected Unresolved, got {:?}", executable.target);
                };
                Some(target.value)
            }
            _ => None,
        })
        .collect();
    assert_eq!(
        names,
        vec!["click", "navigate", "deselect", "click", "select", "click"]
    );
}

#[test]
fn roman_subsubstep_ordinals_preserved() {
    // The IR keeps the verbatim ordinal string from the parser; the lexical
    // kind (Arabic / Alpha / Roman) is derivable by inspection.
    let source = r#"
% technique v1

run :

1.  Outer.
    a.  Substep.
        i.   First sub-substep.
        ii.  Second sub-substep.
        "#
    .trim_ascii();
    let path = Path::new("Test.tq");
    let document = parsing::parse(path, source).expect("parse");
    let program = translate(&document).expect("translate");

    let Operation::Sequence(ops) = &program.subroutines[0].body else {
        panic!("expected Sequence");
    };
    let Operation::Step { body: outer, .. } = &ops[0] else {
        panic!("expected outer Step");
    };
    let Operation::Sequence(outer_ops) = outer.as_ref() else {
        panic!("expected Sequence");
    };
    let Operation::Step { body: substep, .. } = &outer_ops[1] else {
        panic!("expected substep");
    };
    let Operation::Sequence(sub_ops) = substep.as_ref() else {
        panic!("expected Sequence");
    };
    let ordinals: Vec<&str> = sub_ops
        .iter()
        .filter_map(|op| match op {
            Operation::Step {
                ordinal: Ordinal::Dependent(n),
                ..
            } => Some(*n),
            _ => None,
        })
        .collect();
    assert_eq!(ordinals, vec!["i", "ii"]);
}

#[test]
fn response_with_condition_carries_condition() {
    // 'No' but tired -> Response { value: "No", condition: Some("but tired") }
    let source = r#"
% technique v1

run :

1.  Decision time?
        'Yes' | 'No' but tired
        "#
    .trim_ascii();
    let path = Path::new("Test.tq");
    let document = parsing::parse(path, source).expect("parse");
    let program = translate(&document).expect("translate");

    let Operation::Sequence(ops) = &program.subroutines[0].body else {
        panic!("expected Sequence");
    };
    let Operation::Step { responses, .. } = &ops[0] else {
        panic!("expected Step");
    };
    assert_eq!(responses.len(), 2);
    assert_eq!(responses[0].value, "Yes");
    assert!(responses[0]
        .condition
        .is_none());
    assert_eq!(responses[1].value, "No");
    assert_eq!(responses[1].condition, Some("but tired"));
}

#[test]
fn multiple_peer_response_blocks_union_on_step() {
    // Two distinct ResponseBlocks under different attribute scopes peer to
    // the same Step. The Step's responses field accumulates all of them.
    let source = r#"
% technique v1

run :

1.  Choose
        @cook
            'A' | 'B'
        @waiter
            'C' | 'D'
        "#
    .trim_ascii();
    let path = Path::new("Test.tq");
    let document = parsing::parse(path, source).expect("parse");
    let program = translate(&document).expect("translate");

    let Operation::Sequence(ops) = &program.subroutines[0].body else {
        panic!("expected Sequence");
    };
    let Operation::Step { responses, .. } = &ops[0] else {
        panic!("expected Step");
    };
    let values: Vec<&str> = responses
        .iter()
        .map(|r| r.value)
        .collect();
    assert_eq!(values, vec!["A", "B", "C", "D"]);
}

#[test]
fn procedure_body_response_block_lands_on_subroutine() {
    // A ResponseBlock that is a peer of the procedure body (here under a
    // top-level @attribute frame) rolls up into Subroutine.responses, since
    // there is no enclosing Step or Loop carrier for it.
    let source = r#"
% technique v1

choose :

@me
    'A' | 'B'
        "#
    .trim_ascii();
    let path = Path::new("Test.tq");
    let document = parsing::parse(path, source).expect("parse");
    let program = translate(&document).expect("translate");

    assert_eq!(
        program.subroutines[0]
            .responses
            .len(),
        2
    );
    let values: Vec<&str> = program.subroutines[0]
        .responses
        .iter()
        .map(|r| r.value)
        .collect();
    assert_eq!(values, vec!["A", "B"]);
}

#[test]
fn section_title_invocation_hoists_into_body() {
    // A `<call>` in a section title hoists into the body as the explicit
    // entry, suppressing the auto-descent so `init` runs once, not twice.
    let source = r#"
% technique v1

main :

I. Begin <init>

init : () -> ()
        "#
    .trim_ascii();
    let path = Path::new("Test.tq");
    let document = parsing::parse(path, source).expect("parse");
    let mut program = translate(&document).expect("translate");
    resolve(&mut program).expect("resolve");

    let init_idx = program
        .subroutines
        .iter()
        .position(|s| {
            s.name
                .as_ref()
                .map(|n| n.value)
                == Some("init")
        })
        .expect("init declared");

    let Operation::Sequence(ops) = &program.subroutines[0].body else {
        panic!("expected Sequence");
    };
    let Operation::Section { body, .. } = &ops[0] else {
        panic!("expected Section");
    };
    let Operation::Sequence(section_body) = body.as_ref() else {
        panic!("expected Section body Sequence");
    };
    // The title's hoisted `<init>` Application is the section's single entry:
    // an explicit invocation in the heading suppresses the synthetic descent
    // into the first declared procedure, so the body holds one Invoke, not two.
    assert_eq!(
        section_body.len(),
        1,
        "title's Application is the sole entry"
    );
    let Operation::Invoke(invocable) = &section_body[0] else {
        panic!("expected Invoke, got {:?}", section_body[0]);
    };
    let SubroutineRef::Resolved(SubroutineId(idx)) = invocable.target else {
        panic!("expected Resolved");
    };
    assert_eq!(idx, init_idx);
}

#[test]
fn section_title_non_invoke_keeps_descent() {
    // A title executable that is not an entry invocation keeps the descent.
    let source = r#"
% technique v1

main :

I. Count is { 42 }

init : () -> ()
        "#
    .trim_ascii();
    let path = Path::new("Test.tq");
    let document = parsing::parse(path, source).expect("parse");
    let mut program = translate(&document).expect("translate");
    resolve(&mut program).expect("resolve");

    let Operation::Sequence(ops) = &program.subroutines[0].body else {
        panic!("expected Sequence");
    };
    let Operation::Section { body, .. } = &ops[0] else {
        panic!("expected Section");
    };
    let Operation::Sequence(section_body) = body.as_ref() else {
        panic!("expected Section body Sequence");
    };
    // The title's value-read, then the descent into `init`.
    assert_eq!(section_body.len(), 2);
    let Operation::Invoke(invocable) = &section_body[1] else {
        panic!("expected Invoke, got {:?}", section_body[1]);
    };
    assert_eq!(invocable.target, SubroutineRef::Resolved(SubroutineId(1)));
}
