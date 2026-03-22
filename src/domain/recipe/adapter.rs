//! Projects the parser's AST into the recipe domain model.
//!
//! Procedures whose steps contain Place-attributed tablets are treated as
//! ingredient sources; the remaining procedures contribute method steps.
//! The first procedure supplies the document title and description.

use std::collections::{HashMap, HashSet};

use crate::domain::Adapter;
use crate::language;

use super::types::{Document, Ingredient, Ingredients, Prose, Step};

pub struct RecipeAdapter;

impl Adapter for RecipeAdapter {
    type Model = Document;

    fn extract(&self, document: &language::Document) -> Document {
        extract(document)
    }
}

fn extract(document: &language::Document) -> Document {
    let mut doc = Document::new();

    let proc_map: HashMap<&str, &language::Procedure> = document
        .procedures()
        .map(|p| (p.name(), p))
        .collect();

    let mut resolved: HashSet<&str> = HashSet::new();
    let mut procedures = document.procedures();

    if let Some(first) = procedures.next() {
        doc.title = first
            .title()
            .map(String::from);
        doc.description = first
            .description()
            .map(|p| Prose::parse(&p.content()))
            .collect();

        for scope in first.steps() {
            collect_steps(&mut doc.steps, scope, None, &proc_map, &mut resolved);
        }

        // Top-level steps are phase headings, not numbered items.
        // Their direct children keep ordinals; grandchildren lose them.
        for step in &mut doc.steps {
            step.ordinal = None;
            for child in &mut step.children {
                strip_ordinals(&mut child.children);
            }
        }
    }

    for procedure in procedures {
        let mut items = Vec::new();
        for scope in procedure.steps() {
            collect_ingredients(&mut items, scope, None);
        }

        let description: Vec<Prose> = procedure
            .description()
            .map(|p| Prose::parse(&p.content()))
            .collect();

        if !items.is_empty() {
            doc.ingredients
                .push(Ingredients {
                    heading: procedure
                        .title()
                        .map(String::from),
                    description,
                    items,
                });
        } else if !resolved.contains(procedure.name()) {
            let mut children = Vec::new();
            for scope in procedure.steps() {
                collect_steps(&mut children, scope, None, &proc_map, &mut resolved);
            }
            if !children.is_empty() {
                doc.steps
                    .push(Step {
                        ordinal: None,
                        title: procedure
                            .title()
                            .map(String::from),
                        description,
                        role: None,
                        children,
                    });
            }
        }
    }

    // Handle bare top-level steps (no procedures)
    if doc
        .steps
        .is_empty()
    {
        for scope in document.steps() {
            collect_steps(&mut doc.steps, scope, None, &proc_map, &mut resolved);
        }
    }

    doc
}

/// Walk a scope tree collecting ingredients from Place-attributed tablets.
fn collect_ingredients(items: &mut Vec<Ingredient>, scope: &language::Scope, place: Option<&str>) {
    // Place attribute sets the source for contained ingredients
    let places: Vec<_> = scope
        .places()
        .collect();
    if !places.is_empty() {
        let name = places
            .first()
            .copied();
        for child in scope.children() {
            collect_ingredients(items, child, name);
        }
        return;
    }

    // Steps may contain tablet children
    if scope.is_step() {
        for child in scope.children() {
            if let Some(pairs) = child.tablet() {
                for pair in pairs {
                    items.push(Ingredient {
                        label: pair
                            .label
                            .to_string(),
                        quantity: format_value(&pair.value),
                        source: place.map(String::from),
                    });
                }
            }
        }
    }
}

/// Walk a scope tree collecting method steps, inheriting role downward.
/// Invocations are resolved by inlining the called procedure's content.
fn collect_steps<'i>(
    steps: &mut Vec<Step>,
    scope: &language::Scope<'i>,
    role: Option<&str>,
    proc_map: &HashMap<&'i str, &language::Procedure<'i>>,
    resolved: &mut HashSet<&'i str>,
) {
    if scope.is_step() {
        let title = scope
            .description()
            .next()
            .map(|p| p.content());
        let title = title.filter(|t| !t.is_empty());

        // Append built-in function parameters to title
        let suffix = scope
            .description()
            .next()
            .and_then(builtin_suffix_from_paragraph);
        let title = match (title, suffix) {
            (Some(t), Some(s)) => Some(format!("{} {}", t, s)),
            (t, _) => t,
        };

        let mut children = Vec::new();
        for child in scope.children() {
            if child
                .tablet()
                .is_some()
            {
                continue;
            }
            collect_steps(&mut children, child, role, proc_map, resolved);
        }

        // Resolve invocations: inline called procedures' content
        let mut description = Vec::new();
        let invocations: Vec<&str> = scope
            .description()
            .flat_map(|p| p.invocations())
            .collect();

        for target in invocations {
            if is_builtin(target) {
                continue;
            }
            if let Some(proc) = proc_map.get(target) {
                if resolved.insert(target) {
                    for para in proc.description() {
                        description.push(Prose::parse(&para.content()));
                    }
                    for child_scope in proc.steps() {
                        collect_steps(&mut children, child_scope, None, proc_map, resolved);
                    }
                }
            }
        }

        steps.push(Step {
            ordinal: scope
                .ordinal()
                .map(String::from),
            title,
            description,
            role: role.map(String::from),
            children,
        });
        return;
    }

    // Role attribute — inherit role name onto child steps
    let roles: Vec<_> = scope
        .roles()
        .collect();
    if !roles.is_empty() {
        let name = roles
            .first()
            .copied();
        for child in scope.children() {
            collect_steps(steps, child, name, proc_map, resolved);
        }
        return;
    }

    // Place attribute — skip (ingredient territory)
    if scope
        .places()
        .next()
        .is_some()
    {
        return;
    }

    // Other scopes — recurse
    for child in scope.children() {
        collect_steps(steps, child, role, proc_map, resolved);
    }
}

/// Recursively strip ordinals from steps (used for deeply nested steps).
fn strip_ordinals(steps: &mut Vec<Step>) {
    for step in steps {
        step.ordinal = None;
        strip_ordinals(&mut step.children);
    }
}

/// Format an expression value as a human-readable quantity string.
fn format_value(expr: &language::Expression) -> String {
    match expr {
        language::Expression::Number(language::Numeric::Scientific(q)) => q.to_string(),
        language::Expression::Number(language::Numeric::Integral(n)) => n.to_string(),
        _ => String::new(),
    }
}

// -- Recipe domain built-in functions ----------------------------------------

/// Returns true if the name is a recipe domain built-in function.
fn is_builtin(name: &str) -> bool {
    match name {
        "oven" | "timer" => true,
        _ => false,
    }
}

/// Render a built-in function call as a human-readable suffix.
fn builtin_suffix(name: &str, params: &[language::Expression]) -> Option<String> {
    let val = params
        .first()
        .map(format_value);
    let val = val.filter(|v| !v.is_empty());
    match name {
        "oven" => val.map(|v| format!("to {}", v)),
        "timer" => val.map(|v| format!("for {}", v)),
        _ => None,
    }
}

/// Extract a built-in suffix from a step's description paragraph.
fn builtin_suffix_from_paragraph(para: &language::Paragraph) -> Option<String> {
    for d in &para.0 {
        if let Some(s) = builtin_from_descriptive(d) {
            return Some(s);
        }
    }
    None
}

fn builtin_from_descriptive(d: &language::Descriptive) -> Option<String> {
    match d {
        language::Descriptive::CodeInline(expr) => builtin_from_expression(expr),
        language::Descriptive::Application(inv) => builtin_from_invocation(inv),
        language::Descriptive::Binding(inner, _) => builtin_from_descriptive(inner),
        _ => None,
    }
}

fn builtin_from_expression(expr: &language::Expression) -> Option<String> {
    match expr {
        language::Expression::Application(inv) => builtin_from_invocation(inv),
        language::Expression::Execution(func) => builtin_suffix(
            func.target
                .0,
            &func.parameters,
        ),
        language::Expression::Binding(inner, _) => builtin_from_expression(inner),
        _ => None,
    }
}

fn builtin_from_invocation(inv: &language::Invocation) -> Option<String> {
    let name = match &inv.target {
        language::Target::Local(id) => id.0,
        _ => return None,
    };
    match &inv.parameters {
        Some(params) => builtin_suffix(name, params),
        None => None,
    }
}

#[cfg(test)]
mod check {
    use std::path::Path;

    use crate::domain::Adapter;
    use crate::parsing;

    use super::RecipeAdapter;

    fn trim(s: &str) -> &str {
        s.strip_prefix('\n')
            .unwrap_or(s)
    }

    fn extract(source: &str) -> super::Document {
        let path = Path::new("test.tq");
        let doc = parsing::parse(path, source).unwrap();
        RecipeAdapter.extract(&doc)
    }

    #[test]
    fn title_and_description_from_first_procedure() {
        let doc = extract(trim(
            r#"
dinner :

# Christmas Dinner

A festive meal for the whole family.

    1. Cook food
            "#,
        ));
        assert_eq!(doc.title, Some("Christmas Dinner".into()));
        assert_eq!(
            doc.description
                .len(),
            1
        );
    }

    #[test]
    fn ingredients_from_place_scoped_tablets() {
        let doc = extract(trim(
            r#"
dinner :

# Dinner

    1. Get ingredients

turkey : () -> Ingredients

# Turkey

    ^butcher
        - Buy turkey
            {
                [
                    "Turkey" = 4 kg
                    "Bacon" = 2 pieces
                ]
            }
            "#,
        ));
        assert_eq!(
            doc.ingredients
                .len(),
            1
        );
        assert_eq!(doc.ingredients[0].heading, Some("Turkey".into()));
        assert_eq!(
            doc.ingredients[0]
                .items
                .len(),
            2
        );
        assert_eq!(doc.ingredients[0].items[0].label, "Turkey");
        assert_eq!(doc.ingredients[0].items[0].quantity, "4 kg");
        assert_eq!(doc.ingredients[0].items[0].source, Some("butcher".into()));
        assert_eq!(doc.ingredients[0].items[1].label, "Bacon");
        assert_eq!(doc.ingredients[0].items[1].quantity, "2 pieces");
    }

    #[test]
    fn method_steps_from_role_scoped_procedures() {
        let doc = extract(trim(
            r#"
dinner :

# Dinner

    1. Cook food

roast :

# Roast Turkey

    @chef
        1. Set oven temperature
        2. Place bird in oven
            "#,
        ));
        // First procedure contributes one overview step
        assert_eq!(
            doc.steps
                .len(),
            2
        );
        assert_eq!(doc.steps[0].title, Some("Cook food".into()));
        // Second procedure becomes a grouped step
        assert_eq!(doc.steps[1].title, Some("Roast Turkey".into()));
        assert_eq!(
            doc.steps[1]
                .children
                .len(),
            2
        );
        assert_eq!(doc.steps[1].children[0].role, Some("chef".into()));
    }

    #[test]
    fn ingredient_procedures_excluded_from_steps() {
        let doc = extract(trim(
            r#"
dinner :

# Dinner

    1. Get ingredients

shopping : () -> Ingredients

# Shopping

    ^store
        - Buy salt
            {
                [
                    "Salt" = 1 teaspoon
                ]
            }
            "#,
        ));
        assert_eq!(
            doc.ingredients
                .len(),
            1
        );
        // The shopping procedure should not also appear as method steps
        assert_eq!(
            doc.steps
                .len(),
            1
        );
        assert_eq!(doc.steps[0].title, Some("Get ingredients".into()));
    }

    #[test]
    fn invocations_inlined_as_children() {
        let doc = extract(trim(
            r#"
main :

# Main

    1. Do the thing { <sub>() }

sub :

# Sub Task

Detailed instructions follow.

    @worker
        1. Step one
        2. Step two
            "#,
        ));
        // sub is resolved via invocation, so only one top-level step
        assert_eq!(
            doc.steps
                .len(),
            1
        );
        assert_eq!(doc.steps[0].title, Some("Do the thing".into()));
        // sub's description is inherited
        assert_eq!(
            doc.steps[0]
                .description
                .len(),
            1
        );
        // sub's steps are inlined as children
        assert_eq!(
            doc.steps[0]
                .children
                .len(),
            2
        );
        assert_eq!(doc.steps[0].children[0].title, Some("Step one".into()));
        assert_eq!(doc.steps[0].children[0].role, Some("worker".into()));
    }

    #[test]
    fn builtin_functions_rendered_inline() {
        let doc = extract(trim(
            r#"
main :

# Main

    1. Set oven temperature { <oven>(180 °C) }
    2. Wait for cooking { timer(3 hr) }

oven(temperature) :

# Set oven temperature

    @chef
        - Set temperature to

            "#,
        ));
        // Built-in parameters appended to step titles
        assert_eq!(
            doc.steps[0].title,
            Some("Set oven temperature to 180 °C".into())
        );
        assert_eq!(doc.steps[1].title, Some("Wait for cooking for 3 hr".into()));
        // oven procedure not inlined as children
        assert!(doc.steps[0]
            .children
            .is_empty());
    }

    #[test]
    fn multiple_places_within_one_procedure() {
        let doc = extract(trim(
            r#"
dinner :

# Dinner

    1. Go shopping

stuffing : () -> Ingredients

# Stuffing

    ^store
        - Get spices
            {
                [
                    "Salt" = 1 teaspoon
                ]
            }

    ^bakery
        - Get bread
            {
                [
                    "Bread" = 2 slices
                ]
            }
            "#,
        ));
        assert_eq!(
            doc.ingredients
                .len(),
            1
        );
        assert_eq!(doc.ingredients[0].heading, Some("Stuffing".into()));
        assert_eq!(
            doc.ingredients[0]
                .items
                .len(),
            2
        );
        assert_eq!(doc.ingredients[0].items[0].source, Some("store".into()));
        assert_eq!(doc.ingredients[0].items[1].source, Some("bakery".into()));
    }
}
