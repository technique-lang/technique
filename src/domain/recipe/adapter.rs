//! Projects the parser's AST into the recipe domain model.
//!
//! Procedures whose steps contain Place-attributed tablets are treated as
//! ingredient sources; the remaining procedures contribute method steps.
//! The first procedure supplies the document title and description.

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
            collect_steps(&mut doc.steps, scope, None);
        }
    }

    for procedure in procedures {
        let mut items = Vec::new();
        for scope in procedure.steps() {
            collect_ingredients(&mut items, scope, None);
        }

        if !items.is_empty() {
            doc.ingredients
                .push(Ingredients {
                    heading: procedure
                        .title()
                        .map(String::from),
                    items,
                });
        } else {
            let mut children = Vec::new();
            for scope in procedure.steps() {
                collect_steps(&mut children, scope, None);
            }
            if !children.is_empty() {
                doc.steps
                    .push(Step {
                        ordinal: None,
                        text: procedure
                            .title()
                            .map(String::from),
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
            collect_steps(&mut doc.steps, scope, None);
        }
    }

    doc
}

/// Walk a scope tree collecting ingredients from Place-attributed tablets.
fn collect_ingredients(
    items: &mut Vec<Ingredient>,
    scope: &language::Scope,
    place: Option<&str>,
) {
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
        return;
    }

    // Role attributes — pass through
    if scope
        .roles()
        .next()
        .is_some()
    {
        for child in scope.children() {
            collect_ingredients(items, child, place);
        }
    }
}

/// Walk a scope tree collecting method steps, inheriting role downward.
fn collect_steps(steps: &mut Vec<Step>, scope: &language::Scope, role: Option<&str>) {
    if scope.is_step() {
        let text = scope
            .description()
            .next()
            .map(|p| p.content());
        let text = text.filter(|t| !t.is_empty());

        let mut children = Vec::new();
        for child in scope.children() {
            if child.tablet().is_some() {
                continue;
            }
            collect_steps(&mut children, child, role);
        }

        steps.push(Step {
            ordinal: scope
                .ordinal()
                .map(String::from),
            text,
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
            collect_steps(steps, child, name);
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
        collect_steps(steps, child, role);
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
        assert_eq!(
            doc.ingredients[0].items[0].source,
            Some("butcher".into())
        );
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
        assert_eq!(
            doc.steps[0]
                .text,
            Some("Cook food".into())
        );
        // Second procedure becomes a grouped step
        assert_eq!(
            doc.steps[1]
                .text,
            Some("Roast Turkey".into())
        );
        assert_eq!(
            doc.steps[1]
                .children
                .len(),
            2
        );
        assert_eq!(
            doc.steps[1].children[0].role,
            Some("chef".into())
        );
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
        assert_eq!(
            doc.steps[0]
                .text,
            Some("Get ingredients".into())
        );
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
        assert_eq!(
            doc.ingredients[0].items[0].source,
            Some("store".into())
        );
        assert_eq!(
            doc.ingredients[0].items[1].source,
            Some("bakery".into())
        );
    }
}
