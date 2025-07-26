#[cfg(test)]
mod verify {
    use technique::language::*;
    use technique::formatting::*;
    
    fn trim(text: &str) -> &str {
        let head = text.trim_start_matches('\n');
        let tail = head.trim_end_matches(' ');
        tail
    }

    #[test]
    fn header_and_body() {
        let technique = Technique {
            header: Some(Metadata {
                version: 1,
                license: Some("MIT"),
                copyright: None,
                template: Some("checklist"),
            }),
            body: None,
        };

        let result = format(&technique);
        assert_eq!(
            result,
            trim(
                r#"
% technique v1
! MIT
& checklist
                "#
            )
        );

        let technique = Technique {
            header: None,
            body: Some(vec![Procedure {
                name: Identifier("first"),
                parameters: None,
                signature: Some(Signature {
                    domain: Genus::Single(Forma("A")),
                    range: Genus::Single(Forma("B")),
                }),
                title: None,
                description: vec![],
                attribute: vec![],
                steps: vec![],
            }]),
        };

        let result = format(&technique);
        assert_eq!(
            result,
            trim(
                r#"
first : A -> B
                "#
            )
        );

        let technique = Technique {
            header: Some(Metadata {
                version: 1,
                license: Some("PD"),
                copyright: Some("2025 The First Procedure Society, Inc"),
                template: None,
            }),
            body: Some(vec![
                Procedure {
                    name: Identifier("first"),
                    parameters: None,
                    signature: Some(Signature {
                        domain: Genus::Single(Forma("A")),
                        range: Genus::Single(Forma("B")),
                    }),
                    title: None,
                    description: vec![],
                    attribute: vec![],
                    steps: vec![],
                },
                Procedure {
                    name: Identifier("second"),
                    parameters: None,
                    signature: Some(Signature {
                        domain: Genus::List(Forma("Thing")),
                        range: Genus::Tuple(vec![Forma("Who"), Forma("Where"), Forma("Why")]),
                    }),
                    title: None,
                    description: vec![],
                    attribute: vec![],
                    steps: vec![],
                },
            ]),
        };

        let result = format(&technique);
        assert_eq!(
            result,
            trim(
                r#"
% technique v1
! PD; © 2025 The First Procedure Society, Inc

first : A -> B

second : [Thing] -> (Who, Where, Why)
                "#
            )
        );
    }
}
