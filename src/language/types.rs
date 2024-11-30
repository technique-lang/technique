// Abstract Syntax Trees for the Technique language

#[derive(Eq, Debug, PartialEq)]
pub struct Technique<'i> {
    pub version: u8,
    pub license: Option<&'i str>,
    pub copyright: Option<&'i str>,
    pub template: Option<&'i str>,
}

impl Default for Technique<'_> {
    fn default() -> Self {
        Technique {
            version: 1,
            license: None,
            copyright: None,
            template: None,
        }
    }
}

#[derive(Eq, Debug, PartialEq)]
pub struct Procedure<'i> {
    pub name: &'i str,
    pub signature: Option<Signature<'i>>,
}

#[derive(Eq, Debug, PartialEq)]
pub struct Forma<'i> {
    pub name: &'i str,
}

#[derive(Eq, Debug, PartialEq)]
pub enum Genus<'i> {
    Single(Forma<'i>),
    Tuple(Vec<Forma<'i>>),
    List(Forma<'i>),
}

#[derive(Eq, Debug, PartialEq)]
pub struct Signature<'i> {
    pub domain: Genus<'i>,
    pub range: Genus<'i>,
}

#[derive(Eq, Debug, PartialEq)]
pub struct Attribute<'i> {
    pub name: &'i str,
}

#[cfg(test)]
mod tests {
    use super::*;

    fn maker<'i>() -> Technique<'i> {
        let t1 = Technique {
            version: 1,
            license: None,
            copyright: None,
            template: None,
        };

        t1
    }

    #[test]
    fn check_ast_construction() {
        let t1 = Technique {
            version: 1,
            license: None,
            copyright: None,
            template: None,
        };

        assert_eq!(Technique::default(), t1);

        let t2 = Technique {
            version: 1,
            license: Some("MIT"),
            copyright: Some("ACME, Inc"),
            template: Some("checklist"),
        };

        let t3 = maker();

        assert_eq!(t3, t1);

        let t4 = Technique {
            license: Some("MIT"),
            copyright: Some("ACME, Inc"),
            template: Some("checklist"),
            ..t3
        };

        assert_eq!(t4, t2);
    }
}
