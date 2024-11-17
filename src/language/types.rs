// Abstact Syntax Trees for the Technique language

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
            template: None
        }
    }
}

#[derive(Eq, Debug, PartialEq)]
pub struct Procedure {
    pub name: String,
    pub signature: Option<Signature>,
}

#[derive(Eq, Debug, PartialEq)]
pub struct Forma {
    pub name: String
}

#[derive(Eq, Debug, PartialEq)]
pub enum Genus {
    Single(Forma),
    Tuple(Vec<Forma>),
    List(Forma)
}

#[derive(Eq, Debug, PartialEq)]
pub struct Signature {
    pub domain: Genus,
    pub range: Genus,
}

#[derive(Eq, Debug, PartialEq)]
pub struct Attribute {
    pub name: String
}
