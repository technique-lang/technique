// Abstact Syntax Trees for the Technique language

#[derive(Eq, Debug, PartialEq)]
pub struct Technique {
    pub version: u8,
    pub license: Option<String>,
    pub copyright: Option<String>,
}

impl Default for Technique {
    fn default() -> Self {
        Technique {
            version: 1,
            license: None,
            copyright: None,
        }
    }
}
