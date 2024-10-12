// Abstact Syntax Trees for the Technique language

pub struct Technique<'i> {
    pub version: u8,
    pub license: &'i str, 
    pub copyright: &'i str 
}

impl<'i> Default for Technique<'i> {
    fn default() -> Self {
        Technique {
            version: 1,
            license: "",
            copyright: ""
        }
    }
}
