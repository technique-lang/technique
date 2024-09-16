// parsing machinery

use pest::Parser;
use pest_derive::Parser;

#[derive(Parser)]
#[grammar = "../technique.pest"]
struct TechniqueParser;

pub fn load() {}

#[cfg(test)]
mod tests {
    use super::*; // Import all parent module items

    #[test]
    fn check_procedure_declaration() {
        let input = "making_coffee : Beans -> Coffee";

        let declaration = TechniqueParser::parse(Rule::declaration, &input)
            .expect("Unsuccessful Parse")
            .next()
            .unwrap();

        assert_eq!(declaration.as_str(), "making_coffee : Beans -> Coffee");
        assert_eq!(declaration.as_rule(), Rule::declaration);

        let identifier = declaration
            .into_inner()
            .next()
            .unwrap();

        assert_eq!(identifier.as_str(), "making_coffee");
        assert_eq!(identifier.as_rule(), Rule::identifier);
    }
}
