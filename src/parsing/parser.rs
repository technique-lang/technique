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

        let mut pairs = declaration.into_inner();

        let identifier = pairs
            .next()
            .unwrap();

        assert_eq!(identifier.as_str(), "making_coffee");
        assert_eq!(identifier.as_rule(), Rule::identifier);

        let signature = pairs
            .next()
            .unwrap();

        assert_eq!(signature.as_str(), "Beans -> Coffee");
        assert_eq!(signature.as_rule(), Rule::signature);

        let mut pairs = signature.into_inner();

        let domain = pairs.next().unwrap();

        assert_eq!(domain.as_str(), "Beans");
        assert_eq!(domain.as_rule(), Rule::typa);

        let range = pairs.next().unwrap();

        assert_eq!(range.as_str(), "Coffee");
        assert_eq!(range.as_rule(), Rule::typa);
    }
}
