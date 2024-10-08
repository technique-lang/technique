// parsing machinery

use winnow::error::StrContext;
use winnow::token::{one_of, take_while};
use winnow::{PResult, Parser};

pub fn parse_via_winnow(content: &str) {
    let result = parse_identifier.parse(content).unwrap();
    println!("{}", result);
}

// a winnow parser that takes an alpha and then any character
fn parse_identifier<'s>(input: &mut &'s str) -> PResult<&'s str> {
    (
        one_of('a'..='z'),
        take_while(0.., (('0'..='9'), ('a'..='z'), ('_'))),
    )
        .take()
        .verify(|s: &str| s.len() == input.len())
        .context(StrContext::Label("identifier"))
        .parse_next(input)
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn check_identifier_rules() {
        let mut input = "p";

        let result = parse_identifier
            .parse_next(&mut input)
            .unwrap();

        assert_eq!(result, "p");

        let mut input = "pizza";
        let result = parse_identifier
            .parse_next(&mut input)
            .unwrap();
        assert_eq!(result, "pizza");

        let mut input = "cook_pizza";
        let result = parse_identifier
            .parse_next(&mut input)
            .unwrap();
        assert_eq!(result, "cook_pizza");

        assert!(parse_identifier(&mut "0trust").is_err());
        assert!(parse_identifier(&mut "Pizza").is_err());
        assert!(parse_identifier(&mut "pizZa").is_err());

        assert_eq!(parse_identifier(&mut "cook_pizza"), Ok("cook_pizza"));
        assert!(parse_identifier(&mut "cook-pizza").is_err());
    }
}
