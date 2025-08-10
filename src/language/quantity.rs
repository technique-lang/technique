//! Quantity types and parsing for scientific measurements with uncertainty and units
//!
//! A Quantity is an amount, possibly with uncertainty, at the magnitude if
//! given, of the units specified.
//!
//! Valid Quantities include:
//!
//! 149 kg
//! 5.9722 × 10²⁴ kg
//! 5.9722 ± 0.0006 kg
//! 5.9722 ± 0.0006 × 10²⁴ kg
//!
//! More conventional ASCII symbol characters are also supported when writing
//! Quantity values in a Technique file:
//!
//! 5.9722 * 10^24 kg
//! 5.9722 +/- 0.0006 kg
//! 5.9722 +/- 0.0006 × 10^24 kg
//!
//! The parser and validation code has considerable flexibility to handle
//! various input formats.

use crate::regex::*;
use std::fmt::{self, Display};

#[derive(Debug, PartialEq, Eq)]
pub struct Quantity<'i> {
    pub mantissa: Decimal,
    pub uncertainty: Option<Decimal>,
    pub magnitude: Option<i8>,
    pub symbol: &'i str,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub struct Decimal {
    pub number: i64,
    pub precision: u8,
}

/// Parse a string as a Quantity if it matches the expected format
pub fn parse_quantity(input: &str) -> Option<Quantity> {
    // Look for patterns that indicate a quantity:
    // - decimal number followed by space and unit symbol
    // - decimal number with uncertainty (±)
    // - decimal number with magnitude (× or x)
    let re = regex!(r"^\s*(-?[0-9]+(?:\.[0-9]+)?)\s*(.*)$");

    let cap = re.captures(input)?;
    let one = cap.get(1)?;
    let two = cap.get(2)?;

    // Parse the mantissa as a decimal
    let mantissa = parse_decimal(one.as_str())?;

    // Parse the remainder for uncertainty, magnitude, and symbol
    let mut remainder = two
        .as_str()
        .trim_ascii();

    let mut uncertainty = None;
    let mut magnitude = None;

    let re = regex!(r"^(?:±|\+/-)\s*([0-9]+(?:\.[0-9]+)?)\s*(.*)$");

    // Parse uncertainty (± or +/-)
    if let Some(cap) = re.captures(remainder) {
        uncertainty = Some(parse_decimal(
            cap.get(1)?
                .as_str(),
        )?);
        remainder = cap
            .get(2)?
            .as_str();
    }

    // Parse magnitude (× 10^n or x 10^n or * 10^n)
    let re = regex!(r"^(?:×|x|\*)\s*10(?:\^(-?[0-9]+)|([⁰¹²³⁴⁵⁶⁷⁸⁹⁻]+))\s*(.*)$");
    if let Some(cap) = re.captures(remainder) {
        let exp_str = if let Some(ascii_exp) = cap.get(1) {
            ascii_exp.as_str()
        } else if let Some(super_exp) = cap.get(2) {
            // Convert superscript to regular digits
            &convert_superscript(super_exp.as_str())
        } else {
            return None;
        };

        magnitude = Some(
            exp_str
                .parse()
                .ok()?,
        );
        remainder = cap
            .get(3)?
            .as_str();
    }

    // The rest should be the unit symbol
    let symbol = remainder.trim_ascii();
    if symbol.is_empty() {
        return None; // Quantities must have units
    }

    // Validate unit symbol contains only allowed characters
    let re = regex!(r"^[a-zA-Z°/μ]+$");
    if !re.is_match(symbol) {
        return None;
    }

    Some(Quantity {
        mantissa,
        uncertainty,
        magnitude,
        symbol,
    })
}

pub fn parse_decimal(input: &str) -> Option<Decimal> {
    if let Some(dot_pos) = input.find('.') {
        // Has decimal point
        let whole_part = &input[..dot_pos];
        let frac_part = &input[dot_pos + 1..];

        let whole: i64 = whole_part
            .parse()
            .ok()?;
        let frac: i64 = frac_part
            .parse()
            .ok()?;
        let precision = frac_part.len() as u8;

        // Combine whole and fractional parts
        let number = whole * 10_i64.pow(precision as u32) + if whole < 0 { -frac } else { frac };

        Some(Decimal { number, precision })
    } else {
        // Integer
        let number: i64 = input
            .parse()
            .ok()?;
        Some(Decimal {
            number,
            precision: 0,
        })
    }
}

/// Convert Unicode superscript characters to ASCII digits
pub fn convert_superscript(input: &str) -> String {
    input
        .chars()
        .map(|c| match c {
            '⁰' => '0',
            '¹' => '1',
            '²' => '2',
            '³' => '3',
            '⁴' => '4',
            '⁵' => '5',
            '⁶' => '6',
            '⁷' => '7',
            '⁸' => '8',
            '⁹' => '9',
            '⁻' => '-',
            _ => c,
        })
        .collect()
}

impl Display for Decimal {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        if self.precision == 0 {
            write!(f, "{}", self.number)
        } else {
            let divisor = 10_i64.pow(self.precision as u32);
            let whole = self.number / divisor;
            let frac = self
                .number
                .abs()
                % divisor;
            write!(
                f,
                "{}.{:0width$}",
                whole,
                frac,
                width = self.precision as usize
            )
        }
    }
}

impl<'i> Display for Quantity<'i> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        // Start with mantissa
        write!(f, "{}", self.mantissa)?;

        // Add uncertainty if present
        if let Some(uncertainty) = &self.uncertainty {
            write!(f, " ± {}", uncertainty)?;
        }

        // Add magnitude if present
        if let Some(magnitude) = &self.magnitude {
            write!(f, " × 10{}", to_superscript(*magnitude))?;
        }

        // Add unit symbol
        write!(f, " {}", self.symbol)?;

        Ok(())
    }
}

fn to_superscript(num: i8) -> String {
    num.to_string()
        .chars()
        .map(|c| match c {
            '0' => '⁰',
            '1' => '¹',
            '2' => '²',
            '3' => '³',
            '4' => '⁴',
            '5' => '⁵',
            '6' => '⁶',
            '7' => '⁷',
            '8' => '⁸',
            '9' => '⁹',
            '-' => '⁻',
            _ => c,
        })
        .collect()
}

#[cfg(test)]
mod check {
    use super::*;

    #[test]
    fn parse_simple_quantities() {
        // Simple quantity with unit
        let result = parse_quantity("4 kg").unwrap();
        assert_eq!(
            result.mantissa,
            Decimal {
                number: 4,
                precision: 0
            }
        );
        assert_eq!(result.uncertainty, None);
        assert_eq!(result.magnitude, None);
        assert_eq!(result.symbol, "kg");
    }

    #[test]
    fn parse_decimal_quantities() {
        let result = parse_quantity("5.9722 kg").unwrap();
        assert_eq!(
            result.mantissa,
            Decimal {
                number: 59722,
                precision: 4
            }
        );
        assert_eq!(result.symbol, "kg");
    }

    #[test]
    fn parse_quantity_with_uncertainty() {
        let result = parse_quantity("4 ± 1 kg").unwrap();
        assert_eq!(
            result.mantissa,
            Decimal {
                number: 4,
                precision: 0
            }
        );
        assert_eq!(
            result.uncertainty,
            Some(Decimal {
                number: 1,
                precision: 0
            })
        );
        assert_eq!(result.symbol, "kg");
    }

    #[test]
    fn parse_quantity_with_magnitude() {
        let result = parse_quantity("4 × 10^2 kg").unwrap();
        assert_eq!(
            result.mantissa,
            Decimal {
                number: 4,
                precision: 0
            }
        );
        assert_eq!(result.magnitude, Some(2));
        assert_eq!(result.symbol, "kg");
    }

    #[test]
    fn parse_full_quantity() {
        let result = parse_quantity("5.9722 ± 0.0006 × 10^24 kg").unwrap();
        assert_eq!(
            result.mantissa,
            Decimal {
                number: 59722,
                precision: 4
            }
        );
        assert_eq!(
            result.uncertainty,
            Some(Decimal {
                number: 6,
                precision: 4
            })
        );
        assert_eq!(result.magnitude, Some(24));
        assert_eq!(result.symbol, "kg");
    }

    #[test]
    fn parse_arbitrary_units() {
        assert_eq!(
            parse_quantity("3 breadsticks")
                .unwrap()
                .symbol,
            "breadsticks"
        );
        assert_eq!(
            parse_quantity("42 widgets")
                .unwrap()
                .symbol,
            "widgets"
        );
        assert_eq!(
            parse_quantity("15 m/s")
                .unwrap()
                .symbol,
            "m/s"
        );
        assert_eq!(
            parse_quantity("20 °C")
                .unwrap()
                .symbol,
            "°C"
        );
        assert_eq!(
            parse_quantity("5.5 μm")
                .unwrap()
                .symbol,
            "μm"
        );
    }

    #[test]
    fn parse_superscript_magnitude() {
        let result = parse_quantity("6.022 × 10²³ mol").unwrap();
        assert_eq!(result.magnitude, Some(23));
        assert_eq!(result.symbol, "mol");
    }

    #[test]
    fn invalid_quantities() {
        assert!(parse_quantity("42").is_none()); // No units
        assert!(parse_quantity("kg").is_none()); // No number
        assert!(parse_quantity("4 kg-meters").is_none()); // Invalid unit chars (hyphen)
        assert!(parse_quantity("4 kg_squared").is_none()); // Invalid unit chars (underscore)
        assert!(parse_quantity("4 × 11^2 kg").is_none()); // Invalid magnitude base (not 10)
        assert!(parse_quantity("4 × kg").is_none()); // Missing magnitude
        assert!(parse_quantity("4 ± kg").is_none()); // Missing uncertainty value
        assert!(parse_quantity("4 k g").is_none()); // Space in unit symbol
        assert!(parse_quantity("4 ± 1.5").is_none()); // No units after uncertainty
        assert!(parse_quantity("").is_none()); // Empty string
        assert!(parse_quantity("   ").is_none()); // Just whitespace
    }

    #[test]
    fn decimal_display() {
        assert_eq!(
            Decimal {
                number: 42,
                precision: 0
            }
            .to_string(),
            "42"
        );
        assert_eq!(
            Decimal {
                number: 59722,
                precision: 4
            }
            .to_string(),
            "5.9722"
        );
        assert_eq!(
            Decimal {
                number: -123,
                precision: 0
            }
            .to_string(),
            "-123"
        );
        assert_eq!(
            Decimal {
                number: -59722,
                precision: 4
            }
            .to_string(),
            "-5.9722"
        );
        assert_eq!(
            Decimal {
                number: 6,
                precision: 4
            }
            .to_string(),
            "0.0006"
        );
    }

    #[test]
    fn quantity_display() {
        let simple = Quantity {
            mantissa: Decimal {
                number: 4,
                precision: 0,
            },
            uncertainty: None,
            magnitude: None,
            symbol: "kg",
        };
        assert_eq!(simple.to_string(), "4 kg");

        let with_uncertainty = Quantity {
            mantissa: Decimal {
                number: 4,
                precision: 0,
            },
            uncertainty: Some(Decimal {
                number: 1,
                precision: 0,
            }),
            magnitude: None,
            symbol: "kg",
        };
        assert_eq!(with_uncertainty.to_string(), "4 ± 1 kg");

        let with_magnitude = Quantity {
            mantissa: Decimal {
                number: 4,
                precision: 0,
            },
            uncertainty: None,
            magnitude: Some(2),
            symbol: "kg",
        };
        assert_eq!(with_magnitude.to_string(), "4 × 10² kg");

        let full = Quantity {
            mantissa: Decimal {
                number: 59722,
                precision: 4,
            },
            uncertainty: Some(Decimal {
                number: 6,
                precision: 4,
            }),
            magnitude: Some(24),
            symbol: "kg",
        };
        assert_eq!(full.to_string(), "5.9722 ± 0.0006 × 10²⁴ kg");
    }
}
