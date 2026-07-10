//! Values are the heart of the Technique language; they are the payload of
//! the results of executing steps. Values are produced at runtime by reducing
//! Operations, and are stored in PFFTT records on-disk.

use std::fmt::{self, Display};

use crate::language;

/// A value is the payload in the result of a step.
///
/// Need names? Science names newly discovered creatures in Latin. I don't
/// speak Latin, but neither does anyone else so we can just make words up.
/// Yeay! (The lengths some people will go to in order to avoid qualified
/// imports is really impressive, isn't it?)
///
/// `Value` is fully owned as results in PFFTT files stand alone irrespective
/// of a source document or live runtime environment.
#[derive(Debug, Clone, PartialEq)]
pub enum Value {
    Unitus,
    Literali(String),
    Enumerati(String),
    Quanticle(Numeric),
    Intratempse(Numeric),
    Tabularum(Vec<(String, Value)>),
    Arraeum(Vec<Value>),
    Parametriq(Vec<Value>),
    Futurae(String),
}

/// Owned mirror of `language::Numeric`.
#[derive(Debug, Clone, PartialEq, Eq)]
pub enum Numeric {
    Integral(i64),
    Scientific(Quantity),
}

/// Owned mirror of `language::Quantity`.
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Quantity {
    pub mantissa: language::Decimal,
    pub uncertainty: Option<language::Decimal>,
    pub magnitude: Option<i8>,
    pub symbol: String,
}

impl From<&language::Numeric<'_>> for Numeric {
    fn from(n: &language::Numeric<'_>) -> Self {
        match n {
            language::Numeric::Integral(i) => Numeric::Integral(*i),
            language::Numeric::Scientific(q) => Numeric::Scientific(Quantity::from(q)),
        }
    }
}

impl From<&language::Quantity<'_>> for Quantity {
    fn from(q: &language::Quantity<'_>) -> Self {
        Quantity {
            mantissa: q.mantissa,
            uncertainty: q.uncertainty,
            magnitude: q.magnitude,
            symbol: q
                .symbol
                .to_string(),
        }
    }
}

/// Plain-text rendering of a Value, suitable for splicing into an
/// interpolated string fragment or showing the user in a step description
/// prompt. Numeric formatting delegates to `crate::formatting`'s number
/// renderer so the user sees the same shape they would in source.
///
/// Not intended for on-disk serialization.
impl Display for Value {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Value::Unitus => Ok(()),
            Value::Literali(text) => write!(f, "\"{}\"", text),
            Value::Enumerati(text) => write!(f, "'{}'", text),
            Value::Quanticle(numeric) => write!(f, "{}", numeric),
            Value::Intratempse(numeric) => write!(f, "$({})", numeric),
            Value::Tabularum(pairs) => {
                f.write_str("[")?;
                for (i, (label, value)) in pairs
                    .iter()
                    .enumerate()
                {
                    if i > 0 {
                        f.write_str(", ")?;
                    }
                    write!(f, "{} = {}", label, value)?;
                }
                f.write_str("]")
            }
            Value::Parametriq(values) => {
                f.write_str("(")?;
                for (i, value) in values
                    .iter()
                    .enumerate()
                {
                    if i > 0 {
                        f.write_str(", ")?;
                    }
                    write!(f, "{}", value)?;
                }
                f.write_str(")")
            }
            Value::Arraeum(values) => {
                f.write_str("[")?;
                for (i, value) in values
                    .iter()
                    .enumerate()
                {
                    if i > 0 {
                        f.write_str(", ")?;
                    }
                    write!(f, "{}", value)?;
                }
                f.write_str("]")
            }
            Value::Futurae(name) => write!(f, "{{{}}}", name),
        }
    }
}

impl Value {
    /// Unquoted text for a prompt label: `Literali`/`Enumerati` unwrap
    /// their string, everything else falls back to `Display`.
    pub fn label(&self) -> String {
        match self {
            Value::Literali(text) | Value::Enumerati(text) => text.clone(),
            other => other.to_string(),
        }
    }
}

// Numeric rendering goes through `crate::formatting`'s number renderer.
// To call into it we briefly reconstruct a borrowed `language::Numeric`
impl Display for Numeric {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Numeric::Integral(i) => {
                let n = language::Numeric::Integral(*i);
                f.write_str(&crate::formatting::render_numeric(
                    &n,
                    &crate::formatting::Identity,
                ))
            }
            Numeric::Scientific(q) => {
                let qb = language::Quantity {
                    mantissa: q.mantissa,
                    uncertainty: q.uncertainty,
                    magnitude: q.magnitude,
                    symbol: &q.symbol,
                };
                let n = language::Numeric::Scientific(qb);
                f.write_str(&crate::formatting::render_numeric(
                    &n,
                    &crate::formatting::Identity,
                ))
            }
        }
    }
}

#[cfg(test)]
#[path = "checks/types.rs"]
mod check;
