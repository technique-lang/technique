//! Code formatter for the Technique language

use technique::language::*;

pub fn format(technique: &Technique) -> String {
    let mut output = Formatter::new();

    if let Some(metadata) = &technique.header {
        output.format_header(metadata);
    }

    if let Some(procedures) = &technique.body {
        for procedure in procedures {
            output.format_procedure(procedure);
        }
    }

    output.buffer
}

struct Formatter {
    buffer: String,
}

impl Formatter {
    fn new() -> Formatter {
        Formatter {
            buffer: String::new(),
        }
    }

    #[cfg(test)]
    fn reset(&mut self) {
        self.buffer
            .clear();
    }

    fn append_str(&mut self, text: &str) {
        self.buffer
            .push_str(text);
    }

    fn append_char(&mut self, c: char) {
        self.buffer
            .push(c);
    }

    fn is_empty(&self) -> bool {
        self.buffer
            .len()
            == 0
    }

    fn format_header(&mut self, metadata: &Metadata) {
        self.append_str("% technique v1\n");

        if let Some(license) = metadata.license {
            self.append_str("! ");
            self.append_str(license);

            if let Some(copyright) = metadata.copyright {
                self.append_str("; © ");
                self.append_str(copyright);
            }

            self.append_char('\n');
        }

        if let Some(template) = metadata.template {
            self.append_str("& ");
            self.append_str(template);
            self.append_char('\n');
        }
    }

    fn format_procedure(&mut self, procedure: &Procedure) {
        // if a header or another procedure has already been added,
        // separate the upcoming one with a blank line.
        if !self.is_empty() {
            self.append_char('\n');
        }

        let name = &procedure.name;
        self.append_str(name.0);

        self.append_char(' ');
        self.append_char(':');

        if let Some(signature) = &procedure.signature {
            self.append_char(' ');
            self.append_signature(signature);
        }

        self.append_char('\n');
    }

    fn append_signature(&mut self, signature: &Signature) {
        self.append_genus(&signature.domain);
        self.append_str(" -> ");
        self.append_genus(&signature.range);
    }

    fn append_genus(&mut self, genus: &Genus) {
        match genus {
            Genus::Unit => {
                self.append_char('(');
                self.append_char(')');
            }
            Genus::Single(forma) => self.append_forma(forma),
            Genus::Tuple(formas) => {
                self.append_char('(');
                for (i, forma) in formas
                    .iter()
                    .enumerate()
                {
                    if i > 0 {
                        self.append_char(',');
                        self.append_char(' ');
                    }
                    self.append_forma(forma);
                }
                self.append_char(')');
            }
            Genus::List(forma) => {
                self.append_char('[');
                self.append_forma(forma);
                self.append_char(']');
            }
        }
    }

    fn append_forma(&mut self, forma: &Forma) {
        self.append_str(forma.0)
    }
}

#[cfg(test)]
mod check {
    use super::*;

    #[test]
    fn genus() {
        let mut output = Formatter::new();

        output.append_forma(&Forma("Jedi"));
        assert_eq!(output.buffer, "Jedi");

        output.reset();
        output.append_genus(&Genus::Unit);
        assert_eq!(output.buffer, "()");

        output.reset();
        output.append_genus(&Genus::Single(Forma("Stormtrooper")));
        assert_eq!(output.buffer, "Stormtrooper");

        output.reset();
        output.append_genus(&Genus::List(Forma("Pilot")));
        assert_eq!(output.buffer, "[Pilot]");

        output.reset();
        output.append_genus(&Genus::Tuple(vec![
            Forma("Kid"),
            Forma("Pilot"),
            Forma("Scoundrel"),
            Forma("Princess"),
        ]));
        assert_eq!(output.buffer, "(Kid, Pilot, Scoundrel, Princess)");

        output.reset();
    }

    #[test]
    fn signatures() {
        let mut output = Formatter::new();

        output.append_signature(&Signature {
            domain: Genus::Single(Forma("Alderaan")),
            range: Genus::Single(Forma("AsteroidField")),
        });
        assert_eq!(output.buffer, "Alderaan -> AsteroidField");

        output.reset();
        output.append_signature(&Signature {
            domain: Genus::List(Forma("Clone")),
            range: Genus::Single(Forma("Army")),
        });
        assert_eq!(output.buffer, "[Clone] -> Army");

        output.reset();
        output.append_signature(&Signature {
            domain: Genus::Single(Forma("TaxationOfTradeRoutes")),
            range: Genus::Tuple(vec![Forma("Rebels"), Forma("Empire")]),
        });
        assert_eq!(output.buffer, "TaxationOfTradeRoutes -> (Rebels, Empire)");
    }
}
