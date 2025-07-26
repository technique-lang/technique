//! Code formatter for the Technique language

use technique::language::*;

pub fn format<'i>(technique: &Technique) -> String {
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

        self.append_str(" : ");

        if let Some(signature) = &procedure.signature {
            self.append_genus(&signature.domain);
            self.append_str(" -> ");
            self.append_genus(&signature.range);
        }

        self.append_char('\n');
    }

    fn append_genus<'i>(&mut self, genus: &Genus<'i>) {
        match genus {
            Genus::Unit => self.append_str("()"),
            Genus::Single(forma) => self.append_forma(forma),
            Genus::Tuple(formas) => {
                self.append_char('(');
                for (i, forma) in formas
                    .iter()
                    .enumerate()
                {
                    if i > 0 {
                        self.append_str(", ");
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

    fn append_forma<'i>(&mut self, forma: &Forma<'i>) {
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
}
