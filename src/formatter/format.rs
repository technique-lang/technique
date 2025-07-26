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

    fn append_str(&mut self, text: &str) {
        self.buffer
            .push_str(text);
    }

    fn append_newline(&mut self) {
        self.buffer
            .push('\n');
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

            self.append_newline();
        }

        if let Some(template) = metadata.template {
            self.append_str("& ");
            self.append_str(template);
            self.append_newline();
        }
    }

    fn format_procedure(&mut self, procedure: &Procedure) {
        // if a header or another procedure has already been added,
        // separate the upcoming one with a blank line.
        if !self.is_empty() {
            self.append_newline();
        }

        let name = &procedure.name;
        self.append_str(name.0);

        self.append_str(" : ");

        if let Some(signature) = &procedure.signature {
            self.append_genus(&signature.domain);
            self.append_str(" -> ");
            self.append_genus(&signature.range);
        }

        self.buffer
            .push('\n');
    }

    fn append_forma<'i>(&mut self, forma: &Forma<'i>) {
        self.append_str(forma.0)
    }
}

#[cfg(test)]
mod check {
    use super::*;
}
