//! Code formatter for the Technique language

use technique::language::*;

pub fn format<'i>(technique: &Technique) -> String {
    let mut buffer = String::new();

    if let Some(metadata) = &technique.header {
        buffer.push_str("% technique v1\n");

        if let Some(license) = metadata.license {
            buffer.push_str("! ");
            buffer.push_str(license);

            if let Some(copyright) = metadata.copyright {
                buffer.push_str("; © ");
                buffer.push_str(copyright);
            }

            buffer.push('\n');
        }

        if let Some(template) = metadata.template {
            buffer.push_str("& ");
            buffer.push_str(template);
            buffer.push('\n');
        }
    }

    if let Some(procedures) = &technique.body {
        for procedure in procedures {
            // if a header or another procedure has already been added,
            // separate the upcoming one with a blank line.
            if buffer.len() > 0 {
                buffer.push('\n');
            }
            let name = &procedure.name;
            buffer.push_str(name.0);

            buffer.push_str(" : ");

            if let Some(signature) = &procedure.signature {
                buffer.push_str(format_genus(&signature.domain));
                buffer.push_str(" -> ");
                buffer.push_str(format_genus(&signature.range));
            }

            buffer.push('\n');
        }
    }

    buffer
}

#[cfg(test)]
mod check {
    use super::*;
}
