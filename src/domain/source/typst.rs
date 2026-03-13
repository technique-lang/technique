//! Typst serialization for source domain types.

use crate::domain::serialize::{escape_string, Markup, Render};

use super::types::{Document, Fragment};

impl Render for Document {
    fn render(&self, out: &mut Markup) {
        for fragment in &self.fragments {
            fragment.render(out);
        }
    }
}

impl Render for Fragment {
    fn render(&self, out: &mut Markup) {
        let func = match self
            .syntax
            .as_str()
        {
            "Neutral" => "render-neutral",
            "Indent" => "render-indent",
            "Newline" => "render-newline",
            "Header" => "render-header",
            "Declaration" => "render-declaration",
            "Description" => "render-description",
            "Forma" => "render-forma",
            "StepItem" => "render-stepitem",
            "CodeBlock" => "render-codeblock",
            "Variable" => "render-variable",
            "Section" => "render-section",
            "String" => "render-string",
            "Numeric" => "render-numeric",
            "Response" => "render-response",
            "Invocation" => "render-invocation",
            "Title" => "render-title",
            "Keyword" => "render-keyword",
            "Function" => "render-function",
            "Multiline" => "render-multiline",
            "Label" => "render-label",
            "Operator" => "render-operator",
            "Quote" => "render-quote",
            "Language" => "render-language",
            "Attribute" => "render-attribute",
            "Structure" => "render-structure",
            _ => "render-neutral",
        };

        if self.syntax == "Newline" {
            out.raw(&format!("#{}()\n", func));
        } else {
            out.raw(&format!(
                "#{}(\"{}\")",
                func,
                escape_string(&self.content)
            ));
        }
    }
}
