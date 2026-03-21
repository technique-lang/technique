//! Typst serialization builders.
//!
//! The `Markup` builder emits Typst function calls — `#render-step(ordinal:
//! "1", ...)` — so that the tree-walk happens in Rust and the `.typ` file
//! shrinks to a set of thin, overridable formatting functions. The `Render`
//! trait is implemented by domain types that serialize themselves via
//! `Markup`.

const INDENT: &str = "    ";

/// Escape `\` and `"` for Typst string literals.
pub fn escape_string(s: &str) -> String {
    s.replace('\\', "\\\\")
        .replace('"', "\\\"")
}

/// Stateful builder for accumulating Typst function-call markup.
pub struct Markup {
    out: String,
    depth: usize,
}

impl Markup {
    pub fn new() -> Self {
        Markup {
            out: String::new(),
            depth: 0,
        }
    }

    pub fn finish(self) -> String {
        self.out
    }

    fn pad(&mut self) {
        for _ in 0..self.depth {
            self.out
                .push_str(INDENT);
        }
    }

    /// Begin a function call: `#name(`.
    pub fn call(&mut self, name: &str) {
        self.pad();
        self.out
            .push_str(&format!("#{}(", name));
    }

    /// Emit a string parameter: `key: "escaped-value", `.
    pub fn param(&mut self, key: &str, value: &str) {
        self.out
            .push_str(&format!("{}: \"{}\", ", key, escape_string(value)));
    }

    /// Emit an optional string parameter: `key: "value", ` or `key: none, `.
    pub fn param_opt(&mut self, key: &str, value: &Option<String>) {
        match value {
            Some(v) => self
                .out
                .push_str(&format!("{}: \"{}\", ", key, escape_string(v))),
            None => self
                .out
                .push_str(&format!("{}: none, ", key)),
        }
    }

    /// Emit a tuple of strings: `key: ("a", "b"), `.
    pub fn param_list(&mut self, key: &str, items: &[String]) {
        self.out
            .push_str(&format!("{}: (", key));
        for item in items {
            self.out
                .push_str(&format!("\"{}\", ", escape_string(item)));
        }
        self.out
            .push_str("), ");
    }

    /// Open a content block parameter: `key: [\n` + indent.
    pub fn content_open(&mut self, key: &str) {
        self.out
            .push_str(&format!("{}: [\n", key));
        self.depth += 1;
    }

    /// Close a content block: dedent + `], `.
    pub fn content_close(&mut self) {
        self.depth -= 1;
        self.pad();
        self.out
            .push_str("], ");
    }

    /// Emit raw Typst content (for inline data like outline tuples).
    pub fn raw(&mut self, s: &str) {
        self.out
            .push_str(s);
    }

    /// Close a function call: `)\n`.
    pub fn close(&mut self) {
        self.out
            .push_str(")\n");
    }
}

/// Emit a list of prose paragraphs as Typst content blocks.
pub fn render_prose_list(out: &mut Markup, key: &str, items: &[super::engine::Prose]) {
    out.raw(&format!("{}: (", key));
    for item in items {
        out.raw("[");
        for fragment in &item.0 {
            match fragment {
                super::engine::Inline::Text(s) => out.raw(&format!("#\"{}\"", escape_string(s))),
                super::engine::Inline::Emphasis(s) => {
                    out.raw(&format!("#emph(\"{}\")", escape_string(s)))
                }
                super::engine::Inline::Strong(s) => {
                    out.raw(&format!("#strong(\"{}\")", escape_string(s)))
                }
                super::engine::Inline::Code(s) => {
                    out.raw(&format!("#raw(\"{}\")", escape_string(s)))
                }
            }
        }
        out.raw("], ");
    }
    out.raw("), ");
}

/// Render a domain type as Typst function-call markup.
pub trait Render {
    fn render(&self, out: &mut Markup);
}

#[cfg(test)]
mod check {
    use super::*;

    #[test]
    fn escape_string_backslash_and_quote() {
        assert_eq!(escape_string(r#"a "b" c\d"#), r#"a \"b\" c\\d"#);
    }

    #[test]
    fn markup_simple_call() {
        let mut m = Markup::new();
        m.call("render-step");
        m.param("ordinal", "1");
        m.close();
        assert_eq!(m.finish(), "#render-step(ordinal: \"1\", )\n");
    }

    #[test]
    fn markup_param_opt_some_and_none() {
        let mut m = Markup::new();
        m.call("f");
        m.param_opt("a", &Some("yes".into()));
        m.param_opt("b", &None);
        m.close();
        assert_eq!(m.finish(), "#f(a: \"yes\", b: none, )\n");
    }

    #[test]
    fn markup_param_list() {
        let mut m = Markup::new();
        m.call("f");
        m.param_list("items", &["x".into(), "y".into()]);
        m.close();
        assert_eq!(m.finish(), "#f(items: (\"x\", \"y\", ), )\n");
    }

    #[test]
    fn markup_content_block() {
        let mut m = Markup::new();
        m.call("render-section");
        m.param("heading", "Prep");
        m.content_open("children");
        m.call("render-step");
        m.param("ordinal", "1");
        m.close();
        m.content_close();
        m.close();
        let result = m.finish();
        assert!(result.contains("children: [\n"));
        assert!(result.contains("    #render-step("));
        assert!(result.contains("], )\n"));
    }

    #[test]
    fn markup_escapes_strings() {
        let mut m = Markup::new();
        m.call("f");
        m.param("t", r#"say "hello""#);
        m.close();
        assert_eq!(m.finish(), "#f(t: \"say \\\"hello\\\"\", )\n");
    }
}
