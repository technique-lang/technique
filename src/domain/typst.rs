//! Typst data literal builder.
//!
//! Domain types serialize themselves as Typst dictionary literals using the
//! `Data` builder. The `Render` trait is implemented by types that emit
//! themselves as complete dictionary entries; `Field` is for individual
//! key-value pairs within a dictionary.

const INDENT: &str = "    ";

/// Stateful builder for accumulating Typst data literals.
pub struct Data {
    out: String,
    depth: usize,
}

impl Data {
    pub fn new() -> Self {
        Data {
            out: String::new(),
            depth: 0,
        }
    }

    /// Consume the builder and return the data as a `#let technique = ...`
    /// binding. The trailing comma from `close()` is stripped so the
    /// top-level assignment is valid Typst.
    pub fn finish(self) -> String {
        let out = self
            .out
            .trim_end()
            .trim_end_matches(',');
        format!("#let technique = {}\n", out)
    }

    fn pad(&mut self) {
        for _ in 0..self.depth {
            self.out
                .push_str(INDENT);
        }
    }

    /// Open a dictionary: `(`, newline, and increase depth.
    pub fn open(&mut self) {
        self.pad();
        self.out
            .push_str("(\n");
        self.depth += 1;
    }

    /// Close a dictionary: decrease depth, closing `),` and newline.
    pub fn close(&mut self) {
        self.depth -= 1;
        self.pad();
        self.out
            .push_str("),\n");
    }

    /// Emit a `type: "name",` discriminator field and a newline.
    pub fn tag(&mut self, name: &str) {
        self.pad();
        self.out
            .push_str(&format!("type: \"{}\",\n", name));
    }

    /// Emit a field whose value implements `Field`.
    pub fn field(&mut self, key: &str, value: &(impl Field + ?Sized)) {
        value.emit(self, key);
    }

    /// Emit a list field, calling `Render::render` on each item.
    pub fn list<T: Render>(&mut self, key: &str, items: &[T]) {
        self.pad();
        self.out
            .push_str(&format!("{}: (\n", key));
        self.depth += 1;
        for item in items {
            item.render(self);
        }
        self.depth -= 1;
        self.pad();
        self.out
            .push_str("),\n");
    }
}

/// Emit a domain type as a Typst data literal.
pub trait Render {
    fn render(&self, data: &mut Data);
}

impl Render for String {
    fn render(&self, data: &mut Data) {
        data.pad();
        data.out
            .push_str(&format!("\"{}\",\n", escape_string(self)));
    }
}

/// Any type that knows how to emit itself as a `key: value,` pair in a Typst
/// dictionary should implement Field, which can then be used by the Data
/// builder's field() method.
pub trait Field {
    fn emit(&self, data: &mut Data, key: &str);
}

impl Field for str {
    fn emit(&self, data: &mut Data, key: &str) {
        data.pad();
        data.out
            .push_str(&format!("{}: \"{}\",\n", key, escape_string(self)));
    }
}

impl Field for String {
    fn emit(&self, data: &mut Data, key: &str) {
        self.as_str()
            .emit(data, key);
    }
}

impl Field for Option<String> {
    fn emit(&self, data: &mut Data, key: &str) {
        data.pad();
        match self {
            Some(v) => data
                .out
                .push_str(&format!("{}: \"{}\",\n", key, escape_string(v))),
            None => data
                .out
                .push_str(&format!("{}: none,\n", key)),
        }
    }
}

/// Escape `\` and `"` for Typst string literals.
pub fn escape_string(s: &str) -> String {
    s.replace('\\', "\\\\")
        .replace('"', "\\\"")
}

#[cfg(test)]
mod check {
    use super::*;

    #[test]
    fn escape_string_backslash_and_quote() {
        assert_eq!(escape_string(r#"a "b" c\d"#), r#"a \"b\" c\\d"#);
    }

    #[test]
    fn field_some() {
        let mut d = Data::new();
        d.depth = 1;
        d.field("title", &Some("Hello".into()));
        assert_eq!(d.out, "    title: \"Hello\",\n");
    }

    #[test]
    fn field_none() {
        let mut d = Data::new();
        d.depth = 1;
        d.field("title", &None::<String>);
        assert_eq!(d.out, "    title: none,\n");
    }

    #[test]
    fn open_close_tracks_depth() {
        let mut d = Data::new();
        d.open();
        assert_eq!(d.depth, 1);
        d.close();
        assert_eq!(d.depth, 0);
        assert_eq!(d.out, "(\n),\n");
    }

    #[test]
    fn nested_dict() {
        let mut d = Data::new();
        d.open();
        d.field("name", "outer");
        d.open();
        d.field("name", "inner");
        d.close();
        d.close();
        assert!(d
            .out
            .contains("    name: \"outer\",\n"));
        assert!(d
            .out
            .contains("        name: \"inner\",\n"));
    }
}
