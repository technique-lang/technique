#[macro_export]
macro_rules! compile {
    ($pattern:expr) => {{
        use std::sync::OnceLock;
        static REGEX: OnceLock<regex::Regex> = OnceLock::new();
        REGEX.get_or_init(|| regex::Regex::new($pattern).unwrap_or_else(|e| panic!("{}", e)))
    }};
}
