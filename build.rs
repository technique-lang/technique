fn main() {
    let mut config = cc::Build::new();

    // Compile the generated C parser
    config.file("src/parser.c");

    // Also include the directory with the C headers
    config.include("src");

    config.compile("tree-sitter-technique");

    // Rerun the build script if the C source changes
    println!("cargo:rerun-if-changed=src/parser.c");
}
