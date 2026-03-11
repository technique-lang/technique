//! Rendering of Technique source code with syntax highlighting

mod renderer;
mod terminal;
mod typst;

pub use renderer::render;
pub use terminal::Terminal;
pub use typst::Typst;
