use technique::language::*;

pub fn render(document: &Document) -> String {
    let mut output = Renderer::new();

    if let Some(metadata) = &document.header {
        output.render_metadata(metadata);
    }
    
    output.buffer
}

struct Renderer {
    buffer: String,
}

impl Renderer {
    pub fn new() -> Self {
        Self {
            // TODO calculate an appropriate size, multiples of page
            buffer: String::new(),
        }
    }

    fn render_metadata(&mut self, _metadata: &Metadata) {
        
    }
}
