// Translation of the parser's abstract syntax tree into a runnable Program.

use std::collections::HashMap;

use crate::language;
use crate::language::{Document, Span};

use super::types::{
    Entry, Fragment, Invoke, Operation, Ordinal, Program, Subroutine, SubroutineId, SubroutineRef,
};

pub fn translate<'i>(document: &'i Document<'i>) -> Result<Program<'i>, Vec<TranslationError<'i>>> {
    let mut translator = Translator::new();

    if let Some(body) = &document.body {
        if let language::Technique::Steps(scopes) = body {
            // Top-level Steps-only document is wrapped in a synthetic
            // anonymous subroutine at index 0, so downstream code can
            // assume a uniform Vec<Subroutine>.
            let mut wrapper = Subroutine::anonymous();
            wrapper.body = translator.translate_subscopes(scopes, &[]);
            translator
                .program
                .subroutines
                .push(wrapper);
        }
        translator.collect_technique(body);
    }

    if translator
        .problems
        .is_empty()
    {
        Ok(translator.program)
    } else {
        Err(translator.problems)
    }
}

#[derive(Debug, Eq, PartialEq)]
pub enum TranslationError<'i> {
    DuplicateProcedure(language::Identifier<'i>),
    DuplicateTitle {
        procedure: language::Identifier<'i>,
        at: Span,
    },
    InterleavedDescription {
        procedure: language::Identifier<'i>,
        at: Span,
    },
    OrphanResponse(Span),
}

struct Translator<'i> {
    program: Program<'i>,
    problems: Vec<TranslationError<'i>>,
    known: HashMap<&'i str, SubroutineId>,
}

impl<'i> Translator<'i> {
    fn new() -> Self {
        Translator {
            program: Program::new(),
            problems: Vec::new(),
            known: HashMap::new(),
        }
    }

    // Walk a Technique node, registering any procedures it declares directly
    // or transitively through nested sections. Procedures are hoisted into
    // the flat Program.subroutines list regardless of where in the section
    // tree they were declared. The top-level synthetic anonymous wrapper
    // (for a Technique::Steps-only document) is added by translate(), not
    // here.
    fn collect_technique(&mut self, technique: &'i language::Technique<'i>) {
        match technique {
            language::Technique::Procedures(procedures) => {
                for procedure in procedures {
                    if let Some(id) = self.register_procedure(procedure) {
                        self.translate_procedure(id, procedure);
                    }

                    // Element::Steps scopes may contain Sections, whose
                    // bodies can in turn declare further procedures. Walk
                    // the procedure's scopes so those nested declarations
                    // are discovered; the walk is independent of whether
                    // this procedure itself was a duplicate.
                    for element in &procedure.elements {
                        if let language::Element::Steps(scopes, _) = element {
                            for scope in scopes {
                                self.collect_scope(scope);
                            }
                        }
                    }
                }
            }
            language::Technique::Steps(scopes) => {
                for scope in scopes {
                    self.collect_scope(scope);
                }
            }
            language::Technique::Empty => {}
        }
    }

    // Pass 1: gather a procedure's name and reserve its slot in
    // Program.subroutines.
    fn register_procedure(
        &mut self,
        procedure: &'i language::Procedure<'i>,
    ) -> Option<SubroutineId> {
        let name = procedure
            .name
            .value;

        if self
            .known
            .contains_key(name)
        {
            self.problems
                .push(TranslationError::DuplicateProcedure(procedure.name));
            return None;
        }

        let id = SubroutineId(
            self.program
                .subroutines
                .len(),
        );
        self.known
            .insert(name, id);
        self.program
            .subroutines
            .push(Subroutine::new(procedure.name));
        Some(id)
    }

    // Pass 2: populate a registered subroutine with its title, description,
    // parameters, signature, and body.
    fn translate_procedure(&mut self, id: SubroutineId, procedure: &'i language::Procedure<'i>) {
        let (title, description) = self.extract_procedure_elements(procedure);

        // build body
        let mut ops = Vec::new();
        for element in &procedure.elements {
            match element {
                language::Element::Steps(scopes, _) => {
                    for scope in scopes {
                        self.append_attributes(&mut ops, scope, &[]);
                    }
                }
                language::Element::CodeBlock(expressions, _) => {
                    for expression in expressions {
                        ops.push(self.translate_expression(expression));
                    }
                }
                _ => {}
            }
        }
        let body = Operation::Sequence(ops);

        let entry = &mut self
            .program
            .subroutines[id.0];
        entry.title = title;
        entry.description = description;
        entry.parameters = procedure
            .parameters
            .as_ref()
            .map(Vec::as_slice);
        entry.signature = procedure
            .signature
            .as_ref();
        entry.body = body;
    }

    fn translate_scope(
        &mut self,
        scope: &'i language::Scope<'i>,
        attrs: &[&'i [language::Attribute<'i>]],
    ) -> Operation<'i> {
        match scope {
            language::Scope::SectionChunk {
                numeral,
                title,
                body,
                ..
            } => {
                let inner = match body {
                    language::Technique::Steps(scopes) => self.translate_subscopes(scopes, attrs),
                    // Procedures declared inside a section are hoisted into
                    // Program.subroutines by the collect pass; the section
                    // node carries no executable body for them.
                    language::Technique::Procedures(_) => Operation::Sequence(Vec::new()),
                    language::Technique::Empty => Operation::Sequence(Vec::new()),
                };
                Operation::Section {
                    numeral,
                    title: title.as_ref(),
                    body: Box::new(inner),
                }
            }
            language::Scope::DependentBlock {
                ordinal,
                description,
                subscopes,
                ..
            } => {
                let (body, expects) = self.translate_step_subscopes(subscopes, attrs);
                Operation::Step {
                    ordinal: Ordinal::Dependent(ordinal),
                    attributes: attrs.to_vec(),
                    description: description.as_slice(),
                    body: Box::new(body),
                    expects,
                }
            }
            language::Scope::ParallelBlock {
                description,
                subscopes,
                ..
            } => {
                let (body, expects) = self.translate_step_subscopes(subscopes, attrs);
                Operation::Step {
                    ordinal: Ordinal::Parallel,
                    attributes: attrs.to_vec(),
                    description: description.as_slice(),
                    body: Box::new(body),
                    expects,
                }
            }
            // AttributeBlock and ResponseBlock are intercepted by
            // append_attributes before reaching here: AttributeBlock
            // contributes its attribute list to enclosed Steps, and
            // ResponseBlock either attaches to its parent Step's `expects`
            // (handled by translate_step_subscopes) or is reported as an
            // orphan.
            language::Scope::AttributeBlock { .. } | language::Scope::ResponseBlock { .. } => {
                unreachable!()
            }
            language::Scope::CodeBlock { .. } => todo!("code block"),
        }
    }

    // Walk a step's subscopes, splitting ResponseBlock(s) off for the step's
    // `expects` field while emitting the remaining scopes as the step's
    // body. If multiple ResponseBlock subscopes appear, the first wins.
    fn translate_step_subscopes(
        &mut self,
        subscopes: &'i [language::Scope<'i>],
        attrs: &[&'i [language::Attribute<'i>]],
    ) -> (Operation<'i>, Option<&'i [language::Response<'i>]>) {
        let mut expects: Option<&'i [language::Response<'i>]> = None;
        let mut ops = Vec::new();
        for sub in subscopes {
            if let language::Scope::ResponseBlock { responses, .. } = sub {
                if expects.is_none() {
                    expects = Some(responses.as_slice());
                }
            } else {
                self.append_attributes(&mut ops, sub, attrs);
            }
        }
        (Operation::Sequence(ops), expects)
    }

    // Translate a single scope into operations appended to `ops`. Most
    // scopes contribute exactly one Operation; AttributeBlock contributes
    // no Operation of its own and inlines its (recursively-translated)
    // children with its attribute list pushed onto the enclosing-attribute
    // stack. ResponseBlock reaching here has no parent step and is reported
    // as an orphan.
    fn append_attributes(
        &mut self,
        ops: &mut Vec<Operation<'i>>,
        scope: &'i language::Scope<'i>,
        attrs: &[&'i [language::Attribute<'i>]],
    ) {
        match scope {
            language::Scope::AttributeBlock {
                attributes,
                subscopes,
                ..
            } => {
                let mut nested: Vec<&'i [language::Attribute<'i>]> = attrs.to_vec();
                nested.push(attributes.as_slice());
                for sub in subscopes {
                    self.append_attributes(ops, sub, &nested);
                }
            }
            language::Scope::ResponseBlock { span, .. } => {
                self.problems
                    .push(TranslationError::OrphanResponse(*span));
            }
            _ => ops.push(self.translate_scope(scope, attrs)),
        }
    }

    fn translate_subscopes(
        &mut self,
        scopes: &'i [language::Scope<'i>],
        attrs: &[&'i [language::Attribute<'i>]],
    ) -> Operation<'i> {
        let mut ops = Vec::new();
        for scope in scopes {
            self.append_attributes(&mut ops, scope, attrs);
        }
        Operation::Sequence(ops)
    }

    // Walk a procedure's elements to extract the procedure-shell title and
    // description, surfacing the two structural errors:
    //   DuplicateTitle - a second Element::Title in this procedure.
    //   InterleavedDescription - an Element::Description after a Steps or
    //     CodeBlock element.
    // Multiple Element::Description occurrences before any Steps/CodeBlock
    // are not an error; the first wins.
    fn extract_procedure_elements(
        &mut self,
        procedure: &'i language::Procedure<'i>,
    ) -> (Option<&'i str>, &'i [language::Paragraph<'i>]) {
        let mut title: Option<&'i str> = None;
        let mut description: Option<&'i [language::Paragraph<'i>]> = None;
        let mut blocked = false;

        for element in &procedure.elements {
            match element {
                language::Element::Title(value, span) => {
                    if title.is_some() {
                        self.problems
                            .push(TranslationError::DuplicateTitle {
                                procedure: procedure.name,
                                at: *span,
                            });
                    } else {
                        title = Some(*value);
                    }
                }
                language::Element::Description(paragraphs, span) => {
                    if blocked {
                        self.problems
                            .push(TranslationError::InterleavedDescription {
                                procedure: procedure.name,
                                at: *span,
                            });
                    } else if description.is_none() {
                        description = Some(paragraphs.as_slice());
                    }
                }
                language::Element::Steps(_, _) | language::Element::CodeBlock(_, _) => {
                    blocked = true;
                }
            }
        }

        (title, description.unwrap_or(&[]))
    }

    fn collect_scope(&mut self, scope: &'i language::Scope<'i>) {
        match scope {
            language::Scope::SectionChunk { body, .. } => {
                self.collect_technique(body);
            }
            language::Scope::DependentBlock { subscopes, .. }
            | language::Scope::ParallelBlock { subscopes, .. }
            | language::Scope::AttributeBlock { subscopes, .. }
            | language::Scope::CodeBlock { subscopes, .. } => {
                for sub in subscopes {
                    self.collect_scope(sub);
                }
            }
            language::Scope::ResponseBlock { .. } => {}
        }
    }

    fn translate_expression(&mut self, expression: &'i language::Expression<'i>) -> Operation<'i> {
        match expression {
            language::Expression::Variable(id, _) => Operation::Variable(*id),
            language::Expression::Number(numeric, _) => Operation::Number(*numeric),
            language::Expression::String(pieces, _) => {
                let fragments = pieces
                    .iter()
                    .map(|piece| match piece {
                        language::Piece::Text(text) => Fragment::Text(text),
                        language::Piece::Interpolation(expr) => {
                            Fragment::Interpolation(self.translate_expression(expr))
                        }
                    })
                    .collect();
                Operation::String(fragments)
            }
            language::Expression::Multiline(lang, lines, _) => {
                Operation::Multiline(*lang, lines.clone())
            }
            language::Expression::Tablet(pairs, _) => {
                let entries = pairs
                    .iter()
                    .map(|pair| Entry {
                        label: pair.label,
                        value: self.translate_expression(&pair.value),
                    })
                    .collect();
                Operation::Tablet(entries)
            }
            language::Expression::Application(invocation, _) => {
                Operation::Invoke(self.translate_invocation(invocation))
            }
            language::Expression::Execution(function, _) => Operation::Execution {
                target: function.target,
                arguments: function
                    .parameters
                    .iter()
                    .map(|expr| self.translate_expression(expr))
                    .collect(),
            },
            language::Expression::Repeat(body, _) => Operation::Loop {
                names: &[],
                over: None,
                body: Box::new(self.translate_expression(body)),
            },
            // Standalone Foreach has no body in the AST; the body is supplied
            // by the enclosing CodeBlock's subscopes when one is present (see
            // CodeBlock translation). As a bare expression we emit a Loop
            // with an empty Sequence body.
            language::Expression::Foreach(names, source, _) => Operation::Loop {
                names,
                over: Some(Box::new(self.translate_expression(source))),
                body: Box::new(Operation::Sequence(Vec::new())),
            },
            language::Expression::Binding(value, names, _) => Operation::Bind {
                names,
                value: Box::new(self.translate_expression(value)),
            },
            language::Expression::Separator => Operation::Sequence(Vec::new()),
        }
    }

    fn translate_invocation(&mut self, invocation: &'i language::Invocation<'i>) -> Invoke<'i> {
        let target = match &invocation.target {
            language::Target::Local(id) => SubroutineRef::Unresolved(*id),
            language::Target::Remote(_) => todo!("remote invocation target"),
        };
        let arguments = match &invocation.parameters {
            Some(params) => params
                .iter()
                .map(|expr| self.translate_expression(expr))
                .collect(),
            None => Vec::new(),
        };
        Invoke { target, arguments }
    }
}
