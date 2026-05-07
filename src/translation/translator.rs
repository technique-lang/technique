// Translation of the parser's abstract syntax tree into a runnable Program.

use std::collections::HashMap;

use crate::language;
use crate::language::{Document, Span};

use super::types::{
    Entry, Executable, Fragment, Invocable, Operation, Ordinal, Program, Subroutine, SubroutineId,
    SubroutineRef,
};

pub fn translate<'i>(document: &'i Document<'i>) -> Result<Program<'i>, Vec<TranslationError<'i>>> {
    let mut translator = Translator::new();

    if let Some(body) = &document.body {
        if let language::Technique::Steps(scopes) = body {
            // Top-level Steps-only document is wrapped in a synthetic
            // anonymous subroutine at index 0, so downstream code can
            // assume a uniform Vec<Subroutine>.
            let mut wrapper = Subroutine::anonymous();
            let mut ops = Vec::new();
            let mut responses = Vec::new();
            for scope in scopes {
                translator.append_attributes(&mut ops, &mut responses, scope, &[]);
            }
            wrapper.body = Operation::Sequence(ops);
            wrapper.responses = responses;
            translator
                .program
                .subroutines
                .push(wrapper);
        }
        translator.collect_technique(body);
        translator.resolve_references();
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
    /// A local procedure invocation `<name>(...)` whose `name` doesn't
    /// match any procedure declared in this document is an error.
    UnresolvedProcedure(language::Identifier<'i>),
}

impl<'i> TranslationError<'i> {
    pub fn span(&self) -> Span {
        match self {
            TranslationError::DuplicateProcedure(id) => id.span,
            TranslationError::DuplicateTitle { at, .. } => *at,
            TranslationError::InterleavedDescription { at, .. } => *at,
            TranslationError::UnresolvedProcedure(id) => id.span,
        }
    }
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

        let mut ops = Vec::new();
        let mut responses = Vec::new();
        self.translate_descriptions(&mut ops, description);
        for element in &procedure.elements {
            match element {
                language::Element::Steps(scopes, _) => {
                    for scope in scopes {
                        self.append_attributes(&mut ops, &mut responses, scope, &[]);
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
        entry.responses = responses;
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
                let mut body_ops = Vec::new();
                let mut responses = Vec::new();
                if let language::Technique::Steps(scopes) = body {
                    for sub in scopes {
                        self.append_attributes(&mut body_ops, &mut responses, sub, attrs);
                    }
                }
                Operation::Section {
                    numeral,
                    title: title.as_ref(),
                    body: Box::new(Operation::Sequence(body_ops)),
                    responses,
                }
            }
            language::Scope::DependentBlock {
                ordinal,
                description,
                subscopes,
                ..
            } => {
                let mut body_ops = Vec::new();
                let mut responses = Vec::new();
                self.translate_descriptions(&mut body_ops, description);
                for sub in subscopes {
                    self.append_attributes(&mut body_ops, &mut responses, sub, attrs);
                }
                Operation::Step {
                    ordinal: Ordinal::Dependent(ordinal),
                    attributes: attrs.to_vec(),
                    description: description.as_slice(),
                    body: Box::new(Operation::Sequence(body_ops)),
                    responses,
                }
            }
            language::Scope::ParallelBlock {
                description,
                subscopes,
                ..
            } => {
                let mut body_ops = Vec::new();
                let mut responses = Vec::new();
                self.translate_descriptions(&mut body_ops, description);
                for sub in subscopes {
                    self.append_attributes(&mut body_ops, &mut responses, sub, attrs);
                }
                Operation::Step {
                    ordinal: Ordinal::Parallel,
                    attributes: attrs.to_vec(),
                    description: description.as_slice(),
                    body: Box::new(Operation::Sequence(body_ops)),
                    responses,
                }
            }
            // AttributeBlock and ResponseBlock are intercepted by
            // append_attributes before reaching here.
            language::Scope::AttributeBlock { .. } | language::Scope::ResponseBlock { .. } => {
                unreachable!()
            }
            language::Scope::CodeBlock {
                expressions,
                subscopes,
                ..
            } => match expressions.first() {
                Some(language::Expression::Foreach(names, source, _)) => {
                    let mut body_ops = Vec::new();
                    let mut responses = Vec::new();
                    for sub in subscopes {
                        self.append_attributes(&mut body_ops, &mut responses, sub, attrs);
                    }
                    Operation::Loop {
                        names,
                        over: Some(Box::new(self.translate_expression(source))),
                        body: Box::new(Operation::Sequence(body_ops)),
                        responses,
                    }
                }
                Some(language::Expression::Repeat(_, _)) => {
                    let mut body_ops = Vec::new();
                    let mut responses = Vec::new();
                    for sub in subscopes {
                        self.append_attributes(&mut body_ops, &mut responses, sub, attrs);
                    }
                    Operation::Loop {
                        names: &[],
                        over: None,
                        body: Box::new(Operation::Sequence(body_ops)),
                        responses,
                    }
                }
                Some(other) => self.translate_expression(other),
                None => self.translate_subscopes(subscopes, attrs),
            },
        }
    }

    // Hoist executable Descriptives out of description paragraphs and
    // append them to `ops` as an "anonymous step 0" prefix.
    fn translate_descriptions(
        &mut self,
        ops: &mut Vec<Operation<'i>>,
        paragraphs: &'i [language::Paragraph<'i>],
    ) {
        for paragraph in paragraphs {
            let language::Paragraph(descriptives, _) = paragraph;
            for descriptive in descriptives {
                if let Some(op) = self.executable_from_descriptive(descriptive) {
                    ops.push(op);
                }
            }
        }
    }

    /// Plain Text is display-only and not hoisted. Every other Descriptive
    /// requires runtime evaluation: Application invokes a procedure;
    /// CodeInline evaluates the inner Expression (a Variable read needs the
    /// value before the description can be rendered, just as much as an
    /// Application does); Binding captures a result. The description
    /// Paragraph stays borrowed on the enclosing Step for the renderer to
    /// fill the CodeInline holes from these Operations' results.
    fn executable_from_descriptive(
        &mut self,
        descriptive: &'i language::Descriptive<'i>,
    ) -> Option<Operation<'i>> {
        match descriptive {
            language::Descriptive::Text(_) => None,
            language::Descriptive::CodeInline(expr) => Some(self.translate_expression(expr)),
            language::Descriptive::Application(invocation) => {
                Some(Operation::Invoke(self.translate_invocation(invocation)))
            }
            // Naked binding `text ~ var` or `<call> ~ var`.
            language::Descriptive::Binding(inner, names) => {
                let value = self
                    .executable_from_descriptive(inner)
                    .unwrap_or_else(|| Operation::Sequence(Vec::new()));
                Some(Operation::Bind {
                    names: names.as_slice(),
                    value: Box::new(value),
                })
            }
        }
    }

    // Walk a single scope, pushing translated Operations to `ops` and
    // accumulating peer responses (transparent through AttributeBlock) into
    // `responses`. AttributeBlock vanishes by inlining its subscopes;
    // ResponseBlock vanishes by extending `responses`.
    fn append_attributes(
        &mut self,
        ops: &mut Vec<Operation<'i>>,
        responses: &mut Vec<&'i language::Response<'i>>,
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
                    self.append_attributes(ops, responses, sub, &nested);
                }
            }
            language::Scope::ResponseBlock { responses: r, .. } => {
                responses.extend(r);
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
        let mut responses = Vec::new();
        for scope in scopes {
            self.append_attributes(&mut ops, &mut responses, scope, attrs);
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

    // Pass 3: walk the translated subroutines, resolving every Invoke
    // target. Local procedure references that match a name registered by
    // Pass 1 become Resolved(SubroutineId); unmatched local references
    // are errors.
    fn resolve_references(&mut self) {
        for subroutine in &mut self
            .program
            .subroutines
        {
            Self::resolve_operation(&mut subroutine.body, &self.known, &mut self.problems);
        }
    }

    fn resolve_operation(
        op: &mut Operation<'i>,
        known: &HashMap<&'i str, SubroutineId>,
        problems: &mut Vec<TranslationError<'i>>,
    ) {
        match op {
            Operation::Invoke(invocable) => {
                if let SubroutineRef::Unresolved(id) = &invocable.target {
                    match known.get(id.value) {
                        Some(sub_id) => invocable.target = SubroutineRef::Resolved(*sub_id),
                        None => problems.push(TranslationError::UnresolvedProcedure(*id)),
                    }
                }
                for arg in &mut invocable.arguments {
                    Self::resolve_operation(arg, known, problems);
                }
            }
            Operation::Sequence(ops) => {
                for op in ops {
                    Self::resolve_operation(op, known, problems);
                }
            }
            Operation::Section { body, .. } => Self::resolve_operation(body, known, problems),
            Operation::Step { body, .. } => Self::resolve_operation(body, known, problems),
            Operation::Loop { over, body, .. } => {
                if let Some(over) = over {
                    Self::resolve_operation(over, known, problems);
                }
                Self::resolve_operation(body, known, problems);
            }
            Operation::Bind { value, .. } => Self::resolve_operation(value, known, problems),
            Operation::Execute(executable) => {
                for arg in &mut executable.arguments {
                    Self::resolve_operation(arg, known, problems);
                }
            }
            Operation::String(fragments) => {
                for fragment in fragments {
                    if let Fragment::Interpolation(op) = fragment {
                        Self::resolve_operation(op, known, problems);
                    }
                }
            }
            Operation::Tablet(entries) => {
                for entry in entries {
                    Self::resolve_operation(&mut entry.value, known, problems);
                }
            }
            Operation::Variable(_) | Operation::Number(_) | Operation::Multiline(_, _) => {}
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
            language::Expression::Execution(function, _) => Operation::Execute(Executable {
                target: function.target,
                arguments: function
                    .parameters
                    .iter()
                    .map(|expr| self.translate_expression(expr))
                    .collect(),
            }),
            language::Expression::Repeat(body, _) => Operation::Loop {
                names: &[],
                over: None,
                body: Box::new(self.translate_expression(body)),
                responses: Vec::new(),
            },
            // Standalone Foreach has no body in the AST; the body is supplied
            // by the enclosing CodeBlock's subscopes when one is present (see
            // CodeBlock translation). As a bare expression we emit a Loop
            // with an empty Sequence body.
            language::Expression::Foreach(names, source, _) => Operation::Loop {
                names,
                over: Some(Box::new(self.translate_expression(source))),
                body: Box::new(Operation::Sequence(Vec::new())),
                responses: Vec::new(),
            },
            language::Expression::Binding(value, names, _) => Operation::Bind {
                names,
                value: Box::new(self.translate_expression(value)),
            },
            language::Expression::Separator => Operation::Sequence(Vec::new()),
        }
    }

    fn translate_invocation(&mut self, invocation: &'i language::Invocation<'i>) -> Invocable<'i> {
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
        Invocable { target, arguments }
    }
}
