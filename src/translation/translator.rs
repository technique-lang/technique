// Translation of the parser's abstract syntax tree into a runnable Program.

use std::collections::HashMap;

use crate::language;
use crate::language::{Document, Span};

use crate::program::{
    Entry, Executable, ExecutableRef, Fragment, Invocable, Locale, Operation, Ordinal, Program,
    Subroutine, SubroutineId, SubroutineRef,
};

pub fn translate<'i>(document: &'i Document<'i>) -> Result<Program<'i>, Vec<TranslationError<'i>>> {
    let mut translator = Translator::new();
    translator
        .program
        .prelude = document
        .header
        .as_ref();

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

// Whether a section body already holds a procedure descent: an Invoke
// hoisted from an explicit `<name>` in the section heading.
fn descends(ops: &[Operation<'_>]) -> bool {
    ops.iter()
        .any(|op| {
            if let Operation::Invoke(_) = op {
                true
            } else {
                false
            }
        })
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
    /// A procedure invocation `<name>(...)` whose argument count doesn't match
    /// the arity of the procedure it resolves to.
    ProcedureArityMismatch {
        procedure: language::Identifier<'i>,
        expected: usize,
        actual: usize,
    },
    /// A procedure declaring both a parameter list and a signature whose
    /// required inputs disagree in count. The names must correspond one-to-one
    /// with the signature's inputs.
    SignatureParameterMismatch {
        procedure: language::Identifier<'i>,
        parameters: usize,
        requires: usize,
    },
    /// Binding the result of a `repeat` to a variable is an error; the
    /// `repeat` keyword does not terminate naturally and does not produces a
    /// value so cannot be bound. Note: we could reconsider this in the fugure
    /// if we implement a `break` or `return` keyword.
    BoundRepeat {
        at: Span,
    },
    /// A list mixing labelled pairs (`"label" = value`) with bare values is
    /// neither a tablet nor a plain list. The two forms can't be combined in
    /// one set of brackets.
    HeterogenousList {
        at: Span,
    },
}

impl<'i> TranslationError<'i> {
    pub fn span(&self) -> Span {
        match self {
            TranslationError::DuplicateProcedure(id) => id.span,
            TranslationError::DuplicateTitle { at, .. } => *at,
            TranslationError::InterleavedDescription { at, .. } => *at,
            TranslationError::UnresolvedProcedure(id) => id.span,
            TranslationError::ProcedureArityMismatch { procedure, .. } => procedure.span,
            TranslationError::SignatureParameterMismatch { procedure, .. } => procedure.span,
            TranslationError::BoundRepeat { at } => *at,
            TranslationError::HeterogenousList { at } => *at,
        }
    }
}

struct Translator<'i> {
    program: Program<'i>,
    problems: Vec<TranslationError<'i>>,
    known: HashMap<&'i str, SubroutineId>,
    locus: Vec<Locale<'i>>,
}

impl<'i> Translator<'i> {
    fn new() -> Self {
        Translator {
            program: Program::new(),
            problems: Vec::new(),
            known: HashMap::new(),
            locus: Vec::new(),
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
                    // bodies can in turn declare further procedures. Walk the
                    // procedure's scopes so those nested declarations are
                    // discovered, with this procedure on the lexical prefix so
                    // their addresses descend from it.
                    self.locus
                        .push(Locale::Procedure(
                            procedure
                                .name
                                .value,
                        ));
                    for element in &procedure.elements {
                        if let language::Element::Steps(scopes, _) = element {
                            for scope in scopes {
                                self.collect_scope(scope);
                            }
                        }
                    }
                    self.locus
                        .pop();
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
        let mut subroutine = Subroutine::new(procedure.name);
        subroutine.locale = self
            .locus
            .iter()
            .cloned()
            .chain(std::iter::once(Locale::Procedure(name)))
            .collect();
        self.program
            .subroutines
            .push(subroutine);
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
                language::Element::CodeBlock(expressions, subscopes, _) => {
                    match self.translate_loop_block(expressions, subscopes, &[]) {
                        Some(loop_op) => ops.push(loop_op),
                        None => {
                            for expression in expressions {
                                ops.push(self.translate_expression(expression));
                            }
                            for sub in subscopes {
                                self.append_attributes(&mut ops, &mut responses, sub, &[]);
                            }
                        }
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

        if let (Some(parameters), Some(signature)) = (&procedure.parameters, &procedure.signature) {
            let requires = signature
                .requires
                .cardinality();
            if parameters.len() != requires {
                self.problems
                    .push(TranslationError::SignatureParameterMismatch {
                        procedure: procedure.name,
                        parameters: parameters.len(),
                        requires,
                    });
            }
        }
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

                if let Some(paragraph) = title {
                    // scan for operations in section content
                    self.translate_descriptions(&mut body_ops, std::slice::from_ref(paragraph));
                }
                match body {
                    language::Technique::Steps(scopes) => {
                        for sub in scopes {
                            self.append_attributes(&mut body_ops, &mut responses, sub, attrs);
                        }
                    }
                    language::Technique::Procedures(procedures) => {
                        // A section descends into the first procedure its body
                        // declares, its entry point. A heading that already
                        // invokes one explicitly (a 
                        // 
                        // II. Do it now <thing>
                        // 
                        // in the title) has hoisted that invoke above and is
                        // the descent already, pre-empting this one so the
                        // procedure isn't run twice.
                        if let Some(procedure) = procedures
                            .first()
                            .filter(|_| !descends(&body_ops))
                        {
                            body_ops.push(Operation::Invoke(Invocable {
                                target: SubroutineRef::Unresolved(procedure.name),
                                arguments: Vec::new(),
                                elided: true,
                            }));
                        }
                    }
                    language::Technique::Empty => {}
                }
                let title_op = title
                    .as_ref()
                    .map(|p| Box::new(self.translate_paragraph(p)));
                Operation::Section {
                    numeral,
                    title: title_op,
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
                    source: scope,
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
                    source: scope,
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
            } => match self.translate_loop_block(expressions, subscopes, attrs) {
                Some(loop_op) => loop_op,
                None => {
                    let mut ops = Vec::new();
                    for expression in expressions {
                        ops.push(self.translate_expression(expression));
                    }
                    let mut responses = Vec::new();
                    for sub in subscopes {
                        self.append_attributes(&mut ops, &mut responses, sub, attrs);
                    }
                    Operation::Sequence(ops)
                }
            },
        }
    }

    // Build the `Loop` for a foreach or repeat code block, its subscopes
    // forming the body. Returns None for any other code block, which the
    // caller translates as a plain sequence of its expressions.
    fn translate_loop_block(
        &mut self,
        expressions: &'i [language::Expression<'i>],
        subscopes: &'i [language::Scope<'i>],
        attrs: &[&'i [language::Attribute<'i>]],
    ) -> Option<Operation<'i>> {
        if expressions.len() != 1 {
            return None;
        }
        match &expressions[0] {
            language::Expression::Foreach(names, source, _) => {
                let mut body_ops = Vec::new();
                let mut responses = Vec::new();
                for sub in subscopes {
                    self.append_attributes(&mut body_ops, &mut responses, sub, attrs);
                }
                Some(Operation::Loop {
                    names,
                    over: Some(Box::new(self.translate_expression(source))),
                    body: Box::new(Operation::Sequence(body_ops)),
                    responses,
                })
            }
            language::Expression::Repeat(inner, _) => {
                // `repeat <thing>` does `<thing>` over and over: the inline
                // expression is the loop body. Any subscopes follow it.
                let mut body_ops = vec![self.translate_expression(inner)];
                let mut responses = Vec::new();
                for sub in subscopes {
                    self.append_attributes(&mut body_ops, &mut responses, sub, attrs);
                }
                Some(Operation::Loop {
                    names: &[],
                    over: None,
                    body: Box::new(Operation::Sequence(body_ops)),
                    responses,
                })
            }
            _ => None,
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

    // Descriptive paragraphs are whitespace-agnostic and re-wrappable: the
    // parser strips edge whitespace from each Text fragment, so the original
    // single-space joins between adjacent tokens are lost. Reinsert one
    // separator space between each pair of fragments here so the runtime
    // renderer sees the canonical form the formatter would emit. Expression
    // string literals translated elsewhere preserve whitespace verbatim and
    // are not affected by this rule.
    fn translate_paragraph(&mut self, paragraph: &'i language::Paragraph<'i>) -> Operation<'i> {
        let language::Paragraph(descriptives, _) = paragraph;
        let mut fragments = Vec::with_capacity(
            descriptives
                .len()
                .saturating_mul(2),
        );
        for descriptive in descriptives {
            if let Some(fragment) = self.fragment_from_descriptive(descriptive) {
                if !fragments.is_empty() {
                    fragments.push(Fragment::Text(" "));
                }
                fragments.push(fragment);
            }
        }
        Operation::String(fragments)
    }

    // The display fragment for a descriptive, or `None` when it renders no
    // text. Only a value read or literal shows inline; everything executable
    // (a bare invocation, inline `exec`/`repeat`/`foreach`, a binding) is
    // hoisted into the enclosing body (a step's body, or the procedure's
    // step-0 prefix) and contributes no fragment here.
    fn fragment_from_descriptive(
        &mut self,
        descriptive: &'i language::Descriptive<'i>,
    ) -> Option<Fragment<'i>> {
        match descriptive {
            language::Descriptive::Text(text) => Some(Fragment::Text(text)),
            language::Descriptive::CodeInline(expr) => match expr {
                language::Expression::Variable(..)
                | language::Expression::Number(..)
                | language::Expression::String(..)
                | language::Expression::Multiline(..)
                | language::Expression::Pair(..)
                | language::Expression::List(..) => {
                    Some(Fragment::Interpolation(self.translate_expression(expr)))
                }
                language::Expression::Repeat(..)
                | language::Expression::Foreach(..)
                | language::Expression::Application(..)
                | language::Expression::Execution(..)
                | language::Expression::Binding(..)
                | language::Expression::Hole(..)
                | language::Expression::Separator => None,
            },
            language::Descriptive::Application(_) => None,
            language::Descriptive::Binding(inner, _) => self.fragment_from_descriptive(inner),
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

    // Walk a procedure's elements to extract the procedure-shell title and
    // description, surfacing the two structural errors:
    // - DuplicateTitle, if a second title appears in this procedure; and
    // - InterleavedDescription, as a procedure allows at most one
    //   description, and it must appear before any body element.
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
                    if blocked || description.is_some() {
                        self.problems
                            .push(TranslationError::InterleavedDescription {
                                procedure: procedure.name,
                                at: *span,
                            });
                    } else {
                        description = Some(paragraphs.as_slice());
                    }
                }
                language::Element::Steps(_, _) | language::Element::CodeBlock(..) => {
                    blocked = true;
                }
            }
        }

        (title, description.unwrap_or(&[]))
    }

    fn collect_scope(&mut self, scope: &'i language::Scope<'i>) {
        match scope {
            language::Scope::SectionChunk { numeral, body, .. } => {
                self.locus
                    .push(Locale::Section(numeral));
                self.collect_technique(body);
                self.locus
                    .pop();
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
        // Arity of every declared subroutine, indexed by SubroutineId, so an
        // Invoke's argument count can be checked against the procedure it
        // resolves to.
        let arities: Vec<usize> = self
            .program
            .subroutines
            .iter()
            .map(Subroutine::arity)
            .collect();
        for subroutine in &mut self
            .program
            .subroutines
        {
            Self::resolve_operation(
                &mut subroutine.body,
                &self.known,
                &arities,
                &mut self.problems,
            );
        }
    }

    fn resolve_operation(
        op: &mut Operation<'i>,
        known: &HashMap<&'i str, SubroutineId>,
        arities: &[usize],
        problems: &mut Vec<TranslationError<'i>>,
    ) {
        match op {
            Operation::Invoke(invocable) => {
                if let SubroutineRef::Unresolved(id) = &invocable.target {
                    match known.get(id.value) {
                        Some(sub_id) => {
                            // A bare call (`<thing>`) defers all arguments, so
                            // it is exempt; a parenthesised list must match the
                            // procedure's arity exactly.
                            let expected = arities[sub_id.0];
                            let actual = invocable
                                .arguments
                                .len();
                            if !invocable.elided && expected != actual {
                                problems.push(TranslationError::ProcedureArityMismatch {
                                    procedure: *id,
                                    expected,
                                    actual,
                                });
                            }
                            invocable.target = SubroutineRef::Resolved(*sub_id);
                        }
                        None => problems.push(TranslationError::UnresolvedProcedure(*id)),
                    }
                }
                for arg in &mut invocable.arguments {
                    Self::resolve_operation(arg, known, arities, problems);
                }
            }
            Operation::Sequence(ops) => {
                for op in ops {
                    Self::resolve_operation(op, known, arities, problems);
                }
            }
            Operation::Section { body, .. } => {
                Self::resolve_operation(body, known, arities, problems)
            }
            Operation::Step { body, .. } => Self::resolve_operation(body, known, arities, problems),
            Operation::Loop { over, body, .. } => {
                if let Some(over) = over {
                    Self::resolve_operation(over, known, arities, problems);
                }
                Self::resolve_operation(body, known, arities, problems);
            }
            Operation::Bind { value, .. } => {
                Self::resolve_operation(value, known, arities, problems)
            }
            Operation::Execute(executable) => {
                for arg in &mut executable.arguments {
                    Self::resolve_operation(arg, known, arities, problems);
                }
            }
            Operation::String(fragments) => {
                for fragment in fragments {
                    if let Fragment::Interpolation(op) = fragment {
                        Self::resolve_operation(op, known, arities, problems);
                    }
                }
            }
            Operation::Tablet(entries) => {
                for entry in entries {
                    Self::resolve_operation(&mut entry.value, known, arities, problems);
                }
            }
            Operation::List(items) => {
                for item in items {
                    Self::resolve_operation(item, known, arities, problems);
                }
            }
            Operation::Variable(_)
            | Operation::Number(_)
            | Operation::Multiline(_, _)
            | Operation::Hole => {}
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
            language::Expression::Pair(pair, _) => {
                // A standalone labelled value widens to a single-entry
                // tablet, mirroring the way a bare value widens to a
                // single-element list.
                Operation::Tablet(vec![Entry {
                    label: pair.label,
                    value: self.translate_expression(&pair.value),
                }])
            }
            language::Expression::List(elements, span) => {
                let labelled = elements
                    .iter()
                    .any(|element| {
                        if let language::Expression::Pair(..) = element {
                            true
                        } else {
                            false
                        }
                    });
                let unlabelled = elements
                    .iter()
                    .any(|element| {
                        if let language::Expression::Pair(..) = element {
                            false
                        } else {
                            true
                        }
                    });

                if labelled && unlabelled {
                    self.problems
                        .push(TranslationError::HeterogenousList { at: *span });
                }

                // All elements labelled: a tablet. Otherwise (including the
                // empty list and the mixed-content recovery case) a list.
                if labelled && !unlabelled {
                    let entries = elements
                        .iter()
                        .filter_map(|element| {
                            if let language::Expression::Pair(pair, _) = element {
                                Some(Entry {
                                    label: pair.label,
                                    value: self.translate_expression(&pair.value),
                                })
                            } else {
                                None
                            }
                        })
                        .collect();
                    Operation::Tablet(entries)
                } else {
                    let items = elements
                        .iter()
                        .map(|element| self.translate_expression(element))
                        .collect();
                    Operation::List(items)
                }
            }
            language::Expression::Application(invocation, _) => {
                Operation::Invoke(self.translate_invocation(invocation))
            }
            language::Expression::Execution(function, _) => Operation::Execute(Executable {
                target: ExecutableRef::Unresolved(function.target),
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
            language::Expression::Binding(value, names, span) => {
                if let language::Expression::Repeat(_, _) = value.as_ref() {
                    self.problems
                        .push(TranslationError::BoundRepeat { at: *span });
                }
                Operation::Bind {
                    names,
                    value: Box::new(self.translate_expression(value)),
                }
            }
            language::Expression::Hole(_) => Operation::Hole,
            language::Expression::Separator => Operation::Sequence(Vec::new()),
        }
    }

    fn translate_invocation(&mut self, invocation: &'i language::Invocation<'i>) -> Invocable<'i> {
        let target = match &invocation.target {
            language::Target::Local(id) => SubroutineRef::Unresolved(*id),
            language::Target::Remote(external) => SubroutineRef::Deferred(*external),
        };
        let elided = invocation
            .parameters
            .is_none();
        let arguments = match &invocation.parameters {
            Some(params) => params
                .iter()
                .map(|expr| self.translate_expression(expr))
                .collect(),
            None => Vec::new(),
        };
        Invocable {
            target,
            arguments,
            elided,
        }
    }
}
