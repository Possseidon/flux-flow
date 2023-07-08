pub mod grammar;
pub mod parse_request;
pub mod syntax_tree;
pub mod syntax_tree_node_builder;

use std::ops::Range;

use anyhow::anyhow;

use crate::compiler::parser::{
    parse_request::{BuildRequestPostAction, RepeatUntil},
    syntax_tree::Module,
    syntax_tree_node_builder::NodeBuilderError,
};

use self::{
    grammar::{Grammar, RecursiveRule, Rule, RuleRef},
    parse_request::{
        BuildRequest, ParseMode, ParseRequest, ParseRequestStack, Revert, RuleRequest,
    },
    syntax_tree::SyntaxTree,
    syntax_tree_node_builder::{NodeBuilderElement, NodeBuilderInput, NodeBuilderReader},
};

use super::{
    diagnostic::Diagnostic,
    lexer::{self, LexError, Token, TokenKind, TokenStream},
};

pub struct ModuleParseResult {
    pub syntax_tree: SyntaxTree,
    pub diagnostics: Vec<Diagnostic>,
}

pub fn parse(code: &str) -> ModuleParseResult {
    let mut token_stream = TokenStream::new();

    let grammar = Module::grammar();

    let mut state = ParseState {
        syntax_tree: SyntaxTree::new(token_stream.skip_whitespace_tokens(code)),
        diagnostics: vec![],
        last_lex_error: None,
        token_streams: vec![token_stream],
        parse_requests: ParseRequestStack::new(grammar),
        node_builder_input: NodeBuilderInput::new(),
    };

    while let Some(request) = state.parse_requests.pop() {
        state.process_parse_request(code, grammar, request);
        // state.visualize(code, grammar);
    }

    let token_stream = state
        .token_streams
        .pop()
        .expect("token streams should not be empty after parsing");
    assert!(state.token_streams.is_empty());
    assert!(token_stream.is_at_end_of_code(code));

    state.node_builder_input.final_check();

    ModuleParseResult {
        syntax_tree: state.syntax_tree,
        diagnostics: state.diagnostics,
    }
}

#[derive(Debug)]
struct ParseState {
    syntax_tree: SyntaxTree,
    diagnostics: Vec<Diagnostic>,
    last_lex_error: Option<usize>,
    token_streams: Vec<TokenStream>,
    parse_requests: ParseRequestStack,
    node_builder_input: NodeBuilderInput,
}

impl ParseState {
    fn token_stream(&self) -> TokenStream {
        *self
            .token_streams
            .last()
            .expect("token streams should not be empty")
    }

    fn token_stream_mut(&mut self) -> &mut TokenStream {
        self.token_streams
            .last_mut()
            .expect("token streams should not be empty")
    }

    /// Processes a single parse request.
    fn process_parse_request(&mut self, code: &str, grammar: &Grammar, request: ParseRequest) {
        match request {
            ParseRequest::Rule(rule_request) => {
                self.process_rule_request(code, grammar, rule_request)
            }
            ParseRequest::Build(build_request) => {
                self.process_build_request(code, grammar, build_request)
            }
        }
    }

    /// Parses a single token or pushes a build request on the stack.
    fn process_rule_request(&mut self, code: &str, grammar: &Grammar, rule_request: RuleRequest) {
        match rule_request.rule {
            Rule::Token(token_kind) => {
                self.parse_token(code, grammar, token_kind, rule_request.mode);
            }
            Rule::Ref(rule_ref) => {
                self.push_build_request(grammar, rule_ref, rule_request.mode);
            }
        }
    }

    /// Pushes a build request with the given mode on the stack.
    fn push_build_request(&mut self, grammar: &Grammar, rule_ref: RuleRef, mode: ParseMode) {
        self.parse_requests.push(ParseRequest::Build(BuildRequest {
            rule_ref,
            node_builder_index: self.node_builder_input.len(),
            post_action: match mode {
                ParseMode::Required => Some(BuildRequestPostAction::TreatMissingAsError),
                ParseMode::Optional => None,
                ParseMode::Essential => {
                    let index = self.token_streams.len();
                    self.token_streams.push(self.token_stream());
                    Some(BuildRequestPostAction::Revert(Revert {
                        token_stream: Some(index),
                        alternations: false,
                        build_request: true,
                    }))
                }
                ParseMode::Repetition(repeat_until) => {
                    Some(BuildRequestPostAction::Repeat(repeat_until))
                }
                ParseMode::Alternation => {
                    let index = self.token_streams.len();
                    self.token_streams.push(self.token_stream());
                    Some(BuildRequestPostAction::Revert(Revert {
                        token_stream: Some(index),
                        alternations: true,
                        build_request: false,
                    }))
                }
            },
        }));

        match &grammar.rule(rule_ref).rule {
            RecursiveRule::Concatenation {
                essential,
                last_essential,
                required,
            } => {
                self.parse_requests
                    .extend(required.iter().copied().rev().map(ParseRequest::required));
                self.parse_requests.push(ParseRequest::Rule(RuleRequest {
                    rule: *last_essential,
                    mode: ParseMode::Essential,
                }));
                self.parse_requests
                    .extend(essential.iter().copied().rev().map(ParseRequest::essential));
            }
            RecursiveRule::Alternation(rules) => {
                self.parse_requests.extend(rules.iter().rev().map(|&rule| {
                    ParseRequest::Rule(RuleRequest {
                        rule,
                        mode: ParseMode::Alternation,
                    })
                }))
            }
            RecursiveRule::Repetition(rule) => {
                self.parse_requests.push(ParseRequest::Rule(RuleRequest {
                    rule: *rule,
                    mode: ParseMode::Repetition(RepeatUntil::Mismatch),
                }))
            }
            RecursiveRule::GlobalRepetition(rule) => {
                self.parse_requests.push(ParseRequest::Rule(RuleRequest {
                    rule: *rule,
                    mode: ParseMode::Repetition(RepeatUntil::EndOfTokenStream),
                }))
            }
            RecursiveRule::BracedRepetition(brace_kind, rule) => {
                self.parse_requests.push(ParseRequest::Rule(RuleRequest {
                    rule: Rule::Token(brace_kind.closing_token()),
                    mode: ParseMode::Required,
                }));
                self.parse_requests.push(ParseRequest::Rule(RuleRequest {
                    rule: *rule,
                    mode: ParseMode::Repetition(RepeatUntil::Token(brace_kind.closing_token())),
                }));
                self.parse_requests.push(ParseRequest::Rule(RuleRequest {
                    rule: Rule::Token(brace_kind.opening_token()),
                    mode: ParseMode::Essential,
                }));
            }
        }
    }

    /// Parses a single token with the given mode.
    fn parse_token(
        &mut self,
        code: &str,
        grammar: &Grammar,
        token_kind: TokenKind,
        mode: ParseMode,
    ) {
        match (self.next_token(code), mode) {
            (Some(token), _) if token.token_kind == token_kind => {
                if let ParseMode::Alternation = mode {
                    self.parse_requests.pop_remaining_alternations();
                }
                self.repush_repetition_token(mode, token_kind);
                self.advance_token_and_push(code, token);
            }
            (Some(token), ParseMode::Repetition(RepeatUntil::Token(until_token)))
                if token.token_kind == until_token =>
            {
                self.node_builder_input.push(NodeBuilderElement::Empty)
            }
            (Some(_), ParseMode::Repetition(RepeatUntil::Mismatch))
            | (None, ParseMode::Repetition(RepeatUntil::EndOfTokenStream))
            | (
                _,
                ParseMode::Optional
                | ParseMode::Alternation
                | ParseMode::Repetition(RepeatUntil::Mismatch),
            ) => self.node_builder_input.push(NodeBuilderElement::Empty),

            (token, ParseMode::Essential) => {
                // TODO: add Option<Token> parameter for better error messages
                self.revert_build_request(code, grammar, token_kind.name(), token);
            }

            (Some(token), ParseMode::Required) => {
                self.x_expected_found_y(token_kind.name(), token.token_kind.name());
                self.node_builder_input.push(NodeBuilderElement::Error);
            }
            (Some(token), ParseMode::Repetition(RepeatUntil::Token(until_token))) => {
                self.repush_repetition_token(mode, token_kind);

                let TokenInfo { range, .. } = self.advance_token(code, token);
                self.x_or_y_expected_found_z_at(
                    token_kind.name(),
                    until_token.name(),
                    token.token_kind.name(),
                    range,
                );
            }
            (Some(token), ParseMode::Repetition(RepeatUntil::EndOfTokenStream)) => {
                self.repush_repetition_token(mode, token_kind);

                let TokenInfo { range, .. } = self.advance_token(code, token);
                self.x_expected_found_y_at(token_kind.name(), token.token_kind.name(), range);
            }

            (None, ParseMode::Repetition(RepeatUntil::Token(until_token))) => {
                self.x_or_y_expected(token_kind.name(), until_token.name());
                self.node_builder_input.push(NodeBuilderElement::Empty);
            }
            (None, ParseMode::Required) => {
                self.x_expected(token_kind.name());
                self.node_builder_input.push(NodeBuilderElement::Error);
            }
        }
    }

    /// Pushes the same token parse request if the current parse mode is [`ParseMode::Repetition`].
    fn repush_repetition_token(&mut self, mode: ParseMode, token_kind: TokenKind) {
        if let ParseMode::Repetition(_) = mode {
            self.parse_requests.push(ParseRequest::Rule(RuleRequest {
                rule: Rule::Token(token_kind),
                mode,
            }))
        }
    }

    /// Advances over a single token and trailing whitespace returning token index, token end index
    /// and trailing whitespace.
    fn advance_token(&mut self, code: &str, token: lexer::Token) -> TokenInfo {
        let index = self.token_stream().index();
        self.token_stream_mut().advance_token(token);
        TokenInfo {
            range: index..self.token_stream().index(),
            trailing_whitespace: self.token_stream_mut().skip_whitespace_tokens(code),
        }
    }

    /// Advances over a single token and whitespace and pushes it.
    fn advance_token_and_push(&mut self, code: &str, token: lexer::Token) {
        let TokenInfo {
            range,
            trailing_whitespace,
        } = self.advance_token(code, token);
        self.node_builder_input
            .push(NodeBuilderElement::Token(syntax_tree::Token {
                index: range.start,
                len: token.len,
                trailing_whitespace,
            }));
    }

    fn process_build_request(
        &mut self,
        code: &str,
        grammar: &Grammar,
        build_request: BuildRequest,
    ) {
        let named_rule = grammar.rule(build_request.rule_ref);
        let mut node_builder_reader = NodeBuilderReader::new(
            &mut self.node_builder_input,
            build_request.node_builder_index,
        );
        let node_ref = named_rule.builder.0(&mut self.syntax_tree, &mut node_builder_reader);
        let reader_empty = node_builder_reader.is_empty();
        self.node_builder_input
            .pop_stack(build_request.node_builder_index);

        match node_ref {
            Ok(Some(node_ref)) => {
                assert!(reader_empty);
                self.node_builder_input
                    .push(NodeBuilderElement::NodeRef(node_ref));

                match build_request.post_action {
                    Some(BuildRequestPostAction::Repeat(repeat_until)) => {
                        self.push_build_request(
                            grammar,
                            build_request.rule_ref,
                            ParseMode::Repetition(repeat_until),
                        );
                    }
                    Some(BuildRequestPostAction::Revert(revert)) => {
                        if let Some(index) = revert.token_stream {
                            let last_token_stream = self.token_stream();
                            self.token_streams.drain(index..);
                            *self.token_stream_mut() = last_token_stream;
                        }
                        if revert.alternations {
                            self.parse_requests.pop_remaining_alternations();
                        }
                    }
                    Some(BuildRequestPostAction::TreatMissingAsError) | None => {}
                }
            }
            Ok(None) => {
                assert!(reader_empty);
                match build_request.post_action {
                    Some(BuildRequestPostAction::TreatMissingAsError) => {
                        self.x_expected(named_rule.name);
                        self.node_builder_input.push(NodeBuilderElement::Error);
                    }
                    Some(BuildRequestPostAction::Repeat(repeat_until)) => {
                        self.process_mismatched_repetition_build_request(
                            code,
                            grammar,
                            repeat_until,
                            named_rule.name,
                            build_request.rule_ref,
                        );
                    }
                    Some(BuildRequestPostAction::Revert(revert)) => {
                        if let Some(index) = revert.token_stream {
                            self.token_streams.drain(index..);
                        }
                        if revert.alternations {
                            self.parse_requests.pop_remaining_alternations();
                        }
                        if revert.build_request {
                            let token = self.next_token(code);
                            self.revert_build_request(code, grammar, named_rule.name, token);
                        } else {
                            self.node_builder_input.push(NodeBuilderElement::Empty);
                        }
                    }
                    None => {
                        self.node_builder_input.push(NodeBuilderElement::Empty);
                    }
                }
            }
            Err(NodeBuilderError) => match build_request.post_action {
                Some(BuildRequestPostAction::Repeat(repeat_until)) => {
                    if let Some(token) = self.next_token(code) {
                        match repeat_until {
                            RepeatUntil::Mismatch | RepeatUntil::EndOfTokenStream => {
                                let TokenInfo { range, .. } = self.advance_token(code, token);
                                self.x_expected_found_y_at(
                                    named_rule.name,
                                    token.token_kind.name(),
                                    range,
                                );

                                self.push_build_request(
                                    grammar,
                                    build_request.rule_ref,
                                    ParseMode::Repetition(repeat_until),
                                );
                            }
                            RepeatUntil::Token(repeat_until_token) => {
                                if repeat_until_token == token.token_kind {
                                    self.node_builder_input.push(NodeBuilderElement::Empty)
                                } else {
                                    // This might endless loop?
                                    // Might need to advance over the token in some cases.
                                    self.push_build_request(
                                        grammar,
                                        build_request.rule_ref,
                                        ParseMode::Repetition(repeat_until),
                                    );
                                }
                            }
                        }
                    } else {
                        self.node_builder_input.push(NodeBuilderElement::Empty)
                    }
                }
                Some(BuildRequestPostAction::Revert(revert)) => {
                    if let Some(index) = revert.token_stream {
                        let last_token_stream = self.token_stream();
                        self.token_streams.drain(index..);
                        *self.token_stream_mut() = last_token_stream;
                    }
                    if revert.alternations {
                        self.parse_requests.pop_remaining_alternations();
                    }
                    self.node_builder_input.push(NodeBuilderElement::Error);
                }
                Some(BuildRequestPostAction::TreatMissingAsError) | None => {
                    self.node_builder_input.push(NodeBuilderElement::Error);
                }
            },
        }
    }

    fn revert_build_request(
        &mut self,
        code: &str,
        grammar: &Grammar,
        rule_name: &str,
        token: Option<Token>,
    ) {
        let mut next_build_request = Some(self.parse_requests.pop_build_request());
        while let Some(build_request) = next_build_request.take() {
            self.node_builder_input
                .pop_stack(build_request.node_builder_index);
            match build_request.post_action {
                Some(BuildRequestPostAction::Revert(revert)) => {
                    if let Some(index) = revert.token_stream {
                        self.token_streams.drain(index..);
                    }
                    if revert.build_request {
                        next_build_request = Some(self.parse_requests.pop_build_request());
                    } else {
                        self.node_builder_input.push(NodeBuilderElement::Empty);
                    }
                }
                Some(BuildRequestPostAction::TreatMissingAsError) => {
                    if let Some(token) = token {
                        self.x_expected_found_y(rule_name, token.token_kind.name());
                    } else {
                        self.x_expected(rule_name);
                    }
                    self.node_builder_input.push(NodeBuilderElement::Empty);
                }
                Some(BuildRequestPostAction::Repeat(repeat_until)) => {
                    self.process_mismatched_repetition_build_request(
                        code,
                        grammar,
                        repeat_until,
                        rule_name,
                        build_request.rule_ref,
                    );
                }
                None => self.node_builder_input.push(NodeBuilderElement::Empty),
            }
        }
    }

    /// Returns the next token of the token stream, processing all lex errors that may occur.
    ///
    /// Only lex errors are advanced, the final token will not be advanced yet to allow for proper
    /// handling of nested token streams.
    fn next_token(&mut self, code: &str) -> Option<lexer::Token> {
        loop {
            match self.token_stream().peek(code) {
                Some(Ok(next_token)) => break Some(next_token),
                Some(Err(error)) => self.process_lex_error(code, error),
                None => break None,
            }
        }
    }

    /// Advances over the error and adds a diagnostic.
    ///
    /// In case of backtracking, only the first instance of the error is added as diagnostic.
    fn process_lex_error(&mut self, code: &str, error: LexError) {
        let index = self.token_stream().index();
        let end = index + self.token_stream_mut().advance_error(code, &error);
        self.token_stream_mut().skip_whitespace_tokens(code);

        if self
            .last_lex_error
            .map_or(true, |last_lex_error| index > last_lex_error)
        {
            self.last_lex_error = Some(index);
            self.diagnostics.push(Diagnostic::error(error, index..end));
        }
    }

    #[allow(dead_code)]
    fn visualize(&self, code: &str, grammar: &Grammar) {
        print!("Nodes:");
        for node in &self.node_builder_input.stack {
            match node {
                NodeBuilderElement::NodeRef(node_ref) => {
                    print!(" {:?}", node_ref);
                }
                NodeBuilderElement::Token(token) => {
                    print!(" {}", &code[token.index..(token.index + token.len)]);
                }
                NodeBuilderElement::Empty => {
                    print!(" empty");
                }
                NodeBuilderElement::Error => {
                    print!(" error");
                }
            };
        }
        println!();

        print!("TokenStream at {}", self.token_stream().index());

        for parse_request in &self.parse_requests.0 {
            match parse_request {
                ParseRequest::Rule(rule) => {
                    let name = match rule.rule {
                        Rule::Token(token) => token.name(),
                        Rule::Ref(rule_ref) => grammar.rule(rule_ref).name,
                    };

                    print!(" {name}");
                }
                ParseRequest::Build(build) => {
                    let name = grammar.rule(build.rule_ref).name;
                    print!("\n- {name}:")
                }
            }
        }
        println!();

        for diagnostic in &self.diagnostics {
            println!("⚠ {}", diagnostic.error);
        }
        println!();
    }

    fn process_mismatched_repetition_build_request(
        &mut self,
        code: &str,
        grammar: &Grammar,
        repeat_until: RepeatUntil,
        rule_name: &str,
        rule_ref: RuleRef,
    ) {
        match repeat_until {
            RepeatUntil::Mismatch => {
                self.node_builder_input.push(NodeBuilderElement::Empty);
            }
            RepeatUntil::Token(repeat_until_token) => {
                if let Some(token) = self.next_token(code) {
                    if token.token_kind == repeat_until_token {
                        self.node_builder_input.push(NodeBuilderElement::Empty);
                    } else {
                        let TokenInfo { range, .. } = self.advance_token(code, token);
                        self.x_or_y_expected_found_z_at(
                            rule_name,
                            repeat_until_token.name(),
                            token.token_kind.name(),
                            range,
                        );

                        self.push_build_request(
                            grammar,
                            rule_ref,
                            ParseMode::Repetition(RepeatUntil::Token(repeat_until_token)),
                        );
                    }
                } else {
                    self.x_or_y_expected(rule_name, repeat_until_token.name());
                    self.node_builder_input.push(NodeBuilderElement::Empty);
                }
            }
            RepeatUntil::EndOfTokenStream => {
                if let Some(token) = self.next_token(code) {
                    let TokenInfo { range, .. } = self.advance_token(code, token);
                    self.x_expected_found_y_at(rule_name, token.token_kind.name(), range);

                    self.push_build_request(
                        grammar,
                        rule_ref,
                        ParseMode::Repetition(RepeatUntil::EndOfTokenStream),
                    );
                } else {
                    self.node_builder_input.push(NodeBuilderElement::Empty);
                }
            }
        }
    }

    fn error(&mut self, error: anyhow::Error) {
        self.diagnostics
            .push(Diagnostic::error(error, self.token_stream().index()));
    }

    fn error_at(&mut self, error: anyhow::Error, range: Range<usize>) {
        self.diagnostics.push(Diagnostic::error(error, range));
    }

    fn x_expected(&mut self, x: &str) {
        self.error(anyhow!("{x} expected"));
    }

    fn x_expected_found_y(&mut self, x: &str, y: &str) {
        self.error(anyhow!("{x} expected, found {y}"));
    }

    fn x_expected_found_y_at(&mut self, x: &str, y: &str, range: Range<usize>) {
        self.error_at(anyhow!("{x} expected, found {y}"), range);
    }
    fn x_or_y_expected(&mut self, x: &str, y: &str) {
        self.error(anyhow!("{x} or {y} expected"));
    }

    // fn x_or_y_expected_found_z(&mut self, x: &str, y: &str, z: &str) {
    //     self.error(anyhow!("{x} or {y} expected, found {z}"));
    // }

    fn x_or_y_expected_found_z_at(&mut self, x: &str, y: &str, z: &str, range: Range<usize>) {
        self.error_at(anyhow!("{x} or {y} expected, found {z}"), range);
    }
}

struct TokenInfo {
    range: Range<usize>,
    trailing_whitespace: usize,
}
