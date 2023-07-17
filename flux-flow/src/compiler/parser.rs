pub mod grammar;
pub mod node_builder;
pub mod parse_request;
pub mod syntax_tree;
mod token_stream_stack;

use std::ops::Range;

use anyhow::anyhow;

use crate::compiler::{
    lexer::LexerHints,
    parser::{
        node_builder::NodeBuilderInput,
        parse_request::{BuildRequest, ParseRequestStack},
        syntax_tree::Module,
    },
};

use self::{
    grammar::{Grammar, RecursiveRule, RuleRef},
    node_builder::{NodeBuilt, NodeError},
    parse_request::{GroupParseMode, MatchRequest, ParseMode, ParseRequest, RepeatUntil},
    syntax_tree::SyntaxTree,
    token_stream_stack::{TokenInfo, TokenStreamStack},
};

use super::{
    diagnostic::Diagnostic,
    lexer::{
        self, ClosingToken, GrammarToken, GroupKind, GroupToken, LexError, OpeningToken, TokenKind,
        TokenStream,
    },
};

// TODO: Required mismatch does not cancel remaining node builder inputs.
//       Do not push more nodes if node builder is required mismatch.

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
        token_streams: TokenStreamStack::new(token_stream),
        parse_requests: ParseRequestStack::new(),
        node_builder_input: NodeBuilderInput::new(),
    };

    state.push_build_request(grammar, grammar.initial_rule(), ParseMode::Required);

    // state.visualize(code, grammar);
    // let mut i = 0;

    while let Some(request) = state.parse_requests.pop() {
        // Debugging tip:
        // 1. Run without breakpoint
        // 2. Look for error in output and copy its leading integer
        // 3. Put a conditional breakpoint with `i == N` on the following line:
        state.dispatch_parse_request(code, grammar, request);

        // println!("{i}");
        // state.visualize(code, grammar);
        // i += 1;
    }

    state.token_streams.validate(code);

    ModuleParseResult {
        syntax_tree: state.syntax_tree,
        diagnostics: state.diagnostics,
    }
}

#[derive(Debug)]
struct ParseState {
    /// The final output of the parser, slowly getting filled in during the parsing process.
    syntax_tree: SyntaxTree,
    /// Diagnostics that were encountered during the parsing process.
    diagnostics: Vec<Diagnostic>,
    /// Keeps track of the last encountered lexing error, so that errors that occur during
    /// parsing of a previously already lexed sections can be avoided.
    last_lex_error: Option<usize>,
    /// A stack of token streams.
    ///
    /// Each build request pushes a new token stream onto the stack, so that it can easily be
    /// reverted in case of mismatches or errors.
    ///
    /// To revert, the top token stream simply gets popped off the stack.
    ///
    /// To commit a successful build request, the second to top token stream is removed instead.
    token_streams: TokenStreamStack,
    /// A stack of pending parse requests.
    ///
    /// A single parsing step consists of popping the top request off the stack and processing it.
    parse_requests: ParseRequestStack,
    /// A stack onto which tokens, nodes and also mismatches or errors are pushed.
    ///
    /// These elements are processed during build requests and get turned into nodes.
    node_builder_input: NodeBuilderInput,
}

impl ParseState {
    /// Dispatches a single parse request to different functions that then process the request.
    fn dispatch_parse_request(&mut self, code: &str, grammar: &Grammar, request: ParseRequest) {
        match request {
            ParseRequest::Build(build_request) => {
                self.process_build_request(code, grammar, build_request);
            }
            ParseRequest::Match(match_request) => match match_request {
                MatchRequest::Token {
                    token_kind,
                    parse_mode,
                } => {
                    self.parse_token(code, token_kind, parse_mode);
                }
                MatchRequest::Rule {
                    rule_ref,
                    parse_mode,
                } => {
                    self.push_build_request(grammar, rule_ref, parse_mode);
                }
                MatchRequest::Group {
                    token_kind,
                    parse_mode,
                } => match token_kind {
                    GroupToken::Opening(token_kind) => {
                        self.process_opening_group_request(code, token_kind, parse_mode);
                    }
                    GroupToken::Closing(token_kind) => {
                        self.process_closing_group_request(code, token_kind, parse_mode);
                    }
                },
            },
        }
    }

    fn process_build_request(
        &mut self,
        code: &str,
        grammar: &Grammar,
        build_request: BuildRequest,
    ) {
        let named_rule = grammar.rule(build_request.rule);

        let result = self
            .node_builder_input
            .build(&mut self.syntax_tree, named_rule.builder);

        match result {
            Ok(NodeBuilt { warnings: false }) => {
                self.token_streams.commit();

                match build_request.parse_mode {
                    ParseMode::Essential => {}
                    ParseMode::Required => {}
                    ParseMode::Optional => {}
                    ParseMode::Repetition { repeat_until } => {
                        if self.token_streams.current().is_at_end_of_code(code) {
                            self.node_builder_input
                                .push_end_repetition(self.code_index());
                        } else {
                            self.push_build_request(
                                grammar,
                                build_request.rule,
                                ParseMode::Repetition {
                                    repeat_until: repeat_until.with_required(),
                                },
                            );
                        }
                    }
                    ParseMode::Alternation => self.parse_requests.pop_remaining_match_requests(),
                }
            }
            Ok(NodeBuilt { warnings: true }) => {
                self.token_streams.commit();

                match build_request.parse_mode {
                    ParseMode::Essential => {}
                    ParseMode::Required => {}
                    ParseMode::Optional => {}
                    ParseMode::Repetition { repeat_until } => {
                        if self.token_streams.current().is_at_end_of_code(code) {
                            self.node_builder_input
                                .push_end_repetition(self.code_index());
                        } else {
                            self.push_build_request(
                                grammar,
                                build_request.rule,
                                ParseMode::Repetition {
                                    repeat_until: repeat_until.with_required(),
                                },
                            );
                        }
                    }
                    ParseMode::Alternation => self.parse_requests.pop_remaining_match_requests(),
                }
            }
            Err(NodeError::Mismatch) => {
                self.token_streams.revert();

                match build_request.parse_mode {
                    ParseMode::Essential => {
                        self.node_builder_input.mismatch(None);
                        self.parse_requests.pop_remaining_match_requests();
                    }
                    ParseMode::Required => {
                        self.node_builder_input.error();
                        self.x_expected(named_rule.name);
                    }
                    ParseMode::Optional => {
                        self.node_builder_input
                            .push_optional_mismatch(self.code_index());
                    }
                    ParseMode::Repetition { repeat_until } => {
                        if self.repetition_essential_mismatch(
                            code,
                            named_rule.name,
                            repeat_until,
                            None,
                        ) {
                            self.push_build_request(
                                grammar,
                                build_request.rule,
                                ParseMode::Repetition {
                                    repeat_until: repeat_until.with_required(),
                                },
                            )
                        }
                    }
                    ParseMode::Alternation => {
                        self.node_builder_input.alternation_mismatch();
                    }
                }
            }
            Err(NodeError::Error) => {
                self.token_streams.commit();

                match build_request.parse_mode {
                    ParseMode::Essential => self.node_builder_input.error(),
                    ParseMode::Required => self.node_builder_input.error(),
                    ParseMode::Optional => self.node_builder_input.error(),
                    ParseMode::Repetition { repeat_until } => {
                        self.node_builder_input.repetition_error();

                        if self.token_streams.current().is_at_end_of_code(code) {
                            self.node_builder_input
                                .push_end_repetition(self.code_index());
                        } else {
                            self.push_build_request(
                                grammar,
                                build_request.rule,
                                ParseMode::Repetition {
                                    repeat_until: repeat_until.with_required(),
                                },
                            );
                        }
                    }
                    ParseMode::Alternation => {
                        self.node_builder_input.error();
                        self.parse_requests.pop_remaining_match_requests();
                    }
                }
            }
        }
    }

    /// Parses a single token with the given parse mode.
    fn parse_token(&mut self, code: &str, token_kind: GrammarToken, parse_mode: ParseMode) {
        match (parse_mode, self.next_token(code, false)) {
            (parse_mode, Some(token)) if token.kind == token_kind.into() => {
                self.advance_token_and_push(code, token);

                if let ParseMode::Repetition { .. } = parse_mode {
                    self.parse_requests.push(MatchRequest::Token {
                        token_kind,
                        parse_mode,
                    })
                }

                if let ParseMode::Alternation = parse_mode {
                    self.parse_requests.pop_remaining_match_requests();
                }
            }

            (ParseMode::Essential, _) => {
                self.node_builder_input.mismatch(None);
                self.parse_requests.pop_remaining_match_requests();
            }

            (ParseMode::Required, _) => {
                self.node_builder_input.error();

                // self.skip_maybe_token(code, token_kind.name(), token);
                self.x_expected(token_kind.name())
            }

            (ParseMode::Optional, _) => self
                .node_builder_input
                .push_optional_mismatch(self.code_index()),

            (ParseMode::Repetition { repeat_until }, token) => {
                if self.repetition_essential_mismatch(
                    code,
                    token_kind.name(),
                    repeat_until,
                    Some(token),
                ) {
                    self.parse_requests.push(MatchRequest::Token {
                        token_kind,
                        parse_mode,
                    })
                }
            }

            (ParseMode::Alternation, _) => self.node_builder_input.alternation_mismatch(),
        }
    }

    fn repetition_essential_mismatch(
        &mut self,
        code: &str,
        expected: &str,
        repeat_until: RepeatUntil,
        next_token: Option<Option<lexer::Token>>,
    ) -> bool {
        match repeat_until {
            RepeatUntil::Mismatch => {
                self.node_builder_input
                    .push_end_repetition(self.code_index());

                false
            }
            RepeatUntil::ClosingGroup {
                group_kind,
                required,
            } => {
                if let Some(token) = next_token.unwrap_or_else(|| self.next_token(code, false)) {
                    if token.kind == group_kind.closing_token().into() {
                        self.node_builder_input
                            .push_end_repetition(self.code_index());

                        false
                    } else if let TokenKind::Group(group_token) = token.kind {
                        if self.token_streams.contains_group(group_token.kind()) {
                            self.node_builder_input
                                .push_end_repetition(self.code_index());

                            false
                        } else {
                            self.node_builder_input.repetition_error();
                            if self.skip_token(code, expected, token) {
                                true
                            } else {
                                self.node_builder_input
                                    .push_end_repetition(self.code_index());

                                false
                            }
                        }
                    } else if required {
                        self.node_builder_input.repetition_error();
                        if self.skip_token(code, expected, token) {
                            true
                        } else {
                            self.node_builder_input
                                .push_end_repetition(self.code_index());

                            false
                        }
                    } else {
                        if self.node_builder_input.mismatch(Some(self.code_index())) {
                            self.parse_requests.pop_remaining_match_requests();
                        }

                        false
                    }
                } else {
                    self.node_builder_input
                        .push_end_repetition(self.code_index());

                    false
                }
            }
            RepeatUntil::Eof => {
                if let Some(token) = self.next_token(code, false) {
                    self.node_builder_input.repetition_error();
                    if self.skip_token(code, expected, token) {
                        true
                    } else {
                        self.node_builder_input
                            .push_end_repetition(self.code_index());

                        false
                    }
                } else {
                    self.node_builder_input
                        .push_end_repetition(self.code_index());

                    false
                }
            }
        }
    }

    /// Skips the given token and reports an error.
    ///
    /// This might not be possible, since closing group tokens cannot be skipped if there is a
    /// matching opening group token still awaiting to be closed. In that case `false` is returned.
    fn skip_token(&mut self, code: &str, expected: &str, token: lexer::Token) -> bool {
        if let TokenKind::Group(GroupToken::Opening(_)) = token.kind {
            let start = self.code_index();

            let initial_depth = self.token_streams.group_depth();
            self.token_streams.advance_token(code, token);

            let mut end = self.code_index();

            while let Some(token) = self.next_token(code, false) {
                if let TokenKind::Group(GroupToken::Closing(group_token)) = token.kind {
                    if self
                        .token_streams
                        .contains_group_before_but_not_after(group_token.kind(), initial_depth)
                    {
                        break;
                    }
                }

                if self.token_streams.group_depth() <= initial_depth {
                    break;
                }

                end = self.token_streams.advance_token(code, token).range.end;
            }

            self.x_expected_at(expected, start..end);
            return true;
        }

        if let TokenKind::Group(GroupToken::Closing(token_kind)) = token.kind {
            if self.token_streams.contains_group(token_kind.kind()) {
                self.x_expected(expected);
                return false;
            }
        }

        let start = self.code_index();
        let end = start + token.len.get();
        self.x_expected_found_y_at(expected, token.kind.name(), start..end);
        self.token_streams.advance_token(code, token);

        true
    }

    /// Pushes a build request for the given rule and with the given parse mode on the stack.
    fn push_build_request(&mut self, grammar: &Grammar, rule_ref: RuleRef, parse_mode: ParseMode) {
        self.token_streams.push();

        self.parse_requests.push(ParseRequest::Build(BuildRequest {
            rule: rule_ref,
            parse_mode,
        }));

        match &grammar.rule(rule_ref).rule {
            RecursiveRule::Concatenation {
                essential,
                last_essential,
                required,
            } => {
                self.parse_requests.extend(required.iter().rev());
                self.parse_requests.extend([last_essential]);
                self.parse_requests.extend(essential.iter().rev());

                self.node_builder_input.push_concatenation();
            }
            RecursiveRule::Alternation { variants } => {
                self.parse_requests.extend(variants.iter().rev());

                self.node_builder_input.push_alternation(variants.len());
            }
        }
    }

    /// Processes a group request, pushing a new group on the group stack.
    fn process_opening_group_request(
        &mut self,
        code: &str,
        token_kind: OpeningToken,
        parse_mode: GroupParseMode,
    ) {
        match (
            parse_mode,
            self.next_token(code, token_kind == OpeningToken::Angle),
        ) {
            (_, Some(token)) if token.kind == TokenKind::Group(token_kind.into()) => {
                self.advance_token_and_push(code, token);
            }

            (GroupParseMode::Essential, _) => {
                self.node_builder_input.mismatch(None);
                self.parse_requests.pop_remaining_match_requests();
            }

            (GroupParseMode::Required, _) => {
                self.x_expected(token_kind.kind().name());
                self.node_builder_input.error();

                let code_index = self.code_index();
                let node_builder_input = &mut self.node_builder_input;
                self.parse_requests
                    .pop_group(token_kind.kind(), |parse_mode| match parse_mode {
                        ParseMode::Essential => node_builder_input.error(),
                        ParseMode::Required => node_builder_input.error(),
                        ParseMode::Optional => {
                            node_builder_input.mismatch(None);
                        }
                        ParseMode::Repetition { .. } => {
                            node_builder_input.push_end_repetition(code_index)
                        }
                        ParseMode::Alternation => panic!("unexpected alternation"),
                    });

                self.node_builder_input.error();
            }
        }
    }

    fn process_closing_group_request(
        &mut self,
        code: &str,
        token_kind: ClosingToken,
        parse_mode: GroupParseMode,
    ) {
        match (parse_mode, self.next_token(code, false)) {
            (_, Some(token)) if token.kind == TokenKind::Group(token_kind.into()) => {
                self.advance_token_and_push(code, token)
            }

            (GroupParseMode::Essential, _) => {
                self.node_builder_input.mismatch(None);
                self.parse_requests.pop_remaining_match_requests();
            }

            (GroupParseMode::Required, Some(mut token)) => {
                let start = self.code_index();
                let mut end = start;
                let mut closing_group = false;

                let initial_depth = self.token_streams.group_depth();

                loop {
                    if let TokenKind::Group(GroupToken::Closing(group_token)) = token.kind {
                        if self.token_streams.contains_group_before_but_not_after(
                            group_token.kind(),
                            initial_depth - 1,
                        ) {
                            break;
                        }
                    }

                    let TokenInfo {
                        range,
                        trailing_whitespace_len,
                    } = self.token_streams.advance_token(code, token);

                    if self.token_streams.group_depth() < initial_depth {
                        self.node_builder_input.push_token(syntax_tree::Token {
                            len: token.len,
                            code_index: range.start,
                            trailing_whitespace_len,
                        });
                        closing_group = true;
                        break;
                    }

                    end = range.end;

                    token = match self.next_token(code, false) {
                        Some(token) => token,
                        None => break,
                    };
                }

                if closing_group {
                    self.error_at(anyhow!("unexpected trailing data"), start..end);
                } else {
                    self.x_expected_at(token_kind.name(), start..end);
                    self.node_builder_input.error();
                }
            }
            (GroupParseMode::Required, None) => {
                self.x_expected(token_kind.name());
                self.node_builder_input.error();
            }
        }
    }

    /// Advances the current token stream over the given token (including trailing whitespace) and
    /// pushes it onto the node builder.
    fn advance_token_and_push(&mut self, code: &str, token: lexer::Token) {
        let TokenInfo {
            range,
            trailing_whitespace_len,
        } = self.token_streams.advance_token(code, token);
        self.node_builder_input.push_token(syntax_tree::Token {
            len: token.len,
            code_index: range.start,
            trailing_whitespace_len,
        });
    }

    /// Returns the next token of the token stream, processing all lex errors that may occur.
    fn next_token(
        &mut self,
        code: &str,
        expecting_opening_angle_bracket: bool,
    ) -> Option<lexer::Token> {
        loop {
            match self
                .token_streams
                .current()
                .peek(code, self.lexer_hints(expecting_opening_angle_bracket))
            {
                Ok(next_token) => break next_token,
                Err(error) => self.process_lex_error(code, error),
            }
        }
    }

    /// Advances over the error and adds a diagnostic.
    ///
    /// In case of backtracking, only the first instance of the error is added as diagnostic.
    fn process_lex_error(&mut self, code: &str, error: LexError) {
        let (index, end) = self.token_streams.advance_error(code, &error);

        if self
            .last_lex_error
            .map_or(true, |last_lex_error| index > last_lex_error)
        {
            self.last_lex_error = Some(index);
            self.diagnostics.push(Diagnostic::error(error, index..end));
        }
    }

    fn lexer_hints(&self, expecting_opening_angle_bracket: bool) -> LexerHints {
        LexerHints {
            prefer_angle_brackets: expecting_opening_angle_bracket
                || self.token_streams.current_group_kind() == Some(GroupKind::Angle),
        }
    }

    fn error(&mut self, error: anyhow::Error) {
        self.diagnostics
            .push(Diagnostic::error(error, self.code_index()));
    }

    fn error_at(&mut self, error: anyhow::Error, range: Range<usize>) {
        self.diagnostics.push(Diagnostic::error(error, range));
    }

    fn x_expected(&mut self, x: &str) {
        self.error(anyhow!("{x} expected"));
    }

    fn x_expected_at(&mut self, x: &str, range: Range<usize>) {
        self.error_at(anyhow!("{x} expected"), range);
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

    fn x_or_y_expected_found_z(&mut self, x: &str, y: &str, z: &str) {
        self.error(anyhow!("{x} or {y} expected, found {z}"));
    }

    fn x_or_y_expected_found_z_at(&mut self, x: &str, y: &str, z: &str, range: Range<usize>) {
        self.error_at(anyhow!("{x} or {y} expected, found {z}"), range);
    }

    fn code_index(&self) -> usize {
        self.token_streams.current().code_index()
    }

    #[allow(dead_code)]
    fn visualize(&self, code: &str, grammar: &Grammar) {
        self.token_streams.visualize();
        self.node_builder_input.visualize(code);
        self.parse_requests.visualize(grammar);
        println!();

        for diagnostic in &self.diagnostics {
            println!("⚠ {}", diagnostic.error);
        }
        println!();
    }
}
