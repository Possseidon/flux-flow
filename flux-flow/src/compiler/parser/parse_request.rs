use derive_more::From;

use crate::compiler::lexer::{GrammarToken, GroupKind, GroupToken, TokenKind};

use super::grammar::{
    AlternationRule, EssentialRule, EssentialRuleMode, GlobalRule, Grammar, RequiredRule,
    RequiredRuleMode, Rule, RuleRef,
};

/// Contains a stack of [`ParseRequest`]s.
///
/// Consists of [`BuildRequest`]s that are followed by a series of [`MatchRequest`]s.
#[derive(Clone, Debug, Default)]
pub struct ParseRequestStack {
    parse_requests: Vec<ParseRequest>,
}

/// A single parse request, being either a [`MatchRequest`] or a [`BuildRequest`].
#[derive(Clone, Copy, Debug, From)]
pub enum ParseRequest {
    Build(BuildRequest),
    Match(MatchRequest),
}

/// A build request for a given rule.
#[derive(Clone, Copy, Debug)]
pub struct BuildRequest {
    pub rule: RuleRef,
    pub parse_mode: ParseMode,
}

/// Matches the given token or group or expands a rule into a new [`BuildRequest`] followed by
/// more [`MatchRequest`]s.
#[derive(Clone, Copy, Debug)]
pub enum MatchRequest {
    /// Matches a single token.
    Token {
        token_kind: GrammarToken,
        parse_mode: ParseMode,
    },
    /// Matches an entire rule by pushing a new [`BuildRequest`] for it on the stack.
    ///
    /// Rules are not expanded immediately, so that they can be discarded more easily.
    Rule {
        rule_ref: RuleRef,
        parse_mode: ParseMode,
    },
    /// Matches a group token, updating the group stack.
    Group {
        token_kind: GroupToken,
        parse_mode: GroupParseMode,
    },
}

/// How to parse a token or rule.
#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum ParseMode {
    /// Cancels the current build request on mismatch.
    Essential,
    /// Causes an immediate error on mismatch, but continues the current build request.
    Required,
    /// Does not cause an error and continues the current build request on mismatch.
    Optional,
    /// Repeatedly pushes back the same rule and only stops once [`RepeatUntil`] matches.
    Repetition { repeat_until: RepeatUntil },
    /// Similar to [`ParseMode::Optional`], but pops remaining [`ParseMode::Alternation`]s on match.
    Alternation,
}

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum RepeatUntil {
    Mismatch,
    ClosingGroup {
        group_kind: GroupKind,
        required: bool,
    },
    Eof,
}

impl RepeatUntil {
    pub fn with_required(self) -> Self {
        match self {
            RepeatUntil::ClosingGroup { group_kind, .. } => RepeatUntil::ClosingGroup {
                group_kind,
                required: true,
            },
            _ => self,
        }
    }
}

/// Group tokens are always either essential or required.
///
/// Only the content of a group can be optional/repeated.
#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum GroupParseMode {
    Essential,
    Required,
}

impl ParseRequestStack {
    /// Creates a new [ParseRequestStack].
    pub fn new() -> Self {
        Self::default()
    }

    /// Pops a [ParseRequest] from the stack.
    pub fn pop(&mut self) -> Option<ParseRequest> {
        self.parse_requests.pop()
    }

    pub fn peek(&self) -> ParseRequest {
        *self
            .parse_requests
            .last()
            .expect("parse stack should not be empty")
    }

    /// Pushes a [ParseRequest] onto the stack.
    pub fn push(&mut self, request: impl Into<ParseRequest>) {
        self.parse_requests.push(request.into());
    }

    /// Pops an entire [BuildRequest] from the stack.
    ///
    /// Simply discards anything that is not a [BuildRequest].
    pub fn pop_build_request(&mut self) -> BuildRequest {
        loop {
            if let ParseRequest::Build(build_request) = self
                .parse_requests
                .pop()
                .expect("parse stack should contain a build request")
            {
                break build_request;
            }
        }
    }

    /// Pops all remaining [`MatchRequest`], so that only the current [`BuildRequest`] remains.
    pub fn pop_remaining_match_requests(&mut self) {
        while let Some(ParseRequest::Match { .. }) = self.parse_requests.last() {
            self.parse_requests.pop();
        }
    }

    /// Whether a [`BuildRequest`] comes next since all corresponding [`MatchRequest`]s have been
    /// processed.
    pub fn next_is_build_request(&self) -> bool {
        let last = self
            .parse_requests
            .last()
            .expect("parse stack should not be empty");
        matches!(last, ParseRequest::Build { .. })
    }

    pub fn visualize(&self, grammar: &Grammar) {
        print!("PraseRequests:");
        for parse_request in &self.parse_requests {
            match parse_request {
                ParseRequest::Build(build) => print!("\n- {}:", grammar.rule(build.rule).name),
                ParseRequest::Match(match_request) => match match_request {
                    MatchRequest::Token { token_kind, .. } => print!(" {}", token_kind.name()),
                    MatchRequest::Rule { rule_ref, .. } => {
                        print!(" {}", grammar.rule(*rule_ref).name)
                    }
                    MatchRequest::Group { token_kind, .. } => print!(" {}", token_kind.name()),
                },
            }
        }
    }

    pub fn next_token_is(&self, kind: TokenKind) -> bool {
        // Could be improved by also looking into rule requests.
        // If so, make sure not to do it recursively but iterative.

        if let Some(ParseRequest::Match(MatchRequest::Token { token_kind, .. })) =
            self.parse_requests.last()
        {
            TokenKind::from(*token_kind) == kind
        } else {
            false
        }
    }

    pub fn pop_group(&mut self, token_kind: GroupKind, mut skip: impl FnMut(ParseMode)) {
        while let Some(ParseRequest::Match(match_request)) = self.parse_requests.pop() {
            match match_request {
                MatchRequest::Group {
                    token_kind: group_token,
                    ..
                } => {
                    if group_token == token_kind.closing_token().into() {
                        return;
                    } else {
                        break;
                    }
                }
                MatchRequest::Rule { parse_mode, .. } | MatchRequest::Token { parse_mode, .. } => {
                    skip(parse_mode)
                }
            }
        }

        panic!("parse stack should contain matching group token");
    }
}

impl Extend<EssentialRule> for ParseRequestStack {
    fn extend<T: IntoIterator<Item = EssentialRule>>(&mut self, iter: T) {
        for rule in iter {
            self.parse_requests
                .extend(rule.group_rule.group.map(|group| {
                    ParseRequest::Match(MatchRequest::Group {
                        token_kind: group.closing_token().into(),
                        parse_mode: GroupParseMode::Required,
                    })
                }));

            self.parse_requests
                .push(ParseRequest::Match(match rule.group_rule.rule {
                    Rule::Token(token) => MatchRequest::Token {
                        token_kind: token,
                        parse_mode: rule.into(),
                    },
                    Rule::Ref(rule_ref) => MatchRequest::Rule {
                        rule_ref,
                        parse_mode: rule.into(),
                    },
                }));

            self.parse_requests
                .extend(rule.group_rule.group.map(|group| {
                    ParseRequest::Match(MatchRequest::Group {
                        token_kind: group.opening_token().into(),
                        parse_mode: GroupParseMode::Essential,
                    })
                }));
        }
    }
}

impl Extend<RequiredRule> for ParseRequestStack {
    fn extend<T: IntoIterator<Item = RequiredRule>>(&mut self, iter: T) {
        for rule in iter {
            self.parse_requests
                .extend(rule.group_rule.group.map(|group| {
                    ParseRequest::Match(MatchRequest::Group {
                        token_kind: group.closing_token().into(),
                        parse_mode: GroupParseMode::Required,
                    })
                }));

            self.parse_requests
                .push(ParseRequest::Match(match rule.group_rule.rule {
                    Rule::Token(token) => MatchRequest::Token {
                        token_kind: token,
                        parse_mode: rule.into(),
                    },
                    Rule::Ref(rule_ref) => MatchRequest::Rule {
                        rule_ref,
                        parse_mode: rule.into(),
                    },
                }));

            self.parse_requests
                .extend(rule.group_rule.group.map(|group| {
                    ParseRequest::Match(MatchRequest::Group {
                        token_kind: group.opening_token().into(),
                        parse_mode: GroupParseMode::Required,
                    })
                }));
        }
    }
}

impl From<GlobalRule> for ParseRequest {
    fn from(value: GlobalRule) -> Self {
        ParseRequest::Match(match value.rule {
            Rule::Token(token) => MatchRequest::Token {
                token_kind: token,
                parse_mode: ParseMode::Repetition {
                    repeat_until: RepeatUntil::Eof,
                },
            },
            Rule::Ref(rule_ref) => MatchRequest::Rule {
                rule_ref,
                parse_mode: ParseMode::Repetition {
                    repeat_until: RepeatUntil::Eof,
                },
            },
        })
    }
}

impl From<EssentialRule> for ParseMode {
    fn from(value: EssentialRule) -> Self {
        match value.parse_mode {
            EssentialRuleMode::Essential => Self::Essential,
            EssentialRuleMode::Optional => Self::Optional,
            EssentialRuleMode::Repetition => Self::Repetition {
                repeat_until: match value.group_rule.group {
                    Some(group_kind) => RepeatUntil::ClosingGroup {
                        group_kind,
                        required: false,
                    },
                    None => RepeatUntil::Mismatch,
                },
            },
        }
    }
}

impl From<RequiredRule> for ParseMode {
    fn from(value: RequiredRule) -> Self {
        match value.parse_mode {
            RequiredRuleMode::Required => Self::Required,
            RequiredRuleMode::Optional => Self::Optional,
            RequiredRuleMode::Repetition => Self::Repetition {
                repeat_until: match value.group_rule.group {
                    Some(group_kind) => RepeatUntil::ClosingGroup {
                        group_kind,
                        required: true,
                    },
                    None => RepeatUntil::Mismatch,
                },
            },
        }
    }
}

impl From<AlternationRule> for ParseRequest {
    fn from(value: AlternationRule) -> Self {
        ParseRequest::Match(match value.rule {
            Rule::Token(token) => MatchRequest::Token {
                token_kind: token,
                parse_mode: ParseMode::Alternation,
            },
            Rule::Ref(rule_ref) => MatchRequest::Rule {
                rule_ref,
                parse_mode: ParseMode::Alternation,
            },
        })
    }
}

impl Extend<AlternationRule> for ParseRequestStack {
    fn extend<T: IntoIterator<Item = AlternationRule>>(&mut self, iter: T) {
        for rule in iter {
            self.parse_requests.push(rule.into());
        }
    }
}
