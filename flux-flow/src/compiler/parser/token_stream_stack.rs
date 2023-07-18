use std::ops::Range;

use crate::compiler::lexer::{self, GroupKind, GroupToken, TokenKind, TokenStream};

#[derive(Clone, Debug)]
pub struct TokenStreamStack {
    token_streams: Vec<TokenStream>,
    groups: Vec<GroupInfo>,
}

pub struct TokenInfo {
    pub range: Range<usize>,
    pub trailing_whitespace_len: usize,
}

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub struct GroupInfo {
    /// What kind of group this is.
    pub kind: GroupKind,
    pub code_index: usize,
    // /// The index of the current parse request for unrolling.
    // pub parse_request_index: usize,
    // /// The current index of the node builder input for unrolling.
    // pub node_builder_index: usize,
}

impl TokenStreamStack {
    pub fn new(token_stream: TokenStream) -> TokenStreamStack {
        TokenStreamStack {
            token_streams: vec![token_stream],
            groups: vec![],
        }
    }

    pub fn current(&self) -> TokenStream {
        *self
            .token_streams
            .last()
            .expect("token streams should not be empty")
    }

    pub fn advance_token(&mut self, code: &str, token: lexer::Token) -> TokenInfo {
        let current = self
            .token_streams
            .last_mut()
            .expect("token streams should not be empty");

        let code_index = current.code_index();

        if let TokenKind::Group(group_token) = token.kind {
            match group_token {
                GroupToken::Opening(group_token) => {
                    self.groups.push(GroupInfo {
                        kind: group_token.kind(),
                        code_index,
                    });
                }
                GroupToken::Closing(group_token) => {
                    if let Some((index, _)) = self
                        .groups
                        .iter()
                        .enumerate()
                        .rev()
                        .find(|(_, group_info)| group_token.kind() == group_info.kind)
                    {
                        self.groups.drain(index..);
                    }
                }
            }
        }

        current.advance_token(token);

        TokenInfo {
            range: code_index..current.code_index(),
            trailing_whitespace_len: current.skip_whitespace_tokens(code),
        }
    }

    pub fn push(&mut self) {
        self.token_streams.push(
            *self
                .token_streams
                .last()
                .expect("token streams should not be empty"),
        );
    }

    pub fn commit(&mut self) {
        self.token_streams.remove(self.token_streams.len() - 2);
    }

    pub fn revert(&mut self) {
        self.token_streams.pop();
        let code_index = self.current().code_index();
        while let Some(group_info) = self.groups.last().copied() {
            if group_info.code_index < code_index {
                break;
            }
            self.groups.pop();
        }
    }

    pub fn validate(mut self, code: &str) {
        let token_stream = self
            .token_streams
            .pop()
            .expect("token streams should not be empty after parsing");
        assert!(token_stream.is_at_end_of_code(code));
        assert!(self.token_streams.is_empty());
    }

    pub fn contains_group(&self, kind: GroupKind) -> bool {
        self.groups.iter().any(|group_info| group_info.kind == kind)
    }

    pub fn contains_group_before_but_not_after(&self, kind: GroupKind, depth: usize) -> bool {
        self.groups[..depth]
            .iter()
            .any(|group_info| group_info.kind == kind)
            && !self.groups[depth..]
                .iter()
                .any(|group_info| group_info.kind == kind)
    }

    pub fn current_group_kind(&self) -> Option<GroupKind> {
        self.groups.last().map(|group_kind| group_kind.kind)
    }

    pub fn visualize(&self) {
        print!("TokenStreams at");
        for token_stream in &self.token_streams {
            print!(" {}", token_stream.code_index());
        }
        println!();

        print!("Groups:");
        for group in &self.groups {
            print!(" ({} at {})", group.kind.opening_name(), group.code_index);
        }
        println!();
    }

    pub fn advance_error(&mut self, code: &str, error: &lexer::LexError) -> (usize, usize) {
        let current = self
            .token_streams
            .last_mut()
            .expect("token streams should not be empty");

        let index = current.code_index();
        let end = index + current.advance_error(code, error).get();
        current.skip_whitespace_tokens(code);

        (index, end)
    }

    pub fn group_depth(&self) -> usize {
        self.groups.len()
    }
}
