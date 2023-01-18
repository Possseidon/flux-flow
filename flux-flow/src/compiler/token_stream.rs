macro_rules! tokens {
    ( $( $Variant:ident($TK:ident), )* ) => {
        pub(crate) type TokenIndex = u8;

        #[derive(Clone, Copy, Debug, Hash, PartialEq, Eq, PartialOrd, Ord)]
        pub enum TokenKind {
            $( $Variant, )*
        }

        #[derive(Debug)]
        pub struct InvalidTokenIndex;

        impl TryFrom<TokenIndex> for TokenKind {
            type Error = InvalidTokenIndex;

            fn try_from(value: TokenIndex) -> Result<Self, Self::Error> {
                match value {
                    $( tokens::$TK => Ok(Self::$Variant), )*
                    _ => Err(InvalidTokenIndex),
                }
            }
        }

        pub(crate) mod tokens {
            use super::{TokenKind, TokenIndex};

            $( pub(crate) const $TK: TokenIndex = TokenKind::$Variant as TokenIndex; )*
        }
    };
}

tokens! {
    Invalid(TK_INVALID),

    Whitespace(TK_WHITESPACE),
    LineComment(TK_LINE_COMMENT),
    BlockComment(TK_BLOCK_COMMENT),
    DocComment(TK_DOC_COMMENT),

    Ident(TK_IDENT),
    Label(TK_LABEL),
    // String_Body(TK_STRING_BODY),
    // Escaped_Char(TK_ESCAPED_CHAR),
    // Label(TK_LABEL),

    LParen(TK_L_PAREN),
    RParen(TK_R_PAREN),
    LBrack(TK_L_BRACK),
    RBrack(TK_R_BRACK),
    LCurly(TK_L_CURLY),
    RCurly(TK_R_CURLY),
    Plus(TK_PLUS),
    Minus(TK_MINUS),
    Star(TK_STAR),
    Slash(TK_SLASH),
    Percent(TK_PERCENT),
    Caret(TK_CARET),
    Not(TK_NOT),
    And(TK_AND),
    Or(TK_OR),
    AndAnd(TK_AND_AND),
    OrOr(TK_OR_OR),
    Shl(TK_SHL),
    Shr(TK_SHR),
    PlusEq(TK_PLUS_EQ),
    MinusEq(TK_MINUS_EQ),
    StarEq(TK_STAR_EQ),
    SlashEq(TK_SLASH_EQ),
    PercentEq(TK_PERCENT_EQ),
    CaretEq(TK_CARET_EQ),
    AndEq(TK_AND_EQ),
    OrEq(TK_OR_EQ),
    ShlEq(TK_SHL_EQ),
    ShrEq(TK_SHR_EQ),
    Eq(TK_EQ),
    EqEq(TK_EQ_EQ),
    Ne(TK_NE),
    Gt(TK_GT),
    Lt(TK_LT),
    Ge(TK_GE),
    Le(TK_LE),
    At(TK_AT),
    Underscore(TK_UNDERSCORE),
    Dot(TK_DOT),
    DotDot(TK_DOT_DOT),
    DotDotDot(TK_DOT_DOT_DOT),
    DotDotEq(TK_DOT_DOT_EQ),
    Comma(TK_COMMA),
    Semi(TK_SEMI),
    Colon(TK_COLON),
    PathSep(TK_PATH_SEP),
    RArrow(TK_R_ARROW),
    FatArrow(TK_FAT_ARROW),
    Pound(TK_POUND),
    Dollar(TK_DOLLAR),
    Question(TK_QUESTION),
    Tilde(TK_TILDE),

    Break(KW_BREAK),
    Continue(KW_CONTINUE),
    Else(KW_ELSE),
    False(KW_FALSE),
    Fn(KW_FN),
    For(KW_FOR),
    If(KW_IF),
    Impl(KW_IMPL),
    In(KW_IN),
    Let(KW_LET),
    Loop(KW_LOOP),
    Match(KW_MATCH),
    Pub(KW_PUB),
    Return(KW_RETURN),
    SelfValue(KW_SELF_VALUE),
    SelfType(KW_SELF_TYPE),
    Struct(KW_STRUCT),
    Trait(KW_TRAIT),
    True(KW_TRUE),
    Type(KW_TYPE),
    While(KW_WHILE),
}

pub(crate) const TOKEN_MAP: &[Option<TokenKind>] = &[
    Some(TokenKind::Not),
    None, // "
    Some(TokenKind::Pound),
    Some(TokenKind::Dollar),
    Some(TokenKind::Percent),
    Some(TokenKind::And),
    None, // '
    Some(TokenKind::LParen),
    Some(TokenKind::RParen),
    Some(TokenKind::Star),
    Some(TokenKind::Plus),
    Some(TokenKind::Comma),
    Some(TokenKind::Minus),
    Some(TokenKind::Dot),
    Some(TokenKind::Slash),
    None, // 0
    None, // 1
    None, // 2
    None, // 3
    None, // 4
    None, // 5
    None, // 6
    None, // 7
    None, // 8
    None, // 9
    Some(TokenKind::Colon),
    Some(TokenKind::Semi),
    Some(TokenKind::Lt),
    Some(TokenKind::Eq),
    Some(TokenKind::Gt),
    Some(TokenKind::Question),
    Some(TokenKind::At),
    None, // A
    None, // B
    None, // C
    None, // D
    None, // E
    None, // F
    None, // G
    None, // H
    None, // I
    None, // J
    None, // K
    None, // L
    None, // M
    None, // N
    None, // O
    None, // P
    None, // Q
    None, // R
    None, // S
    None, // T
    None, // U
    None, // V
    None, // W
    None, // X
    None, // Y
    None, // Z
    Some(TokenKind::LBrack),
    None, // \
    Some(TokenKind::RBrack),
    Some(TokenKind::Caret),
    Some(TokenKind::Underscore),
    None, // `
    None, // A
    None, // B
    None, // C
    None, // D
    None, // E
    None, // F
    None, // G
    None, // H
    None, // I
    None, // J
    None, // K
    None, // L
    None, // M
    None, // N
    None, // O
    None, // P
    None, // Q
    None, // R
    None, // S
    None, // T
    None, // U
    None, // V
    None, // W
    None, // X
    None, // Y
    None, // Z
    Some(TokenKind::LCurly),
    Some(TokenKind::Or),
    Some(TokenKind::RCurly),
    Some(TokenKind::Tilde),
];

fn is_whitespace_token(kind: TokenKind) -> bool {
    matches!(
        kind,
        TokenKind::Whitespace
            | TokenKind::LineComment
            | TokenKind::BlockComment
            | TokenKind::DocComment
    )
}

#[derive(Clone, Copy, Debug, Hash, PartialEq, Eq, PartialOrd, Ord)]
pub struct Token {
    pub kind: TokenKind,
    pub position: usize,
    pub len: usize,
}

#[derive(Clone, Copy, Debug, Hash, PartialEq, Eq, PartialOrd, Ord)]
pub enum TokenizationErrorKind {
    UnterminatedComment,
    UnterminatedString,
}

#[derive(Clone, Copy, Debug, Hash, PartialEq, Eq, PartialOrd, Ord)]
pub struct TokenizationError {
    pub kind: TokenizationErrorKind,
    pub position: usize,
}

// #[derive(Clone, Copy, Debug, Hash, PartialEq, Eq, PartialOrd, Ord)]
// enum State {
//     CommentNesting(usize),
//     String,
//     Char,
// }

#[derive(Clone, Copy, Debug, Default, Hash, PartialEq, Eq, PartialOrd, Ord)]
pub struct TokenStream<'a> {
    code: &'a str,
    position: usize,
    // state: Option<State>,
}

impl<'a> TokenStream<'a> {
    pub fn new(code: &'a str) -> Self {
        Self {
            code,
            position: 0,
            // state: None,
        }
    }

    pub fn position(&self) -> usize {
        self.position
    }

    fn current(&self) -> &str {
        &self.code[self.position..]
    }

    pub fn next_non_whitespace_token(mut self) -> Option<Result<(Token, Self), TokenizationError>> {
        loop {
            match self.next_token() {
                Some(result) => {
                    if let Ok((token, ts)) = result {
                        if is_whitespace_token(token.kind) {
                            self = ts;
                            continue;
                        }
                    }
                    break Some(result);
                }
                None => break None,
            }
        }
    }

    pub fn next_token(self) -> Option<Result<(Token, Self), TokenizationError>> {
        let new_error = |kind| {
            Err(TokenizationError {
                kind,
                position: self.position,
            })
        };

        let current = self.current();
        if current.is_empty() {
            return None;
        }

        let new_token = |kind, len| Token {
            kind,
            position: self.position,
            len,
        };

        macro_rules! check_basic_tokens {
            ( $current:ident $( $string:literal => $TK:ident, )* ) => {
                $( if $current.starts_with($string) {
                    const LEN: usize = $string.len();
                    Some(new_token(TokenKind::$TK, LEN))
                } else )+ {
                    None
                }
            };
        }

        let check_keyword_tokens = |current: &str| {
            check_basic_tokens! { current
                "break" => Break,
                "continue" => Continue,
                "else" => Else,
                "false" => False,
                "fn" => Fn,
                "for" => For,
                "if" => If,
                "impl" => Impl,
                "in" => In,
                "let" => Let,
                "loop" => Loop,
                "match" => Match,
                "pub" => Pub,
                "return" => Return,
                "self" => SelfValue,
                "Self" => SelfType,
                "struct" => Struct,
                "trait" => Trait,
                "true" => True,
                "type" => Type,
                "while" => While,
            }
        };

        let check_basic_tokens = |current: &str| {
            check_basic_tokens! { current
                "..." => DotDotDot,
                "..=" => DotDotEq,
                "<<=" => ShlEq,
                ">>=" => ShrEq,

                "-=" => MinusEq,
                "->" => RArrow,
                "::" => PathSep,
                "!=" => Ne,
                ".." => DotDot,
                "*=" => StarEq,
                "/=" => SlashEq,
                "&&" => AndAnd,
                "&=" => AndEq,
                "%=" => PercentEq,
                "^=" => CaretEq,
                "+=" => PlusEq,
                "<<" => Shl,
                "<=" => Le,
                "==" => EqEq,
                "=>" => FatArrow,
                ">=" => Ge,
                ">>" => Shr,
                "|=" => OrEq,
                "||" => OrOr,
            }
        };

        let ascii_token = |char: char| {
            if let Ok(ascii_char) = u8::try_from(char) {
                if ascii_char > b' ' {
                    if let Some(token) = TOKEN_MAP
                        .get((ascii_char - b' ' - 1) as usize)
                        .copied()
                        .unwrap_or(None)
                    {
                        return new_token(token, 1);
                    }
                }
            }
            new_token(TokenKind::Invalid, char.len_utf8())
        };

        let is_ident_char = |char: char| char.is_ascii_alphanumeric() || char == '_';

        let result = if current.starts_with(['\t', '\n', '\x0C', '\r', ' ']) {
            Ok(new_token(
                TokenKind::Whitespace,
                current
                    .find(|char: char| !char.is_ascii_whitespace())
                    .unwrap_or(current.len()),
            ))
        } else if current.starts_with("///") {
            Ok(new_token(
                TokenKind::DocComment,
                current.find('\n').unwrap_or(current.len()),
            ))
        } else if current.starts_with("//") {
            Ok(new_token(
                TokenKind::LineComment,
                current.find('\n').unwrap_or(current.len()),
            ))
        } else if let Some(mut line_comment) = current.strip_prefix("/*") {
            let mut nesting: usize = 1;
            loop {
                if let Some(end) = line_comment.find("*/") {
                    if let Some(open) = line_comment[0..end].find("/*") {
                        nesting += 1;
                        line_comment = &line_comment[(open + 2)..];
                    } else {
                        nesting -= 1;
                        if nesting == 0 {
                            break Ok(new_token(TokenKind::BlockComment, end + 2));
                        }
                    }
                } else {
                    break new_error(TokenizationErrorKind::UnterminatedComment);
                }
            }
        } else if current.starts_with(is_ident_char) {
            let len = current
                .find(|char| !is_ident_char(char))
                .unwrap_or(current.len());
            if let Some(keyword) = check_keyword_tokens(&current[0..len]) {
                Ok(keyword)
            } else {
                Ok(new_token(TokenKind::Ident, len))
            }
        } else if let Some(token) = check_basic_tokens(current) {
            Ok(token)
        } else {
            Ok(ascii_token(
                current.chars().next().expect("should not be empty"),
            ))
        };

        Some(result.map(|token| {
            let mut ts = self;
            ts.position += token.len;
            (token, ts)
        }))
    }
}
