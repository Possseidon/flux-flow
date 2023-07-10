use std::num::NonZeroUsize;

use thiserror::Error;

#[derive(Clone, Copy, Debug, Default)]
pub struct TokenStream {
    index: usize,
}

#[derive(Clone, Copy, Debug)]
pub struct Token {
    pub token_kind: TokenKind,
    pub len: NonZeroUsize,
}

#[derive(Debug, Error)]
pub enum LexError {
    #[error("invalid token: {0}")]
    InvalidToken(char),
    #[error("unterminated comment")]
    UnterminatedComment,
    #[error("unterminated string")]
    UnterminatedString,
    #[error("unterminated char")]
    UnterminatedChar,
}

impl TokenStream {
    /// Creates a new [`TokenStream`] referencing the given code.
    pub fn new() -> Self {
        Self::default()
    }

    /// Returns the byte offset where a [`TokenStream::next()`] call would be processed.
    pub fn index(self) -> usize {
        self.index
    }

    /// Returns a slice to all remaining characters that still need to be processed.
    pub fn rest(self, code: &str) -> &str {
        &code[self.index..]
    }

    /// Returns if this [`TokenStream`] reached the end of the code.
    pub fn is_at_end_of_code(self, code: &str) -> bool {
        self.rest(code).is_empty()
    }

    /// Returns the next [`Token`] without advancing the stream.
    pub fn peek(self, code: &str) -> Result<Option<Token>, LexError> {
        let rest = self.rest(code);
        if rest.is_empty() {
            Ok(None)
        } else if rest
            .chars()
            .next()
            .as_ref()
            .map_or(false, char::is_ascii_whitespace)
        {
            Ok(Some(Token {
                token_kind: TokenKind::Whitespace,
                len: NonZeroUsize::new(
                    rest.find(|c: char| !c.is_ascii_whitespace())
                        .unwrap_or(rest.len()),
                )
                .expect("token should not be empty"),
            }))
        } else if rest.starts_with("///") {
            Ok(Some(Token {
                token_kind: TokenKind::DocComment,
                len: NonZeroUsize::new(find_line_break(rest)).expect("token should not be empty"),
            }))
        } else if rest.starts_with("//") {
            Ok(Some(Token {
                token_kind: TokenKind::LineComment,
                len: NonZeroUsize::new(find_line_break(rest)).expect("token should not be empty"),
            }))
        } else if let Some(mut after_line_comment) = rest.strip_prefix("/*") {
            let mut nesting: usize = 0;
            loop {
                let len = after_line_comment
                    .find("*/")
                    .ok_or(LexError::UnterminatedComment)?;
                if let Some(open) = after_line_comment[0..len].find("/*") {
                    after_line_comment = &after_line_comment[(open + 2)..];
                    nesting += 1;
                } else {
                    after_line_comment = &after_line_comment[(len + 2)..];
                    if nesting == 0 {
                        break Ok(Some(Token {
                            token_kind: TokenKind::BlockComment,
                            len: NonZeroUsize::new(rest.len() - after_line_comment.len())
                                .expect("token should not be empty"),
                        }));
                    }
                    nesting -= 1;
                }
            }
        } else if let Some(after_initial_digit) = rest.strip_prefix(|c: char| c.is_ascii_digit()) {
            if let Some(after_base) =
                after_initial_digit.strip_prefix(['B', 'b', 'O', 'o', 'X', 'x'])
            {
                let end =
                    after_base.trim_start_matches(|c: char| c.is_ascii_alphanumeric() || c == '_');
                Ok(Some(Token {
                    token_kind: TokenKind::Integer,
                    len: NonZeroUsize::new(rest.len() - end.len())
                        .expect("token should not be empty"),
                }))
            } else {
                let mut token_kind = TokenKind::Integer;
                let mut after_digits = after_initial_digit
                    .trim_start_matches(|c: char| c.is_ascii_digit() || c == '_');

                if let Some(after_period) = after_digits.strip_prefix('.') {
                    token_kind = TokenKind::Float;
                    after_digits =
                        after_period.trim_start_matches(|c: char| c.is_ascii_digit() || c == '_');
                }

                if let Some(after_exponent) = after_digits.strip_prefix(['E', 'e']) {
                    token_kind = TokenKind::Float;
                    after_digits = after_exponent
                        .strip_prefix(['+', '-'])
                        .unwrap_or(after_exponent)
                        .trim_start_matches(|c: char| c.is_ascii_digit() || c == '_');
                }

                let end = after_digits
                    .trim_start_matches(|c: char| c.is_ascii_alphanumeric() || c == '_');

                Ok(Some(Token {
                    token_kind,
                    len: NonZeroUsize::new(rest.len() - end.len())
                        .expect("token should not be empty"),
                }))
            }
        } else if let Some(after_quote) = rest.strip_prefix('\'') {
            // Implementing label detection can be kindof hard to wrap your head around.

            // Examples for valid chars and labels:

            // input | ident | ' after
            //       | chars | ident
            // ------+-------+--------
            // ''    |     0 | true
            // 'a'   |     1 | true
            // 'ab'  |     2 | true
            // '?'   |     0 | false
            // ------+-------+--------
            // 'a    |     1 | false
            // 'ab   |     2 | false

            // Truth table based on the examples above:

            //   | true | false
            // --+------+------
            // 0 | char | char
            // 1 | char | label
            // 2 | char | label

            // Reading from the truth table:
            // is_char = (ident chars) < 1 || (' after ident)

            let after_ident_chars =
                after_quote.trim_start_matches(|c: char| c.is_ascii_alphanumeric() || c == '_');

            let ident_chars = after_quote.len() - after_ident_chars.len();
            let quote_after_ident = after_ident_chars.starts_with('\'');

            if ident_chars < 1 || quote_after_ident {
                let end = lex_quoted(after_ident_chars, '\'', LexError::UnterminatedChar)?;
                Ok(Some(Token {
                    token_kind: TokenKind::Char,
                    len: NonZeroUsize::new(rest.len() - end.len()).unwrap(),
                }))
            } else {
                Ok(Some(Token {
                    token_kind: TokenKind::Label,
                    len: NonZeroUsize::new(rest.len() - after_ident_chars.len()).unwrap(),
                }))
            }
        } else if let Some(after_quote) = rest.strip_prefix("b'") {
            let end = lex_quoted(after_quote, '\'', LexError::UnterminatedChar)?;
            Ok(Some(Token {
                token_kind: TokenKind::Char,
                len: NonZeroUsize::new(rest.len() - end.len()).unwrap(),
            }))
        } else if let Some(after_quote) = rest.strip_prefix('"') {
            let end = lex_quoted(after_quote, '\"', LexError::UnterminatedString)?;
            Ok(Some(Token {
                token_kind: TokenKind::String,
                len: NonZeroUsize::new(rest.len() - end.len()).unwrap(),
            }))
        } else if let Some(after_quote) = rest.strip_prefix("b\"") {
            let end = lex_quoted(after_quote, '\"', LexError::UnterminatedString)?;
            Ok(Some(Token {
                token_kind: TokenKind::String,
                len: NonZeroUsize::new(rest.len() - end.len()).unwrap(),
            }))
        } else if rest.starts_with("r#") || rest.starts_with("r\"") {
            let end = lex_raw_string(&rest[1..])?;
            Ok(Some(Token {
                token_kind: TokenKind::String,
                len: NonZeroUsize::new(rest.len() - end.len()).unwrap(),
            }))
        } else if rest.starts_with("br#") || rest.starts_with("br\"") {
            let end = lex_raw_string(&rest[2..])?;
            Ok(Some(Token {
                token_kind: TokenKind::String,
                len: NonZeroUsize::new(rest.len() - end.len()).unwrap(),
            }))
        } else if rest.starts_with(|c: char| c.is_ascii_alphabetic() || c == '_') {
            let len = NonZeroUsize::new(
                rest.find(|c: char| !c.is_ascii_alphanumeric() && c != '_')
                    .unwrap_or(rest.len()),
            )
            .expect("token should not be empty");
            if let Some(keyword) = self.keyword_token(&rest[0..len.get()]) {
                Ok(Some(keyword))
            } else {
                Ok(Some(Token {
                    token_kind: TokenKind::Ident,
                    len,
                }))
            }
        } else if let Some(token) = self.punctuation_token(code) {
            Ok(Some(token))
        } else {
            self.ascii_token(code).map(Some)
        }
    }

    /// Skips over all consecutive whitespace tokens, returning the number of skipped bytes.
    pub fn skip_whitespace_tokens(&mut self, code: &str) -> usize {
        let before = self.index;
        // TODO: use && is_whitespace_token(token.token_kind) once stabilized
        while let Ok(Some(token)) = self.peek(code) {
            if !is_whitespace_token(token.token_kind) {
                break;
            }
            self.advance_token(token);
        }
        self.index - before
    }

    /// Advances over the given token, returning the number of advanced bytes.
    pub fn advance_token(&mut self, token: Token) -> NonZeroUsize {
        self.index += token.len.get();
        token.len
    }

    /// Advances over the given error, returning the number of advanced bytes.
    pub fn advance_error(&mut self, code: &str, error: &LexError) -> NonZeroUsize {
        let old_index = self.index;
        match error {
            LexError::InvalidToken(char) => self.index += char.len_utf8(),
            LexError::UnterminatedComment
            | LexError::UnterminatedString
            | LexError::UnterminatedChar => self.index = code.len(),
        };
        NonZeroUsize::new(self.index - old_index).expect("error should have advanced")
    }

    fn keyword_token(self, ident: &str) -> Option<Token> {
        macro_rules! tokens {
            ( $( $string:literal => $TK:ident, )* ) => {
                $( if ident == $string {
                    const LEN: NonZeroUsize = new_non_zero_usize::<{ $string.len() }>();
                    Some(Token { token_kind: TokenKind::$TK, len: LEN })
                } else )+ {
                    None
                }
            };
        }

        tokens! {
            "_" => Underscore,
            "as" => As,
            "break" => Break,
            "continue" => Continue,
            "crate" => Crate,
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
            "mod" => Mod,
            "pub" => Pub,
            "return" => Return,
            "self" => SelfValue,
            "Self" => SelfType,
            "struct" => Struct,
            "super" => Super,
            "trait" => Trait,
            "true" => True,
            "type" => Type,
            "use" => Use,
            "while" => While,
        }
    }

    fn punctuation_token(self, code: &str) -> Option<Token> {
        let current = self.rest(code);

        macro_rules! check_punctutation_tokens {
            ( $( $string:literal => $TK:ident, )* ) => {
                $( if current.starts_with($string) {
                    const LEN: NonZeroUsize = new_non_zero_usize::<{ $string.len() }>();
                    Some(Token { token_kind: TokenKind::$TK, len: LEN })
                } else )+ {
                    None
                }
            };
        }

        check_punctutation_tokens! {
            "..." => DotDotDot,
            "..=" => DotDotEq,
            "<<=" => ShlEq,
            ">>=" => ShrEq,

            "-=" => MinusEq,
            "->" => RArrow,
            "::" => PathSep,
            "!=" => Ne,
            ".." => DotDot,
            ".=" => DotEq,
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
    }

    fn ascii_token(self, code: &str) -> Result<Token, LexError> {
        let char = self.rest(code).chars().next().expect("should not be empty");
        if let Ok(ascii_char) = u8::try_from(char) {
            if ascii_char > b' ' {
                if let Some(token_kind) = TOKEN_MAP
                    .get((ascii_char - b' ' - 1) as usize)
                    .copied()
                    .unwrap_or(None)
                {
                    return Ok(Token {
                        token_kind,
                        len: new_non_zero_usize::<1>(),
                    });
                }
            }
        }
        Err(LexError::InvalidToken(char))
    }
}

fn lex_quoted(
    mut after_quote: &str,
    quote: char,
    unterminated_error: LexError,
) -> Result<&str, LexError> {
    loop {
        after_quote = after_quote.trim_start_matches(|c: char| c != quote && c != '\\');
        if let Some(end) = after_quote.strip_prefix(quote) {
            break Ok(end);
        }
        after_quote = match after_quote
            .trim_start_matches(|c: char| c != '\\')
            .strip_prefix(|_| true)
        {
            Some(after_escape) => after_escape,
            None => return Err(unterminated_error),
        };
    }
}

fn lex_raw_string(after_r: &str) -> Result<&str, LexError> {
    let after_pound = after_r.trim_matches('#');
    let start_pounds = &after_r[0..(after_r.len() - after_pound.len())];
    let mut after_quote = after_pound.strip_prefix('"').unwrap_or(after_pound);
    loop {
        after_quote = after_quote.trim_start_matches(|c: char| c != '"');
        if let Some(end_pounds) = after_quote.strip_prefix('"') {
            if let Some(end) = end_pounds.strip_prefix(start_pounds) {
                break Ok(end);
            }
            after_quote = end_pounds;
        } else {
            break Err(LexError::UnterminatedString);
        }
    }
}

fn find_line_break(current: &str) -> usize {
    let bytes = current.as_bytes();

    let line_break = bytes
        .iter()
        .enumerate()
        .find_map(|(line, byte)| [b'\n', b'\r'].contains(byte).then_some(line))
        .unwrap_or(bytes.len());

    match current.as_bytes().get(line_break + 1) {
        Some(b'\n') => line_break + 1,
        _ => line_break,
    }
}

fn is_whitespace_token(token_kind: TokenKind) -> bool {
    matches!(
        token_kind,
        TokenKind::Whitespace | TokenKind::LineComment | TokenKind::BlockComment
    )
}

const fn new_non_zero_usize<const VALUE: usize>() -> NonZeroUsize {
    if let Some(value) = NonZeroUsize::new(VALUE) {
        value
    } else {
        panic!("value should be non-zero")
    }
}

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum BraceKind {
    Paren,
    Brack,
    Curly,
}

impl BraceKind {
    pub fn name(self) -> &'static str {
        match self {
            Self::Paren => "`()`",
            Self::Brack => "`[]`",
            Self::Curly => "`{}`",
        }
    }

    pub fn opening_name(self) -> &'static str {
        match self {
            Self::Paren => "`(`",
            Self::Brack => "`[`",
            Self::Curly => "`{`",
        }
    }

    pub fn closing_name(self) -> &'static str {
        match self {
            Self::Paren => "`)`",
            Self::Brack => "`]`",
            Self::Curly => "`}`",
        }
    }

    pub fn opening_token(&self) -> TokenKind {
        match self {
            Self::Paren => TokenKind::LParen,
            Self::Brack => TokenKind::LBrack,
            Self::Curly => TokenKind::LCurly,
        }
    }

    pub fn closing_token(&self) -> TokenKind {
        match self {
            Self::Paren => TokenKind::RParen,
            Self::Brack => TokenKind::RBrack,
            Self::Curly => TokenKind::RCurly,
        }
    }
}

macro_rules! tokens {
    ( $( $Variant:ident = $name:literal, )* ) => {
        #[derive(Clone, Copy, Debug, PartialEq, Eq)]
        pub enum TokenKind {
            $( $Variant, )*
        }

        impl TokenKind {
            pub fn name(self) -> &'static str {
                match self {
                    $( Self::$Variant => $name, )*
                }
            }
        }
    };
}

tokens! {
    Whitespace = "whitespace",
    LineComment = "line comment",
    BlockComment = "block comment",
    DocComment = "doc comment",

    Ident = "identifier",
    Integer = "integer",
    Float = "float",
    String = "string",
    Char = "char",
    Label = "label",

    LParen = "`(`",
    RParen = "`)`",
    LBrack = "`[`",
    RBrack = "`]`",
    LCurly = "`{`",
    RCurly = "`}`",
    Plus = "`+`",
    Minus = "`-`",
    Star = "`*`",
    Slash = "`/`",
    Percent = "`%`",
    Caret = "`^`",
    Not = "`!`",
    And = "`&`",
    Or = "`|`",
    AndAnd = "`&&`",
    OrOr = "`||`",
    Shl = "`<<`",
    Shr = "`>>`",
    PlusEq = "`+=`",
    MinusEq = "`-=`",
    StarEq = "`*=`",
    SlashEq = "`/=`",
    PercentEq = "`%=`",
    CaretEq = "`^=`",
    AndEq = "`&=`",
    OrEq = "`|=`",
    ShlEq = "`<<=`",
    ShrEq = "`>>=`",
    Eq = "`=`",
    EqEq = "`==`",
    Ne = "`!=`",
    Gt = "`>`",
    Lt = "`<`",
    Ge = "`>=`",
    Le = "`<=`",
    At = "`@`",
    Underscore = "`_`",
    Dot = "`.`",
    DotDot = "`..`",
    DotEq = "`.=`",
    DotDotDot = "`...`",
    DotDotEq = "`..=`",
    Comma = "`,`",
    Semi = "`;`",
    Colon = "`:`",
    PathSep = "`::`",
    RArrow = "`->`",
    FatArrow = "`=>`",
    Pound = "`#`",
    Dollar = "`$`",
    Question = "`?`",
    Tilde = "`~`",

    As = "`as`",
    Break = "`break`",
    Continue = "`continue`",
    Crate = "`crate`",
    Else = "`else`",
    False = "`false`",
    Fn = "`fn`",
    For = "`for`",
    If = "`if`",
    Impl = "`impl`",
    In = "`in`",
    Let = "`let`",
    Loop = "`loop`",
    Match = "`match`",
    Mod = "`mod`",
    Pub = "`pub`",
    Return = "`return`",
    SelfValue = "`self`",
    SelfType = "`Self`",
    Struct = "`struct`",
    Super = "`super`",
    Trait = "`trait`",
    True = "`true`",
    Type = "`type`",
    Use = "`use`",
    While = "`while`",
}

pub const TOKEN_MAP: &[Option<TokenKind>] = &[
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

#[cfg(test)]
mod tests {
    use super::*;

    fn assert_empty(code: &str, token_stream: TokenStream) {
        let Ok(None) = token_stream.peek(code) else {
            panic!("token stream should be empty")
        };
    }

    fn assert_token_and_advance(code: &str, token_stream: &mut TokenStream) -> Token {
        let index_before = token_stream.index;

        let Ok(Some(token)) = token_stream.peek(code) else {
            panic!("token expected")
        };

        let advanced = token_stream.advance_token(token);
        assert_eq!(advanced, token.len);
        assert_eq!(token_stream.index, index_before + advanced.get());

        token
    }

    #[test]
    fn token_stream_initial_state() {
        let token_stream = TokenStream::new();

        assert_eq!(token_stream.index, 0);
    }

    #[test]
    fn token_stream_empty() {
        let code = "";
        let token_stream = TokenStream::new();

        assert_empty(code, token_stream);
    }

    #[test]
    fn token_stream_with_ident() {
        let code = "test";
        let mut token_stream = TokenStream::new();

        let token = assert_token_and_advance(code, &mut token_stream);

        const LEN: NonZeroUsize = new_non_zero_usize::<4>();
        assert!(matches!(
            token,
            Token {
                len: LEN,
                token_kind: TokenKind::Ident
            },
        ));

        assert_empty(code, token_stream);
    }
}
