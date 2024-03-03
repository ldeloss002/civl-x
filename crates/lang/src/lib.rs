#![feature(trait_alias)]

pub mod proof;
#[cfg(feature = "smalltalk")]
pub mod smalltalk;

use std::{fmt, ops};

use chumsky::{error::Rich, extra, span::SimpleSpan};
pub use chumsky::{error::RichReason, input::Input, Parser};
pub use fraction;
use fraction::{BigDecimal, BigInt, BigUint};

pub type Span = SimpleSpan<usize, ()>;

pub type Error<'tokens, T> = Rich<'tokens, T, Span>;

pub enum Language {
    #[cfg(feature = "smalltalk")]
    Smalltalk,
}

pub fn lex_and_parse_for_language(
    src: &str,
    language: Language,
) -> (Option<Vec<Spanned<Expr>>>, Vec<Error<String>>) {
    match language {
        #[cfg(feature = "smalltalk")]
        Language::Smalltalk => {
            let (tokens, errs) = smalltalk::lexer().parse(src).into_output_errors();

            if let Some(tokens) = tokens.as_ref() {
                let len = src.chars().count();
                let (ast, parse_errors) = smalltalk::module_parser()
                    .parse(tokens.spanned((len..len + 1).into()))
                    .into_output_errors();

                let parse_errors = errs
                    .into_iter()
                    .map(|err| err.map_token(|ch| ch.to_string()))
                    .chain(
                        parse_errors
                            .into_iter()
                            .map(|err| err.map_token(|token| token.to_string())),
                    );

                for err in parse_errors {
                    eprintln!("{err}");
                }

                (ast, Vec::new())
            } else {
                (None, Vec::new())
            }
        }
    }
}

#[derive(Clone, Debug, PartialEq)]
pub enum Literal<'src> {
    Boolean(bool),
    Integer(BigInt),
    Float(BigDecimal),
    String(&'src str),
    Comment(&'src str),
    List(Vec<Self>),
}

impl<'src> std::fmt::Display for Literal<'src> {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        match self {
            Self::Boolean(x) => write!(f, "{x}"),
            Self::Integer(x) => write!(f, "{x}"),
            Self::Float(x, ..) => write!(f, "{x}"),
            Self::String(x) => write!(f, "{x}"),
            Self::Comment(x) => write!(f, "{x}"),
            Self::List(xs) => {
                write!(
                    f,
                    "[{}]",
                    xs.iter()
                        .map(|x| x.to_string())
                        .collect::<Vec<_>>()
                        .join(", ")
                )
            }
        }
    }
}

#[derive(Clone, Debug)]
pub enum BinaryOp {
    Add,
    Sub,
    Mul,
    Div,
    Equal,
    NotEqual,
    LessThan,
    LessThanOrEqual,
    GreaterThan,
    GreaterThanOrEqual,
}

#[derive(Debug, Clone)]
#[cfg_attr(feature = "serde", derive(serde::Serialize, serde::Deserialize))]
pub struct Spanned<T> {
    token: T,
    span: Span,
}

impl<T> Spanned<T> {
    pub fn new(inner: T, span: Span) -> Self {
        Self { token: inner, span }
    }

    pub fn span(&self) -> Span {
        self.span
    }

    pub fn into_deref(self) -> T {
        self.token
    }

    pub fn deref_spanned(&self) -> (&T, Span) {
        (&self.token, self.span)
    }

    pub fn into_deref_spanned(self) -> (T, Span) {
        (self.token, self.span)
    }

    pub fn map<U>(self, f: impl FnOnce(T) -> U) -> Spanned<U> {
        Spanned::new(f(self.token), self.span)
    }

    pub fn span_mut(&mut self) -> &mut Span {
        &mut self.span
    }
}

impl<T: fmt::Display> fmt::Display for Spanned<T> {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "{}", self.token)
    }
}

impl<T> ops::Deref for Spanned<T> {
    type Target = T;

    fn deref(&self) -> &Self::Target {
        &self.token
    }
}

impl<T> ops::DerefMut for Spanned<T> {
    fn deref_mut(&mut self) -> &mut Self::Target {
        &mut self.token
    }
}

#[derive(Debug)]
pub enum Expr<'src> {
    Error,
    Literal(Literal<'src>),
    List(Vec<Spanned<Self>>),
    Ident(Spanned<&'src str>),
    Binary(Box<Spanned<Self>>, BinaryOp, Box<Spanned<Self>>),
    Call(Box<Spanned<Self>>, Spanned<Vec<Spanned<Self>>>),
    If(Box<Spanned<Self>>, Box<Spanned<Self>>, Box<Spanned<Self>>),
    Function(Function<'src>),
}

#[derive(Debug)]
pub struct FunctionSignature<'src> {
    pub name: Spanned<&'src str>,
    pub args: Vec<Spanned<&'src str>>,
}

#[derive(Debug)]
pub struct Function<'src> {
    pub signature: Spanned<FunctionSignature<'src>>,
    pub body: Vec<Spanned<Expr<'src>>>,
}

#[derive(Clone, Debug, PartialEq)]
pub enum Token<'src> {
    Bool(bool),
    Int(BigInt),
    Float(BigDecimal),
    Str(&'src str),
    Op(&'src str),
    Ctrl(char),
    Ident(&'src str),
    Path(&'src str),
    Comment(&'src str),
    Fn,
    Let,
    Import,
}

impl<'src> fmt::Display for Token<'src> {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            Token::Bool(x) => write!(f, "{x}"),
            Token::Int(n) => write!(f, "{n}"),
            Token::Float(n, ..) => write!(f, "{n}"),
            Token::Str(s) => write!(f, "{s}"),
            Token::Op(s) => write!(f, "{s}"),
            Token::Ctrl(c) => write!(f, "{c}"),
            Token::Ident(s) => write!(f, "{s}"),
            Token::Path(s) => write!(f, "{s}"),
            Token::Fn => write!(f, "fn"),
            Token::Let => write!(f, "let"),
            Token::Import => write!(f, "import"),
            Token::Comment(s) => write!(f, "{s}"),
        }
    }
}

type ParserInput<'tokens, 'src> =
    chumsky::input::SpannedInput<Token<'src>, Span, &'tokens [(Token<'src>, Span)]>;

trait TokenParser<'tokens, 'src: 'tokens, I> = Parser<'tokens, ParserInput<'tokens, 'src>, Spanned<I>, extra::Err<Error<'tokens, Token<'src>>>>
    + Clone;
