//! Proofs are defined as `{-@ condition @-}`
//! where `condition` is some relation between variables and constants.

use std::{
    collections::HashMap,
    fmt,
    ops::{self, Neg},
    str::FromStr,
};

use chumsky::{error::RichReason, input::Input, prelude::*, Parser};
use fraction::{BigDecimal, BigInt, BigUint, FromPrimitive};

use crate::{
    BinaryOp, Error, Expr, Function, FunctionSignature, Literal, ParserInput, Span, Spanned, Token,
    TokenParser,
};

pub fn lexer<'src>()
-> impl Parser<'src, &'src str, Vec<(Token<'src>, Span)>, extra::Err<Rich<'src, char, Span>>> {
    let num = just('-')
        .or_not()
        .then(text::int(10))
        .then(just('.').ignore_then(text::digits(10)).to_slice().or_not())
        .map(
            |((negative, before_decimal), after_decimal): ((_, _), Option<&str>)| {
                let is_negative = negative.is_some();

                if let Some(after_decimal) = after_decimal {
                    let float =
                        BigDecimal::from_str(&format!("{before_decimal}{after_decimal}")).unwrap();
                    let signed_float = if is_negative { float.neg() } else { float };

                    let precision = BigUint::from_u32(after_decimal.len() as u32 - 1).unwrap();

                    Token::Float(signed_float)
                } else {
                    let int = BigInt::from_str(before_decimal).unwrap();
                    let signed_int = if is_negative { int.neg() } else { int };

                    Token::Int(signed_int)
                }
            },
        );

    let string = just('"')
        .ignore_then(none_of('\'').repeated())
        .then_ignore(just('\''))
        .to_slice()
        .map(Token::Str);

    let comment = just('"')
        .ignore_then(none_of('"').repeated())
        .then_ignore(just('"'))
        .to_slice()
        .map(Token::Str);

    let op = one_of("+*-/!=<>")
        .repeated()
        .at_least(1)
        .to_slice()
        .map(Token::Op);

    let ctrl = one_of("()[]{};,@").map(Token::Ctrl);

    let ident = any()
        .filter(|c: &char| c.is_ascii_alphabetic() || *c == '_')
        .then(
            any()
                .filter(|c: &char| c.is_ascii_alphanumeric() || *c == '_' || *c == '-')
                .repeated(),
        )
        .to_slice()
        .map(|ident: &str| {
            match ident {
                "fn" => Token::Fn,
                "let" => Token::Let,
                "true" => Token::Bool(true),
                "false" => Token::Bool(false),
                "import" => Token::Import,
                _ => Token::Ident(ident),
            }
        });

    let token = num.or(string).or(comment).or(op).or(ctrl).or(ident);

    let comment = just("//")
        .then(any().and_is(just('\n').not()).repeated())
        .padded();

    token
        .map_with(|token, e| (token, e.span()))
        .padded_by(comment.repeated())
        .padded()
        .recover_with(skip_then_retry_until(any().ignored(), end()))
        .repeated()
        .collect()
}

pub fn proof_parser<'tokens, 'src: 'tokens>() -> impl Parser<
    'tokens,
    ParserInput<'tokens, 'src>,
    Vec<Spanned<Expr<'src>>>,
    extra::Err<Rich<'tokens, Token<'src>, Span>>,
> + Clone {
    let exprs = expr_parser()
        .repeated()
        .collect()
        .delimited_by(just(Token::Ctrl('@')), just(Token::Ctrl('@')))
        .delimited_by(just(Token::Op("-")), just(Token::Op("-")))
        .delimited_by(just(Token::Ctrl('{')), just(Token::Ctrl('}')));

    exprs
}

fn ident<'tokens, 'src: 'tokens>() -> impl TokenParser<'tokens, 'src, &'src str> {
    select! { Token::Ident(ident) => ident }
        .map_with(|ident: &str, e| Spanned::new(ident, e.span()))
        .labelled("identifier")
}

fn expr_parser<'tokens, 'src: 'tokens>() -> impl TokenParser<'tokens, 'src, Expr<'src>> {
    let ident = ident();

    let val = select! {
        Token::Bool(x) => Expr::Literal(Literal::Boolean(x)),
        Token::Int(n) => Expr::Literal(Literal::Integer(n)),
        Token::Float(n) => Expr::Literal(Literal::Float(n)),
        Token::Str(s) => Expr::Literal(Literal::String(s)),
        Token::Comment(s) => Expr::Literal(Literal::Comment(s)),
    }
    .labelled("value");

    recursive(|expr| {
        let items = expr
            .clone()
            .separated_by(just(Token::Ctrl(',')))
            .allow_trailing()
            .collect::<Vec<_>>();

        let list = items
            .clone()
            .map(Expr::List)
            .delimited_by(just(Token::Ctrl('[')), just(Token::Ctrl(']')));

        let atom = val
            .or(ident.map(Expr::Ident))
            .or(list)
            .map_with(|expr, e| Spanned::new(expr, e.span()))
            .or(expr
                .clone()
                .delimited_by(just(Token::Ctrl('(')), just(Token::Ctrl(')'))))
            .recover_with(via_parser(nested_delimiters(
                Token::Ctrl('('),
                Token::Ctrl(')'),
                [
                    (Token::Ctrl('['), Token::Ctrl(']')),
                    (Token::Ctrl('{'), Token::Ctrl('}')),
                ],
                |span| Spanned::new(Expr::Error, span),
            )))
            .recover_with(via_parser(nested_delimiters(
                Token::Ctrl('['),
                Token::Ctrl(']'),
                [
                    (Token::Ctrl('('), Token::Ctrl(')')),
                    (Token::Ctrl('{'), Token::Ctrl('}')),
                ],
                |span| Spanned::new(Expr::Error, span),
            )))
            .boxed();

        let call = atom.foldl_with(
            items
                .delimited_by(just(Token::Ctrl('(')), just(Token::Ctrl(')')))
                .map_with(|args, e| Spanned::new(args, e.span()))
                .repeated(),
            |f, args, e| Spanned::new(Expr::Call(Box::new(f), args), e.span()),
        );

        let product = {
            let op = just(Token::Op("*"))
                .to(BinaryOp::Mul)
                .or(just(Token::Op("/")).to(BinaryOp::Div));

            call.clone()
                .foldl_with(op.then(call).repeated(), |a, (op, b), e| {
                    Spanned::new(Expr::Binary(Box::new(a), op, Box::new(b)), e.span())
                })
        };

        let sum = {
            let op = just(Token::Op("+"))
                .to(BinaryOp::Add)
                .or(just(Token::Op("-")).to(BinaryOp::Sub));

            product
                .clone()
                .foldl_with(op.then(product).repeated(), |a, (op, b), e| {
                    Spanned::new(Expr::Binary(Box::new(a), op, Box::new(b)), e.span())
                })
        };

        let compare = {
            let op = just(Token::Op("=="))
                .to(BinaryOp::Equal)
                .or(just(Token::Op("!=")).to(BinaryOp::NotEqual))
                .or(just(Token::Op("<")).to(BinaryOp::LessThan))
                .or(just(Token::Op(">")).to(BinaryOp::GreaterThan))
                .or(just(Token::Op(">=")).to(BinaryOp::GreaterThanOrEqual));

            sum.clone()
                .foldl_with(op.then(sum).repeated(), |a, (op, b), e| {
                    Spanned::new(Expr::Binary(Box::new(a), op, Box::new(b)), e.span())
                })
        };

        let inline_expr = compare.labelled("expression").as_context();

        let block = inline_expr
            .clone()
            .delimited_by(just(Token::Ctrl('{')), just(Token::Ctrl('}')))
            .recover_with(via_parser(nested_delimiters(
                Token::Ctrl('{'),
                Token::Ctrl('}'),
                [
                    (Token::Ctrl('('), Token::Ctrl(')')),
                    (Token::Ctrl('['), Token::Ctrl(']')),
                ],
                |span| Spanned::new(Expr::Error, span),
            )));

        block.or(inline_expr)
    })
}
