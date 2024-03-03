use ariadne::{sources, Color, Label, Report, ReportKind};
use civlx_lang::{
    fraction::BigInt, lex_and_parse_for_language, BinaryOp, Expr, Input, Language, Literal, Parser,
    Spanned,
};
use civlx_solver::z3::{
    ast::{Array, Ast, Bool, Dynamic, Int, BV},
    *,
};

fn main() {
    // let filename = env::args().nth(1).expect("no file given");
    let filename = "test.st";
    let src = &std::fs::read_to_string(filename).expect("failed to read file");

    let (ast, parse_errors) = lex_and_parse_for_language(src, Language::Smalltalk);

    let parse_errors = parse_errors.into_iter().map(|err| {
        Report::build(ReportKind::Error, filename, err.span().start)
            .with_message(err.to_string())
            .with_label(
                Label::new((filename, err.span().into_range()))
                    .with_message(err.reason().to_string())
                    .with_color(Color::Red),
            )
            .with_labels(err.contexts().map(|(label, span)| {
                Label::new((filename, span.into_range()))
                    .with_message(format!("while parsing this {label}"))
                    .with_color(Color::Blue)
            }))
            .finish()
    });

    for err in parse_errors {
        err.print(sources([(filename, src)])).unwrap()
    }

    if let Some(ast) = ast {
        for spanned_expr in ast {
            let (expr, span) = spanned_expr.into_deref_spanned();

            if let civlx_lang::Expr::Literal(Literal::Comment(comment)) = expr {
                let comment = &comment[1..comment.len() - 1];
                dbg!(&comment);
                let (tokens, parse_errors) = civlx_lang::proof::lexer()
                    .parse(comment)
                    .into_output_errors();

                if let Some(tokens) = tokens {
                    let len = src.chars().count();
                    let (ast, parse_errors) = civlx_lang::proof::proof_parser()
                        .parse(tokens.spanned((len..len + 1).into()))
                        .into_output_errors();

                    if let Some(ast) = ast {
                        let config = Config::new();
                        let context = Context::new(&config);
                        let solver = Solver::new(&context);

                        let mut synthesis = Synthesis::new(solver);

                        synthesis.parse(ast);
                        synthesis.check();

                        dbg!(&synthesis.check(), &synthesis.solver.get_proof());
                    }
                }
            }
        }
    }
}

struct Synthesis<'a> {
    solver: Solver<'a>,
}

impl<'a> Synthesis<'a> {
    fn new(solver: Solver<'a>) -> Synthesis<'a> {
        Synthesis { solver }
    }

    fn parse(&mut self, ast: Vec<Spanned<Expr<'a>>>) {
        for expr_spanned in ast {
            self.parse_expr(expr_spanned);
        }
    }

    fn parse_expr(&mut self, expr_spanned: Spanned<Expr<'a>>) -> Dynamic<'a> {
        let (expr, span) = expr_spanned.into_deref_spanned();
        match expr {
            Expr::Error => todo!(),
            Expr::Literal(literal) => self.parse_literal(literal),
            Expr::List(_) => todo!(),
            Expr::Ident(_) => todo!(),
            Expr::Binary(left, binary_op, right) => self.parse_binary_op(*left, binary_op, *right),
            Expr::Call(..) => todo!(),
            Expr::If(..) => todo!(),
            Expr::Function(_) => todo!(),
        }
    }

    fn parse_literal(&mut self, literal: Literal<'a>) -> Dynamic<'a> {
        match literal {
            Literal::Boolean(_) => todo!(),
            Literal::Integer(int) => self.parse_int(int).into(),
            Literal::Float(_) => todo!(),
            Literal::String(_) => todo!(),
            Literal::Comment(_) => todo!(),
            Literal::List(_) => todo!(),
        }
    }

    fn parse_int(&mut self, int: BigInt) -> Int<'a> {
        Int::from_big_int(self.solver.get_context(), &int)
    }

    fn parse_binary_op(
        &mut self,
        left: Spanned<Expr<'a>>,
        binary_op: BinaryOp,
        right: Spanned<Expr<'a>>,
    ) -> Dynamic<'a> {
        let left = self.parse_expr(left);
        let right = self.parse_expr(right);

        match binary_op {
            BinaryOp::Add => (left.as_int().unwrap() + right.as_int().unwrap()).into(),
            BinaryOp::Sub => (left.as_int().unwrap() - right.as_int().unwrap()).into(),
            BinaryOp::Mul => (left.as_int().unwrap() * right.as_int().unwrap()).into(),
            BinaryOp::Div => (left.as_int().unwrap() / right.as_int().unwrap()).into(),
            BinaryOp::Equal => {
                let result = left.as_int().unwrap()._eq(&right.as_int().unwrap());
                self.solver.assert(&result);
                result.into()
            }
            BinaryOp::NotEqual => todo!(),
            BinaryOp::LessThan => {
                let result = left.as_int().unwrap().lt(&right.as_int().unwrap());
                self.solver.assert(&result);
                result.into()
            }
            BinaryOp::LessThanOrEqual => {
                let result = left.as_int().unwrap().le(&right.as_int().unwrap());
                self.solver.assert(&result);
                result.into()
            }
            BinaryOp::GreaterThan => {
                let result = left.as_int().unwrap().gt(&right.as_int().unwrap());
                self.solver.assert(&result);
                result.into()
            }
            BinaryOp::GreaterThanOrEqual => {
                let result = left.as_int().unwrap().ge(&right.as_int().unwrap());
                self.solver.assert(&result);
                result.into()
            }
        }
    }

    pub fn check(&self) {
        match self.solver.check() {
            SatResult::Sat => println!("sat"),
            SatResult::Unsat => println!("unsat"),
            SatResult::Unknown => println!("unknown"),
        }
    }
}
