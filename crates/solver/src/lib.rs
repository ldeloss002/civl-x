pub mod test;

pub use z3;
use z3::{ast, Config, Context, SatResult, Solver};

fn main() {
    let cfg = Config::new();
    let ctx = Context::new(&cfg);
    let x = ast::Int::new_const(&ctx, "x");
    let y = ast::Int::new_const(&ctx, "y");

    let solver = Solver::new(&ctx);
    solver.assert(&x.gt(&y));

    match solver.check() {
        SatResult::Sat => println!("sat"),
        SatResult::Unsat => println!("unsat"),
        SatResult::Unknown => println!("unknown"),
    }
}
