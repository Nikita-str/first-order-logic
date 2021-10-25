use std::rc::Rc;
use crate::{common::name::Name};
use super::{operations::{BinaryOperations, UnaryOperations}, predicate_expr::PredicateExpr, quants::Quants};


struct UnaryOpExpr<N: Name>{
    op: UnaryOperations,
    formula: FormulaExpr<N>,  
}

struct BinaryOpExpr<N: Name>{
    op: BinaryOperations,
    left_formula: FormulaExpr<N>,  
    right_formula: FormulaExpr<N>,  
}

struct ExprQuant<N: Name>{
    quant: Quants,
    var_name: N,
    expr: FormulaExpr<N>,
}

enum FormulaExpr<N: Name>{
    Predicate(Rc<PredicateExpr<N>>),
    UnaryOp(Rc<UnaryOpExpr<N>>),
    BinaryOp(Rc<BinaryOpExpr<N>>),
    Quant(Rc<ExprQuant<N>>),
}

enum Expr<N: Name>{
    Empty,
    Formula(FormulaExpr<N>),
}