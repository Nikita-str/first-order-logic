use std::rc::Rc;
use crate::{common::name::Name};
use super::{operations::{BinaryOperations, UnaryOperations}, predicate_expr::PredicateExpr, quants::Quants};


pub struct UnaryOpExpr<N: Name>{
    op: UnaryOperations,
    formula: Expr<N>,  
}
impl<N:Name> UnaryOpExpr<N>{
    pub fn new(op: UnaryOperations, formula: Expr<N>) -> Self { Self { op, formula } }
}

pub struct BinaryOpExpr<N: Name>{
    op: BinaryOperations,
    left_formula: Expr<N>,  
    right_formula: Expr<N>,  
}

pub struct ExprQuant<N: Name>{
    quant: Quants,
    var_name: N,
    expr: Expr<N>,
}

pub enum Expr<N: Name>{
    Empty,
    Predicate(Rc<PredicateExpr<N>>),
    UnaryOp(Rc<UnaryOpExpr<N>>),
    BinaryOp(Rc<BinaryOpExpr<N>>),
    Quant(Rc<ExprQuant<N>>),
}

impl<N:Name> Expr<N>{
    pub fn apply_unary_op(self, op: UnaryOperations) -> Self {
        Expr::UnaryOp(Rc::new(UnaryOpExpr::new(op, self))) 
    }
}