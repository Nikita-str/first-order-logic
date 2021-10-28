use std::rc::Rc;
use crate::{common::name::Name};
use super::{all_symbs::AllSymbs, operations::{BinaryOperations, Operations, UnaryOperations}, predicate_expr::PredicateExpr, quants::Quants, term_type::TermType, terms::Term};


#[derive(Debug)]
pub struct UnaryOpExpr<N: Name>{
    op: UnaryOperations,
    formula: Expr<N>,  
}
impl<N:Name> UnaryOpExpr<N>{
    pub fn new(op: UnaryOperations, formula: Expr<N>) -> Self { Self { op, formula } }
    
    pub fn get_op(&self) -> UnaryOperations { self.op }
    pub fn get_expr(&self) -> &Expr<N> { &self.formula }
}

#[derive(Debug)]
pub struct BinaryOpExpr<N: Name>{
    op: BinaryOperations,
    left_formula: Expr<N>,  
    right_formula: Expr<N>,  
}
impl<N:Name> BinaryOpExpr<N>{
    pub fn new(op: BinaryOperations, left: Expr<N>, right: Expr<N>) -> Self 
    { Self { op, left_formula: left, right_formula: right } }

    pub fn get_op(&self) -> BinaryOperations { self.op }
    pub fn get_lexpr(&self) -> &Expr<N> { &self.left_formula }
    pub fn get_rexpr(&self) -> &Expr<N> { &self.right_formula }
}

#[derive(Debug)]
pub struct ExprQuant<N: Name>{
    quant: Quants,
    var_name: N,
    expr: Expr<N>,
}
impl<N:Name> ExprQuant<N>{
    pub fn new(q: Quants, var_name: N, expr: Expr<N>) -> Self { Self { quant: q, var_name, expr } }

    pub fn get_quant(&self) -> Quants { self.quant }
    pub fn get_var_name(&self) -> &N { &self.var_name }
    pub fn get_expr(&self) -> &Expr<N> { &self.expr }
}

#[derive(Debug)]
pub enum Expr<N: Name>{
    Empty,
    Predicate(Rc<PredicateExpr<N>>),
    UnaryOp(Rc<UnaryOpExpr<N>>),
    BinaryOp(Rc<BinaryOpExpr<N>>),
    Quant(Rc<ExprQuant<N>>),
}

impl<N:Name> Expr<N>{
    pub fn is_empty(&self) -> bool { if let Expr::Empty = self { true } else { false }  }
    pub fn is_binary_op(&self) -> bool { if let Expr::BinaryOp(_) = self { true } else { false }  }

    pub fn apply_unary_op(self, op: UnaryOperations) -> Self {
        Expr::UnaryOp(Rc::new(UnaryOpExpr::new(op, self))) 
    }
    pub fn apply_binary_op(op: BinaryOperations, left: Self, right: Self) -> Self {
        Expr::BinaryOp(Rc::new(BinaryOpExpr::new(op, left, right))) 
    }
    pub fn apply_quant(self, quant: Quants, var_name: N) -> Self {
        Expr::Quant(Rc::new(ExprQuant::new(quant, var_name, self))) 
    }

    pub fn new_predicate(name: N, params: Vec<Term<N>>) -> Self{
        Expr::Predicate(Rc::new(PredicateExpr::new(name, params)))
    }


    
    pub fn get_expr_predicate(&self) -> &PredicateExpr<N>{
        match self {
            Expr::Predicate(p) => Rc::as_ref(p),
            _ => panic!("not predicate expr")
        }
    }

    pub fn get_expr_quant(&self) -> &ExprQuant<N>{
        match self {
            Expr::Quant(q) => Rc::as_ref(q),
            _ => panic!("not quant expr")
        }
    }

    pub fn get_expr_unary(&self) -> &UnaryOpExpr<N>{
        match self {
            Expr::UnaryOp(uop) => Rc::as_ref(uop),
            _ => panic!("not unary op expr")
        }
    }
    
    pub fn get_expr_binary(&self) -> &BinaryOpExpr<N>{
        match self {
            Expr::BinaryOp(bop) => Rc::as_ref(bop),
            _ => panic!("not binary op expr")
        }
    }

    pub fn get_priority(&self) -> Option<usize> {
        match self {
            Expr::Empty => AllSymbs::Empty.get_priority(),
            Expr::Predicate(_) => AllSymbs::Term(TermType::Pred).get_priority(),
            Expr::Quant(q) => AllSymbs::Quant(q.get_quant()).get_priority(),
            Expr::UnaryOp(uop) => AllSymbs::Op(Operations::Unary(uop.get_op())).get_priority(),
            Expr::BinaryOp(bop) => AllSymbs::Op(Operations::Binary(bop.get_op())).get_priority(),
        }
    }
}