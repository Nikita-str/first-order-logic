use std::{cell::{Ref, RefCell}, rc::Rc};
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
    pub fn get_lexpr_mut(&mut self) -> &Expr<N> { &mut self.left_formula }
    pub fn get_rexpr(&self) -> &Expr<N> { &self.right_formula }
    pub fn get_rexpr_mut(&mut self) -> &Expr<N> { &mut self.right_formula }
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
    Predicate(Rc<RefCell<PredicateExpr<N>>>),
    UnaryOp(Rc<RefCell<UnaryOpExpr<N>>>),
    BinaryOp(Rc<RefCell<BinaryOpExpr<N>>>),
    Quant(Rc<RefCell<ExprQuant<N>>>),
}

impl<N:Name> Expr<N>{
    pub fn is_empty(&self) -> bool { if let Expr::Empty = self { true } else { false }  }
    pub fn is_binary_op(&self) -> bool { if let Expr::BinaryOp(_) = self { true } else { false }  }

    pub fn apply_unary_op(self, op: UnaryOperations) -> Self {
        Expr::UnaryOp(Rc::new(RefCell::new(UnaryOpExpr::new(op, self)))) 
    }
    pub fn apply_binary_op(op: BinaryOperations, left: Self, right: Self) -> Self {
        Expr::BinaryOp(Rc::new(RefCell::new(BinaryOpExpr::new(op, left, right)))) 
    }
    pub fn apply_quant(self, quant: Quants, var_name: N) -> Self {
        Expr::Quant(Rc::new(RefCell::new(ExprQuant::new(quant, var_name, self)))) 
    }

    pub fn new_predicate(name: N, params: Vec<Term<N>>) -> Self{
        Expr::Predicate(Rc::new(RefCell::new(PredicateExpr::new(name, params))))
    }

    
    pub fn get_expr_predicate(&self) -> Ref<'_, PredicateExpr<N>>{
        match self {
            Expr::Predicate(p) => Rc::as_ref(p).borrow(),
            _ => panic!("not predicate expr")
        }
    }

    pub fn get_expr_quant(&self) -> Ref<'_, ExprQuant<N>>{
        match self {
            Expr::Quant(q) => Rc::as_ref(q).borrow(),
            _ => panic!("not quant expr")
        }
    }

    pub fn get_expr_unary(&self) -> Ref<'_, UnaryOpExpr<N>>{
        match self {
            Expr::UnaryOp(uop) => Rc::as_ref(uop).borrow(),
            _ => panic!("not unary op expr")
        }
    }
    
    pub fn get_expr_binary(&self) -> Ref<'_, BinaryOpExpr<N>>{
        match self {
            Expr::BinaryOp(bop) => Rc::as_ref(bop).borrow(),
            _ => panic!("not binary op expr")
        }
    }

    pub fn get_priority(&self) -> Option<usize> {
        match self {
            Expr::Empty => AllSymbs::Empty.get_priority(),
            Expr::Predicate(_) => AllSymbs::Term(TermType::Pred).get_priority(),
            Expr::Quant(q) => AllSymbs::Quant(q.borrow().get_quant()).get_priority(),
            Expr::UnaryOp(uop) => AllSymbs::Op(Operations::Unary(uop.borrow().get_op())).get_priority(),
            Expr::BinaryOp(bop) => AllSymbs::Op(Operations::Binary(bop.borrow().get_op())).get_priority(),
        }
    }
}


impl<N:Name> Expr<N>{
    pub fn impl_transformation(& mut self){
        match self {
            Expr::BinaryOp(op) => {
                op.borrow_mut().left_formula.impl_transformation();
                op.borrow_mut().right_formula.impl_transformation();
                let old_left = op.borrow().left_formula.clone();
                let bin_op = op.borrow().op;
                if let BinaryOperations::Impl = bin_op{
                    op.borrow_mut().op = BinaryOperations::Or; 
                    op.borrow_mut().left_formula = 
                        Expr::UnaryOp(Rc::new(RefCell::new(UnaryOpExpr::new(UnaryOperations::Not, old_left))));
                }
            },
            
            Expr::Quant(q) => { q.borrow_mut().expr.impl_transformation(); }
            Expr::UnaryOp(uop) => { uop.borrow_mut().formula.impl_transformation(); }

            Expr::Predicate(_) | Expr::Empty => {}
        };
    }
}


impl<N: Name> Clone for Expr<N>{
    fn clone(&self) -> Self {
        match self {
            Expr::Empty => Expr::Empty,
            Expr::Predicate(p) => Expr::Predicate(Rc::clone(p)),
            Expr::Quant(q) => Expr::Quant(Rc::clone(q)),
            Expr::UnaryOp(uop) => Expr::UnaryOp(Rc::clone(uop)),
            Expr::BinaryOp(bop) => Expr::BinaryOp(Rc::clone(bop)),
        }
    }
}