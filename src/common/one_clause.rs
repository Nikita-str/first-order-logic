use super::name::Name;
use crate::logic::{expr::Expr, operations::BinaryOperations, predicate_expr::PredicateExpr};


pub struct ClauseElem<N:Name>{
    positive: bool,
    predicate: PredicateExpr<N>
}

impl<N:Name> ClauseElem<N>{
    pub fn new(expr: Expr<N>) -> Self{
        match expr {
            Expr::UnaryOp(not) => {
                let borrow =not.borrow();
                let predicate = 
                    match borrow.get_expr() { 
                        Expr::Predicate(p) => p.take(),
                        _ =>  panic!("expr is not clause") 
                    };
                Self { positive: false, predicate }
            }
            Expr::Predicate(pred) => Self{ positive: true, predicate: pred.take() },
            Expr::Empty => todo!("not sure need it processed in Elem lvl or OneClause vec.is_empty"),
            _=>panic!("expr is not clause")
        }
    }

    pub fn is_can_be_contrary_pair(a: &Self, b: &Self) -> bool{
        (a.positive != b.positive) && (a.predicate.name == b.predicate.name)
    }

    pub fn is_positive(&self) -> bool { self.positive }
    pub fn is_negative(&self) -> bool { !self.positive }

    pub fn get_predicate(&self) -> &PredicateExpr<N> { &self.predicate }
}

pub struct OneClause<N:Name>{
    elems: Vec<ClauseElem<N>>
}

impl<N:Name> OneClause<N>{
    pub fn new(expr: Expr<N>) -> Self{
        let mut elems = vec![];
        let mut not_procced_expr = vec![expr];
        while !not_procced_expr.is_empty() {
            let expr = not_procced_expr.pop().unwrap();
            match expr {
                Expr::Quant(_) => panic!("expr is not clause"),
                Expr::Empty => todo!("not sure what to do"),
                Expr::BinaryOp(bop) => {
                    match bop.borrow().get_op() {
                        BinaryOperations::Or => {
                            not_procced_expr.push(bop.borrow().get_rexpr().clone());
                            not_procced_expr.push(bop.borrow().get_lexpr().clone());
                        }
                        BinaryOperations::And | BinaryOperations::Impl => panic!("expr is not clause"),
                    }
                }
                Expr::UnaryOp(_) | Expr::Predicate(_) => elems.push(ClauseElem::new(expr)),
            }
        }
        Self { elems }
    }

    pub fn get_elems(&self) -> &Vec<ClauseElem<N>> { &self.elems }
}