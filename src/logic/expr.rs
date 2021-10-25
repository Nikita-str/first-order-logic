use std::rc::Rc;

use crate::{common::name::Name, logic::substit::Substitution};

use super::{operations::{BinaryOperations, UnaryOperations}, quants::Quants, terms::Term};


struct PredicateExpr<N: Name>{
    name: N,
    params: Vec<Term<N>>,
}

impl<N: Name> PredicateExpr<N>{
    pub fn most_comon_unifier(p1: Self, p2: Self) -> Option<Substitution<N>>{
        if p1.params.len() != p2.params.len() || p1.name != p2.name { return None }
        if p1.params.len() == 0 { return Some(Substitution::new_empty()) }

        struct Line<N0: Name>{
            left: Term<N0>,
            right: Term<N0>,
        }
        
        struct Lines<N0: Name>{
            cur_ind: usize,
            lines: Vec<Line<N0>>,
        }




    }
}


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