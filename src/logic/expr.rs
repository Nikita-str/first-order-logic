use std::{collections::LinkedList, rc::Rc};

use crate::{common::name::Name, logic::substit::{Substitution, SubstitutionApply}};

use super::{operations::{BinaryOperations, UnaryOperations}, quants::Quants, terms::Term};


struct PredicateExpr<N: Name>{
    name: N,
    params: Vec<Term<N>>,
}

impl<N: Name> PredicateExpr<N>{
    pub fn most_comon_unifier<'a>(p1: &Self, p2: &Self) -> Option<Substitution<N>>{
        if p1.params.len() != p2.params.len() || p1.name != p2.name { return None }
        if p1.params.len() == 0 { return Some(Substitution::new_empty()) }

        struct Line<N0: Name>{
            left: Term<N0>,
            right: Term<N0>,
        }
        
        struct Lines<N0: Name>{
            cur_ind: usize,
            sz: usize,
            lines: LinkedList<Line<N0>>,
        }

        let mut lines = Lines{cur_ind: 0, sz: 0, lines: LinkedList::new() };
        for (t1, t2) in (& p1.params).into_iter().zip((& p2.params).into_iter()) {
            lines.lines.push_back(Line{ left: Term::new_from_other(t1), right: Term::new_from_other(t2) });
            lines.sz += 1;
        }

        lines.sz-= 1;
        let cur_line = lines.lines.pop_front().unwrap();
        let mut left = cur_line.left; 
        let mut right = cur_line.right; 
        if left.is_func() && right.is_func() {
            let f_sz = left.func_size();
            if f_sz != right.func_size() { return None }
            if left.get_func().get_name() != right.get_func().get_name() { return None }
            lines.sz += f_sz;
            for (l, r) in left.get_func().get_params().into_iter().zip(right.get_func().get_params().into_iter()){
                lines.lines.push_back(Line{ left: l.clone(), right: r.clone() });
            }
            todo!() //next circ
        }
        if left.is_func() && right.is_const() { return None }
        if left.is_func() && right.is_var() {
            let mut subst = Substitution::new_empty();
            subst.add_new_rule(right.get_var().clone(), left.clone());

            for line in (&mut lines.lines).into_iter() {
                subst.apply(&mut line.left);
                subst.apply(&mut line.right);
            }

            lines.sz += 1;
            lines.lines.push_back(Line{left: right, right: left});

            todo!() //next circ
        }
        if left.is_const() && right.is_func() { return None }
        if left.is_const() && right.is_const() { 
            if *left.get_const() != *right.get_const() { return None } 
            todo!() // next circ
         }

        todo!()

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