use std::{cmp::Ordering, collections::HashSet};

use super::{deep_copy::DeepCopy, name::Name};
use crate::logic::{expr::Expr, operations::BinaryOperations, predicate_expr::PredicateExpr, substit::{Substitution, SubstitutionApply}, terms::Term};

#[derive(PartialEq)]
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

    pub fn is_can_be_gluing(a: &Self, b: &Self) -> bool{
        (a.positive == b.positive) && (a.predicate.name == b.predicate.name)
    }


    pub fn is_positive(&self) -> bool { self.positive }
    pub fn is_negative(&self) -> bool { !self.positive }

    pub fn get_predicate(&self) -> &PredicateExpr<N> { &self.predicate }

    pub fn set_var_index(&mut self, index: usize) { self.predicate.set_var_index(index); }
}

impl<N:Name + PartialOrd> PartialOrd for ClauseElem<N>{
    fn partial_cmp(&self, other: &Self) -> Option<std::cmp::Ordering> {
        match self.predicate.partial_cmp(&other.predicate) {
            Some(Ordering::Greater) => Some(Ordering::Greater),
            Some(Ordering::Less) => Some(Ordering::Less),
            Some(Ordering::Equal) => other.positive.partial_cmp(&self.positive),
            None => None,
        }
    }
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

    pub fn set_var_index(&mut self, index: usize) {
        for clause_elem in self.elems.iter_mut() { clause_elem.set_var_index(index) }
    }

    /// TODO: add exception pairs (already checked, for example)
    pub fn get_potential_contrary_pairs(a: &Self, b:&Self) -> HashSet<(usize, usize)>{
        if std::ptr::eq(a, b) { panic!("use gluing instead") }
        let mut ret = HashSet::new();
        for (a_ind, a_elem) in a.elems.iter().enumerate() {
            for (b_ind, b_elem) in b.elems.iter().enumerate(){
                if ClauseElem::is_can_be_contrary_pair(&a_elem, &b_elem) { ret.insert((a_ind, b_ind)); }
            }
        }
        ret
    }

    pub fn get_gluing_pairs(&self) -> HashSet<(usize, usize)>{
        let mut ret = HashSet::new();
        let len = self.elems.len(); 

        for left in 0..len {
            let l_elem = self.elems.get(left).unwrap();
            for right in (left + 1)..len {
                if ClauseElem::is_can_be_gluing(l_elem, self.elems.get(right).unwrap()) { ret.insert((left, right)); }
            }
        }

        ret
    }

    /// in first time pass here (0, 0)
    pub fn get_next_gluing_pairs(&self, start: (usize, usize)) -> Option<(usize, usize)>{
        let len = self.elems.len(); 
        if start.0 >= start.1 { panic!("smth went wrong") }
        if start.0 >= len { panic!("smth went wrong") }
        if start.1 >= len { panic!("smth went wrong") }

        let mut first = true;
        for left in (start.0)..len {
            let l_elem = self.elems.get(left).unwrap();
            let from = if first { start.1 } else { left };
            for right in (from + 1)..len {
                if ClauseElem::is_can_be_gluing(l_elem, self.elems.get(right).unwrap()) { return Some((left, right)) }
            }
            first = false;
        }

        None
    }

    pub fn new_from_this(&self, index_delete: HashSet<usize>, subst: Option<Substitution<N>>) -> Self{
        let mut ret_elems = vec![];
        for (ind, x) in self.get_elems().iter().enumerate() {
            if index_delete.contains(&ind) { continue }
            let mut predicate = x.predicate.deep_copy();
            if let Some(sub) = &subst { sub.apply(& mut predicate); }
            ret_elems.push( ClauseElem { positive: x.positive, predicate } );
        }

        Self { elems: ret_elems }
    }

}

impl<N:Name> ClauseElem<N>{
    pub fn gluing<F>(left: &Self, right: &Self, printer: F) -> Option<Substitution<N>> 
    where F: Fn(&Term<N>, &Term<N>)
    {
        //TODO: need deep-copy  ?! (think yes)
        PredicateExpr::most_comon_unifier(&left.get_predicate().deep_copy(), &right.get_predicate().deep_copy(), printer)
    }
}

impl<N:Name> OneClause<N>{
    pub fn gluing<F>(&self, index_pair:(usize, usize), printer: F) -> Option<OneClause<N>> 
    where F: Fn(&Term<N>, &Term<N>)
    {
        let left = self.elems.get(index_pair.0).unwrap();
        let right = self.elems.get(index_pair.1).unwrap();
        if let Some(subst) = ClauseElem::gluing(left, right, printer) {
            let mut delete = HashSet::new();
            // we need to delete only one : {A or A = A} 
            //                     and not: { A or A = empty }   <-- it not true!
            // so first - stay
            //delete.insert(index_pair.0); 
            delete.insert(index_pair.1);
            Some(self.new_from_this(delete, Some(subst)))
        } else {
            None
        }
    }
}



impl<N: Name> SubstitutionApply<ClauseElem<N>> for Substitution<N>{
    fn apply(&self, other: &mut ClauseElem<N>) { self.apply(&mut other.predicate) }
}
impl<N: Name> SubstitutionApply<OneClause<N>> for Substitution<N>{
    fn apply(&self, other: &mut OneClause<N>) { self.apply(&mut other.elems) }
}