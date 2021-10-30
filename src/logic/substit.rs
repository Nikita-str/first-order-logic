use std::{collections::HashMap, fmt, hash::Hash};

use crate::common::{name::Name, name_holder::NameHolder};
use super::{expr::Expr, predicate_expr::PredicateExpr, terms::{Term, VarTerm}};

pub struct Substitution<N: Name>{
    substs: HashMap<VarTerm<N>, Term<N>>
}


impl<N: Name> Substitution<N>{
    pub fn new_empty() -> Self{ Self{ substs: HashMap::new() } }
    pub fn add_new_rule(&mut self, var: VarTerm<N>, term: Term<N>){
        if self.substs.insert(var, term).is_some() { panic!("here must be new var") }
    }
}

impl<N: Name> SubstitutionApply<Term<N>> for Substitution<N>{
    fn apply(&self, term: &mut Term<N>){
        match term {
            Term::Const(_) => return,
            Term::Func(f) => f.as_ref().borrow_mut().get_params_mut().into_iter().for_each(|new_term| self.apply(new_term)),
            Term::Var(v) => { 
                let new_val = 
                    if let Some(subst_term) = self.substs.get(&v.as_ref().borrow()) { Some(subst_term.clone()) } else { None };
                if new_val.is_some() { *term = new_val.unwrap(); } 
            }
        }
    }
}

impl<N: Name> SubstitutionApply<Substitution<N>> for Substitution<N>{
    fn apply(&self, other: &mut Substitution<N>) {
        for subst in &mut other.substs{ self.apply(subst.1) }
        for (key, term) in &self.substs {
            if !other.substs.contains_key(key) { other.substs.insert(key.clone(), term.clone()); }
        }
    }
}

impl<N: Name> SubstitutionApply<Expr<N>> for Substitution<N>{
    fn apply(&self, expr: &mut Expr<N>) {
        match expr {
            Expr::Empty => return,
            Expr::Predicate(p) => p.borrow_mut().get_params_mut().into_iter().for_each(|term|self.apply(term)),
            Expr::Quant(q) => self.apply(q.borrow_mut().get_expr_mut()),
            Expr::UnaryOp(uop) => self.apply(uop.borrow_mut().get_expr_mut()),
            Expr::BinaryOp(bop) => {
                self.apply(bop.borrow_mut().get_lexpr_mut());
                self.apply(bop.borrow_mut().get_rexpr_mut())
            }
        }
    }
}

impl<N: Name> SubstitutionApply<PredicateExpr<N>> for Substitution<N>{
    fn apply(&self, other: &mut PredicateExpr<N>) { 
        other.get_params_mut().into_iter().for_each(|term|self.apply(term)) 
    }
}

impl<'a, N:Name, X> SubstitutionApply<Vec<X>> for Substitution<N>
where Substitution<N>: SubstitutionApply<X>
{
    fn apply(&self, other: &mut Vec<X>) { for x in other { self.apply(x) } }
}

pub trait SubstitutionApply<O>{ fn apply(&self, other: &mut O); }





impl<N: Name> fmt::Display for Substitution<N> where N: fmt::Display{
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{{  ")?;
        for subst in &self.substs { write!(f, "{}/{}  ", subst.0, subst.1)?; }
        write!(f, "}}")
    }
}

pub struct DisplaySubst<'a, N, T>
where N:Name, T: Hash + Eq,
{
    pub nh: &'a NameHolder<N, T>,
    pub substs: &'a Substitution<N>
}

impl<'a, N: Name, T> fmt::Display for DisplaySubst<'a, N, T> 
where T: fmt::Display + Hash + Eq
{
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{{   ")?;
        for subst in &self.substs.substs { 
            crate::common::ok_parse::display_term_help_func(f, self.nh, &vec![Term::new_var(subst.0.clone())])?;
            write!(f, "/")?; 
            crate::common::ok_parse::display_term_help_func(f, self.nh, &vec![subst.1.clone()])?;
            write!(f, "   ")?; 
        }   
        write!(f, "}}")  
    }
}