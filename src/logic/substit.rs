use std::{collections::HashMap, fmt};

use crate::common::name::Name;
use super::terms::{Term, VarTerm};

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

/*
impl<'s, N: Name> Mul<&'s Substitution<N>> for Term<N>{
    type Output = Self;
    fn mul(self, rhs: &'s Substitution<N>) -> Self::Output { rhs.apply(&mut self); self }
}  
*/

impl<N: Name> SubstitutionApply<Substitution<N>> for Substitution<N>{
    fn apply(&self, other: &mut Substitution<N>) {
        for subst in &mut other.substs{ self.apply(subst.1) }
        for (key, term) in &self.substs {
            if !other.substs.contains_key(key) { other.substs.insert(key.clone(), term.clone()); }
        }
    }
}

pub trait SubstitutionApply<O>{ fn apply(&self, other: &mut O); }





impl<N: Name> fmt::Display for Substitution<N> where N: fmt::Display{
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{{  ")?;
        for subst in &self.substs { write!(f, "{}/{}  ", subst.0, subst.1)?; }
        write!(f, "}}")
    }
}