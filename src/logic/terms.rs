use std::{cell::RefCell, rc::Rc};

use crate::common::name::Name;

#[derive(PartialEq, Eq, Clone)]
pub struct ConstTerm<N: Name>{ name: N }

#[derive(PartialEq, Eq, Hash, Clone)]
pub struct VarTerm<N: Name>{ name: N }

#[derive(PartialEq, Eq, Clone)]
pub struct FuncTerm<N: Name>{
    name: N,
    params: Vec<Term<N>>,
}

impl<N: Name> FuncTerm<N>{
    pub fn get_params(&self) -> &Vec<Term<N>>{ &self.params }
    pub fn get_params_mut(&mut self) -> &mut Vec<Term<N>>{ &mut self.params }
}

pub enum Term<N: Name>{
    Const(Rc<RefCell<ConstTerm<N>>>),
    Var(Rc<RefCell<VarTerm<N>>>),
    Func(Rc<RefCell<FuncTerm<N>>>),
}

impl<N: Name> Term<N>{
    fn new_from_other(other: &Self) -> Self{
        match  other {
            Term::Const(rc) => { let const_term = &**rc; Term::Const(Rc::new(const_term.clone())) },
            Term::Var(rc) => { let const_term = &**rc; Term::Var(Rc::new(const_term.clone())) },
            Term::Func(rc) => { let const_term = &**rc; Term::Func(Rc::new(const_term.clone())) },
        }
    }
}

impl<N: Name> Clone for Term<N>{
    fn clone(&self) -> Self {
        match self {
            Term::Const(rc) => Term::Const(Rc::clone(rc)),
            Term::Var(rc) => Term::Var(Rc::clone(rc)),
            Term::Func(rc) => Term::Func(Rc::clone(rc)),
        }
    }
}

impl<N: Name> Eq for Term<N>{}
impl<N: Name> PartialEq for Term<N>{
    fn eq(&self, other: &Self) -> bool {
        match (self, other) {
            (Term::Const(c1), Term::Const(c2)) => c1 == c2,
            (Term::Var(v1), Term::Var(v2)) => v1 == v2,
            (Term::Func(f1), Term::Func(f2)) => f1 == f2,
            _ => false,
        }
    }
}