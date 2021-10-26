use std::{fmt, cell::{Ref, RefCell}, rc::Rc};

use crate::common::name::Name;


#[derive(PartialEq, Eq, Clone)]
pub struct ConstTerm<N: Name>{ pub name: N }


#[derive(PartialEq, Eq, Hash, Clone)]
pub struct VarTerm<N: Name>{ pub name: N }

#[derive(PartialEq, Eq, Clone)]
pub struct FuncTerm<N: Name>{
    pub name: N,
    pub params: Vec<Term<N>>,
}

impl<N: Name> FuncTerm<N>{
    pub fn get_name(&self) -> &N { &self.name }
    pub fn get_params(&self) -> &Vec<Term<N>>{ &self.params }
    pub fn get_params_mut(&mut self) -> &mut Vec<Term<N>>{ &mut self.params }
}


pub enum Term<N: Name>{
    Const(Rc<RefCell<ConstTerm<N>>>),
    Var(Rc<RefCell<VarTerm<N>>>),
    Func(Rc<RefCell<FuncTerm<N>>>),
}


impl<N: Name> Term<N>{
    pub fn new_const(const_term: ConstTerm<N>) -> Self{ Self::Const(Rc::new(RefCell::new(const_term))) }
    pub fn new_func(func_term: FuncTerm<N>) -> Self{ Self::Func(Rc::new(RefCell::new(func_term))) }
    pub fn new_var(var_term: VarTerm<N>) -> Self{ Self::Var(Rc::new(RefCell::new(var_term))) }

    pub fn is_const(&self) -> bool { if let Term::Const(_) = self { true } else { false } }
    pub fn get_const(&self) -> Ref<ConstTerm<N>> { 
        if let Term::Const(rc) = self { Rc::as_ref(rc).borrow() } 
        else { panic!("term is not const") } 
    }

    pub fn is_func(&self) -> bool { if let Term::Func(_) = self { true } else { false } }
    pub fn get_func(&self) -> Ref<FuncTerm<N>> { 
        if let Term::Func(rc) = self { Rc::as_ref(rc).borrow() } 
        else { panic!("term is not func") } 
    }
    pub fn func_size(&self) -> usize { self.get_func().params.len() }

    pub fn is_var(&self) -> bool { if let Term::Var(_) = self { true } else { false } }
    pub fn get_var(&self) -> Ref<VarTerm<N>> { 
        if let Term::Var(rc) = self { Rc::as_ref(rc).borrow() } 
        else { panic!("term is not var") } 
    }

    pub fn contain(&self, find_term: &Self) -> bool{
        let func_params_contain = || self.get_func().get_params().into_iter().any(|small_term|small_term.contain(find_term));

        match (self, find_term) {
            (Term::Func(_), Term::Func(_)) => self == find_term || func_params_contain(),
            (Term::Func(_), _) => func_params_contain(),
            (Term::Const(_), Term::Const(_)) | (Term::Var(_), Term::Var(_)) => self == find_term,
            (Term::Const(_), _) | (Term::Var(_), _) => false,
        }
    }

    pub fn without_var(&self) -> bool{
        match self {
            Term::Const(_) => true,
            Term::Var(_) => false,
            Term::Func(_) => self.get_func().get_params().into_iter().all(|small_term|small_term.without_var()),
        }
    }


    pub fn new_from_other(other: &Self) -> Self{
        match  other {
            Term::Const(rc) => { Term::Const(Rc::new(RefCell::new(rc.as_ref().borrow().clone()))) },
            Term::Func(rc) => { Term::Func(Rc::new(RefCell::new(rc.as_ref().borrow().clone()))) },
            Term::Var(rc) => { Term::Var(Rc::new(RefCell::new(rc.as_ref().borrow().clone()))) },
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





impl<N: Name> fmt::Display for ConstTerm<N> where N: fmt::Display{
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result { write!(f, "{}", self.name) }
}

impl<N: Name> fmt::Display for VarTerm<N> where N: fmt::Display{
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result { write!(f, "{}", self.name) }
}

impl<N: Name> fmt::Display for FuncTerm<N> where N: fmt::Display{
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result { 
        write!(f, "{}(", self.get_name())?;
        let sz = self.get_params().len();
        for term in self.get_params().into_iter().enumerate() { 
            if term.0 == (sz - 1) { write!(f, "{}", term.1)? } else { write!(f, "{}, ", term.1)? };
        }
        write!(f, ")")
     }
}

impl<N: Name> fmt::Display for Term<N> where N: fmt::Display{
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result { 
        match self {
            Term::Const(_) => self.get_const().fmt(f),
            Term::Func(_) => self.get_func().fmt(f),
            Term::Var(_) => self.get_var().fmt(f),
        }
    }
}


