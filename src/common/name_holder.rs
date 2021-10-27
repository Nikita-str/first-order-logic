use std::{collections::{HashMap}, hash::Hash};
use crate::logic::{term_type::TermType, terms::{ConstTerm, Term, VarTerm}};

use super::name::{Name};


/// 'T': top lvl name  
/// 'N': name
pub struct NameHolder<N: Name, T: Hash + Eq>{
    names: HashMap<TermType, HashMap<T, N>>,
    last_names: HashMap<TermType, N>,
}

impl<N: Name, T: Hash + Eq> NameHolder<N, T>{
    pub fn new() -> Self{
        let mut names = HashMap::new();
        let mut last_names = HashMap::new();

        let mut insert = |t_type|{
            last_names.insert(t_type, N::first_name(t_type));
            names.insert(t_type, HashMap::new());
        };

        insert(TermType::Var);
        insert(TermType::Pred);
        insert(TermType::Func);
        insert(TermType::Const);

        Self{ names, last_names }
    }
}

impl<N: Name, T: Hash + Eq> NameHolder<N, T>{
    pub fn get_term_name(&mut self, term_type: TermType, new_term: T) -> N{
        let map = self.names.get_mut(&term_type).unwrap();
        if let Some(term) = map.get(&new_term){
            term.clone()
        } else {
            let last_name = self.last_names.get(&term_type).unwrap().next_tst_name();
            self.last_names.insert(term_type, last_name.clone());
            map.insert(new_term, last_name.clone());
            last_name
        }
    }
}
