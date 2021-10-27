use std::{collections::{HashMap}, hash::Hash};
use crate::logic::{term_type::TermType, terms::{ConstTerm, Term, VarTerm}};

use super::name::{Name};


/// 'T': top lvl name  
/// 'N': name
pub struct NameHolder<N: Name, T: Hash + Eq>{
    terms: HashMap<TermType, HashMap<T, Term<N>>>,
    last_names: HashMap<TermType, N>,
}

impl<N: Name, T: Hash + Eq> NameHolder<N, T>{
    pub fn get_term_name<I: Iterator<Item = T>>
    (&mut self, term_type: TermType, new_term: T) -> Term<N>{
        let map = self.terms.get_mut(&term_type).unwrap();
        if let Some(term) = map.get(&new_term){
            term.clone()
        } else {
            let last_name = self.last_names.get(&term_type).unwrap().next_tst_name();
            self.last_names.insert(term_type, last_name.clone());
            let term = match term_type {
                TermType::Const => Term::new_const(ConstTerm{ name: last_name }),
                TermType::Var => Term::new_var(VarTerm{ name: last_name }),
                TermType::Pred => Term::new({ name: last_name }),
            };
            map.insert(new_term, term.clone());
            term
        }
    }
}
impl<N: Name, T: UnivocalClassify<TermType> + Hash + Eq> NameHolder<N, T>{
    //TODO:DEL:
    pub fn get_term_for(&mut self, top_level_name: T) -> Term<N>{
        let name_type = top_level_name.classify();
        let map = self.terms.get_mut(&name_type).unwrap();
        if let Some(term) = map.get(&top_level_name){
            term.clone()
        } else {
            let last_name = self.last_names.get(&name_type).unwrap().next_tst_name();
            self.last_names.insert(name_type, last_name.clone());
            let term = match name_type {
                TermType::Const => Term::new_const(ConstTerm{ name: last_name }),
                TermType::Var => Term::new_var(VarTerm{ name: last_name }),
                _ => todo!() // NEED: Multilevel
            };
            map.insert(top_level_name, term.clone());
            term
        }
    }
}

pub trait UnivocalClassify<C>{ fn classify(&self) -> C; }

pub struct LevelPath{ path: Vec<usize> }
pub trait Multilevel{ 
    fn get_next_lvl(&self) -> dyn Iterator<Item = &Self>;
    fn get_lvl_by_path(&self, path: &LevelPath) -> dyn Iterator<Item = &Self>;
    fn get_node_by_path(&self) -> &Self;
}