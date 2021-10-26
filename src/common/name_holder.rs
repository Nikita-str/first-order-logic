use std::{collections::{HashMap}, hash::Hash};
use crate::logic::terms::{ConstTerm, Term, VarTerm};

use super::name::{Name, NameType};


/// 'T': top lvl name  
/// 'N': name
pub struct NameHolder<'a, N: Name, T: UnivocalClassify<NameType> + Hash + Eq>{
    terms: HashMap<NameType, HashMap<&'a T, Term<N>>>,
    last_names: HashMap<NameType, N>,
}

impl<'a, N: Name, T: UnivocalClassify<NameType> + Hash + Eq> NameHolder<'a, N, T>{
    pub fn get_term_for(&mut self, top_level_name: &'a T) -> Term<N>{
        let name_type = top_level_name.classify();
        let map = self.terms.get_mut(&name_type).unwrap();
        if let Some(term) = map.get(top_level_name){
            term.clone()
        } else {
            let last_name = self.last_names.get(&name_type).unwrap().next_tst_name();
            self.last_names.insert(name_type, last_name.clone());
            let term = match name_type {
                NameType::Const => Term::new_const(ConstTerm{ name: last_name }),
                NameType::Var => Term::new_var(VarTerm{ name: last_name }),
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