use std::{collections::{HashMap, HashSet}, hash::Hash};
use crate::logic::{term_type::TermType, terms::{Term}};

use super::name::{Name};


/// 'T': top lvl name  
/// 'N': name
pub struct NameHolder<N: Name, T: Hash + Eq>{
    var_term: HashMap<N, Term<N>>,
    const_term: HashMap<N, Term<N>>,

    func_lens: HashMap<N, usize>,
    pred_lens: HashMap<N, usize>,

    names: HashMap<TermType, HashMap<T, N>>,
    last_names: HashMap<TermType, N>,

    banned_names: HashMap<TermType, HashSet<N>>,
}

impl<N: Name, T: Hash + Eq> NameHolder<N, T>{
    pub fn new() -> Self{
        let mut names = HashMap::new();
        let mut last_names = HashMap::new();
        let mut banned_names = HashMap::new();

        let mut insert = |t_type|{
            last_names.insert(t_type, N::first_name(t_type));
            names.insert(t_type, HashMap::new());
            banned_names.insert(t_type, HashSet::new());
        };

        insert(TermType::Var);
        insert(TermType::Pred);
        insert(TermType::Func);
        insert(TermType::Const);

        Self{ 
            var_term: HashMap::new(), 
            const_term: HashMap::new(), 
            func_lens: HashMap::new(), 
            pred_lens: HashMap::new(),
            names, 
            last_names,
            banned_names, 
        }
    }
}

impl<N: Name, T: Hash + Eq> NameHolder<N, T>{
    pub fn get_name(&mut self, term_type: TermType, new_term: T) -> N{
        let map = self.names.get_mut(&term_type).unwrap();
        if let Some(term) = map.get(&new_term){
            term.clone()
        } else {
            let last_name = self.last_names.get(&term_type).unwrap().next_tst_name();
            self.last_names.insert(term_type, last_name.clone());
            if term_type == TermType::Const { 
                self.const_term.insert(last_name.clone(), Term::new_const_by_param(last_name.clone())); 
            } else if term_type == TermType::Var {
                self.var_term.insert(last_name.clone(), Term::new_var_by_param(last_name.clone())); 
            }
            map.insert(new_term, last_name.clone());
            last_name
        }
    }

    pub fn get_var_term(&self, name: &N) -> Term<N>{ self.var_term.get(name).unwrap().clone() }
    pub fn get_const_term(&self, name: &N) -> Term<N>{ self.const_term.get(name).unwrap().clone() }

    pub fn exist_name(&self, term_type: TermType, term: &T) -> bool{
        //unsafe{self.names.get(&term_type).unwrap_unchecked()}.contains_key(term)
        self.names.get(&term_type).unwrap().contains_key(term)
    }

    fn get_param_len_helper(map: &HashMap<N, usize>, name: &N) -> Option<usize> { map.get(name).and_then(|x|Some(*x)) }
    pub fn get_func_param_len(&self, name: &N) -> Option<usize>{ Self::get_param_len_helper(&self.func_lens, name) }
    pub fn get_pred_param_len(&self, name: &N) -> Option<usize>{ Self::get_param_len_helper(&self.pred_lens, name) }
    pub fn get_param_len(&self, term_type: TermType, name: &N) -> Option<usize>{
        match term_type{
            TermType::Func => self.get_func_param_len(name),
            TermType::Pred => self.get_pred_param_len(name),
            _ => panic!("term such type {:?} has no params len", term_type)
        }
    }

    fn set_param_len_helper(map: &mut HashMap<N, usize>, name: N, size: usize){
        if let Some(prev_size) = map.insert(name, size) { 
            if prev_size != size { panic!("params size not the same! (was={}; new={})", prev_size, size) } 
        } 
    }
    pub fn set_func_param_len(&mut self, name: N, size: usize) { Self::set_param_len_helper(&mut self.func_lens, name, size) }
    pub fn set_pred_param_len(&mut self, name: N, size: usize) { Self::set_param_len_helper(&mut self.pred_lens, name, size) }
    pub fn set_param_len(&mut self, term_type: TermType, name: N, size: usize) {
        match term_type{
            TermType::Func => self.set_func_param_len(name, size),
            TermType::Pred => self.set_pred_param_len(name, size),
            _ => panic!("term such type {:?} has no params len", term_type)
        }
    }

    pub fn is_name_banned(&mut self, term_type: TermType, name: &N) -> bool { self.banned_names.get(&term_type).unwrap().contains(name) }
    pub fn ban_name(&mut self, term_type: TermType, name: N) -> bool { self.banned_names.get_mut(&term_type).unwrap().insert(name) }
    pub fn unban_name(&mut self, term_type: TermType, name: &N) -> bool { self.banned_names.get_mut(&term_type).unwrap().remove(name) }
}
