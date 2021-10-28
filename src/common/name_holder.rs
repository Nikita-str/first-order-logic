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

    renaming: HashMap<N, Vec<N>>
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
            renaming: HashMap::new(),
        }
    }
}

impl<N: Name, T: Hash + Eq> NameHolder<N, T>{
    fn real_add_name(&mut self,term_type: TermType, new_term: T) -> N{
        let map = self.names.get_mut(&term_type).unwrap();
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

    pub fn get_name(&mut self, term_type: TermType, new_term: T) -> N{
        if let Some(name) = self.get_last_existing_name(term_type, &new_term){
            name.clone()
        } else { 
            self.real_add_name(term_type, new_term) 
        }
    }

    pub fn get_var_term(&self, name: &N) -> Term<N>{ self.var_term.get(name).unwrap().clone() }
    pub fn get_const_term(&self, name: &N) -> Term<N>{ self.const_term.get(name).unwrap().clone() }

    pub fn exist_name(&self, term_type: TermType, term: &T) -> bool{
        //unsafe{self.names.get(&term_type).unwrap_unchecked()}.contains_key(term)
        self.names.get(&term_type).unwrap().contains_key(term)
    }
    /// # panic 
    /// if ```!self.exist_name(term_type, term)```
    pub fn get_initial_existing_name(&self, term_type: TermType, term: &T) -> N{
        let map = self.names.get(&term_type).unwrap();
        if let Some(name) = map.get(term){ name.clone() } 
        else { panic!("name not exist!") }
    }

    pub fn get_last_existing_name(&self, term_type: TermType, term: &T) -> Option<&N>{
        let map = self.names.get(&term_type).unwrap();
        if let Some(name) = map.get(&term){ 
            if self.renaming.contains_key(name) {
                self.renaming.get(name).unwrap().last()
            } else {
                Some(name)
            }
        } else {
            None
        }
    }

    /// if name not exist just do the same as ```get_name``` do
    /// 
    /// if name already exist - create new name for that term
    /// 
    /// it may be helpful when the same name used in different cases. 
    /// 
    /// for example: 
    /// `∃x: P(x) -> ∀x: R(x)`
    /// but it mean
    /// `∃x_0: P(x_0) -> ∀x_1: R(x_1)`
    pub fn get_name_uncond_new(&mut self, term_type: TermType, new_term: T) -> N { 
        if !self.exist_name(term_type, &new_term) { self.get_name(term_type, new_term) }
        else{
            let old_name = self.get_initial_existing_name(term_type, &new_term);
            let new_name = self.real_add_name(term_type, new_term);
            if !self.renaming.contains_key(&old_name) { self.renaming.insert(old_name.clone(), Vec::new()); }
            self.renaming.get_mut(&old_name).unwrap().push(new_name.clone());
            new_name
        }
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
