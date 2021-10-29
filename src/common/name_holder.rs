use std::{collections::HashMap, hash::Hash};
use crate::logic::{term_type::TermType, terms::{Term}};

use super::name::{Name};


/// 'T': top lvl name  
/// 'N': name
pub struct NameHolder<N: Name, T: Hash + Eq>{
    var_term: HashMap<N, Term<N>>,
    const_term: HashMap<N, Term<N>>,

    func_lens: HashMap<N, usize>,
    pred_lens: HashMap<N, usize>,

    name_to_token: HashMap<N, T>,
    names: HashMap<TermType, HashMap<T, N>>,
    last_names: HashMap<TermType, N>,

    //banned_names: HashMap<TermType, HashSet<N>>,

    /// key: initial name; vec<N> - stack of quantor restriction
    last_restriction: HashMap<N, Vec<N>>,

    renaming: HashMap<N, Vec<N>>,
    rename_to_init: HashMap<N, (usize, N)>,

    free_var: HashMap<N, N>, // initial name to free var
}

impl<N: Name, T: Hash + Eq> NameHolder<N, T>{
    pub fn new() -> Self{
        let mut names = HashMap::new();
        let mut last_names = HashMap::new();
        //let mut banned_names = HashMap::new();

        let mut insert = |t_type|{
            last_names.insert(t_type, N::first_name(t_type));
            names.insert(t_type, HashMap::new());
            //banned_names.insert(t_type, HashSet::new());
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
            name_to_token: HashMap::new(),
            names, 
            last_names,
            //banned_names, 
            last_restriction: HashMap::new(),
            rename_to_init: HashMap::new(),
            renaming: HashMap::new(),
            free_var: HashMap::new(),
        }
    }
}

impl<N: Name, T: Hash + Eq + Clone> NameHolder<N, T>{
    fn real_add_name(&mut self,term_type: TermType, new_term: T) -> N{
        let map = self.names.get_mut(&term_type).unwrap();
        let last_name = self.last_names.get(&term_type).unwrap().next_tst_name();
        self.last_names.insert(term_type, last_name.clone());
        if term_type == TermType::Const { 
            self.const_term.insert(last_name.clone(), Term::new_const_by_param(last_name.clone())); 
        } else if term_type == TermType::Var {
            self.var_term.insert(last_name.clone(), Term::new_var_by_param(last_name.clone())); 
        }
        if !map.contains_key(&new_term){
            self.name_to_token.insert(last_name.clone(), new_term.clone());
            map.insert(new_term, last_name.clone());
        }
        last_name
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
            let vec = self.renaming.get_mut(&old_name).unwrap();
            vec.push(new_name.clone());
            self.rename_to_init.insert(new_name.clone(), (vec.len(), old_name));
            new_name
        }
    }

    pub fn get_name(&mut self, term_type: TermType, new_term: T) -> N{
        if let Some(name) = self.get_last_existing_name(term_type, &new_term){
            if term_type == TermType::Var{
                if self.is_var_restr_by_quant(name){
                    let name = self.get_var_initial_name(&name).unwrap();
                    self.get_last_restr_name(&name).unwrap()
                } else {
                    self.get_free_var(new_term)
                }
            } else {
                name.clone()
            }
        } else { 
            self.real_add_name(term_type, new_term) 
        }
    }

    pub fn get_free_var(&mut self, term: T) -> N{
        let init_name = self.names.get(&TermType::Var).unwrap().get(&term);
        let init_name = init_name.unwrap().clone();
        if let Some(free_var_name) = self.free_var.get(&init_name) { 
            free_var_name.clone() 
        } else {
            let free_var = self.get_name_uncond_new(TermType::Var, term);
            self.add_free_var(init_name.clone(), free_var.clone());
            free_var
        }
    }
    pub fn add_new_free_var(&mut self, term: T) -> N{ 
        let free_var = self.get_name_uncond_new(TermType::Var, term);
        self.add_free_var(free_var.clone(), free_var.clone());
        free_var
    }
}

impl<N: Name, T: Hash + Eq> NameHolder<N, T>{
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

    //pub fn is_name_banned(&self, term_type: TermType, name: &N) -> bool { self.banned_names.get(&term_type).unwrap().contains(name) }
    //pub fn ban_name(&mut self, term_type: TermType, name: N) -> bool { self.banned_names.get_mut(&term_type).unwrap().insert(name) }
    //pub fn unban_name(&mut self, term_type: TermType, name: &N) -> bool { self.banned_names.get_mut(&term_type).unwrap().remove(name) }


    pub fn is_var_restr_by_quant(&self, name: &N) -> bool {
        if let Some(init) = self.get_var_initial_name(name){
            if let Some(restr_stack) = self.last_restriction.get(&init){
                !restr_stack.is_empty()
            } else {
                false
            }
        } else {
            false
        }
    }
    /// # panic
    /// - if name is not already added var name
    pub fn restr_var_name(&mut self, name: N){
        if let Some(init) = self.get_var_initial_name(&name) {
            if let Some(vec) = self.last_restriction.get_mut(&init) {
                vec.push(name);
            } else {
                let vec = vec![name];
                self.last_restriction.insert(init.clone(), vec);
            }
        } else {
            panic!("it is not var or this var was created outside this NameHolder")
        }
    }
    fn get_last_restr_name(&mut self, init: &N) -> Option<N> {
        self.last_restriction.get(&init).and_then(|vec|vec.last().and_then(|x|Some(x.clone())))
    }

    /// # panic
    /// - if name was not restricted previously
    pub fn remove_last_var_restr(&mut self, name: &N) {
        let init = self.get_var_initial_name(name).unwrap().clone();
        self.last_restriction.get_mut(&init).unwrap().pop().unwrap();
    }

    fn add_free_var(&mut self, init_name: N, var_name: N){ self.free_var.insert(init_name, var_name); }
    pub fn is_free_var(&self, var_name: &N) -> bool { self.free_var.contains_key(var_name) }
    pub fn clear_free_vars(&mut self){ self.free_var.clear() }
    pub fn exist_free_vars(&self) -> bool { self.free_var.len() > 0 }

    pub fn get_free_vars(&self) -> &HashMap<N, N> { &self.free_var }
    pub fn get_init_of_renaming(&self) -> &HashMap<N, (usize, N)> { &self.rename_to_init }

    fn get_var_initial_name(&self, name: &N) -> Option<N> {
        if self.name_to_token.contains_key(name) {
            return Some(name.clone())
        }

        self.rename_to_init.get(name).and_then(
            |(_, name)|
            self.name_to_token.get(name).and_then(
                |t|Some(self.names.get(&TermType::Var).unwrap().get(t).unwrap().clone())
            )
        )
    }

    pub fn token_by_name_unchecked(&self, name: &N) -> (usize, &T) {
        let mut sh = 0;
        let name = if let Some(name) = self.rename_to_init.get(name) { sh = name.0; &name.1 } else { name };
        (sh, self.name_to_token.get(name).unwrap())
    }
}
