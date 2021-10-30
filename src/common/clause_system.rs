use std::{collections::{HashMap, HashSet, LinkedList}, fmt::Formatter, hash::Hash};
use crate::{common::{deep_copy::DeepCopy, ok_parse::display_predicate_helper}, logic::{expr::Expr, operations::BinaryOperations}};
use super::{name::Name, name_holder::NameHolder, one_clause::OneClause};


pub struct ClauseSystem<N:Name, T:Hash + Eq>{
    action_info: ActionInfo,
    name_holder: NameHolder<N, T>,
    system: Vec<OneClause<N>>,
}

impl<N:Name, T:Hash + Eq> ClauseSystem<N, T>{
    pub fn new(expr: &Expr<N>, name_holder: NameHolder<N, T>) -> Self{
        Self { action_info: ActionInfo::new(), name_holder, system: Self::get_expr_system(expr) }
    }

    fn get_expr_system(expr: &Expr<N>) -> Vec<OneClause<N>>{
        let mut system = vec![];
        let mut init_expr = expr.clone();
        while init_expr.is_quant() { 
            let temp = init_expr.get_expr_quant().get_expr().clone();
            init_expr = temp
        }
        let mut not_procced_expr = vec![init_expr];
        while !not_procced_expr.is_empty() {
            let exp = not_procced_expr.pop().unwrap();
            match exp {
                Expr::BinaryOp(bop) => {
                    match bop.borrow().get_op() {
                        BinaryOperations::And => {
                            not_procced_expr.push(bop.borrow().get_rexpr().clone());
                            not_procced_expr.push(bop.borrow().get_lexpr().clone());
                        }
                        BinaryOperations::Or => {
                            system.push(Expr::new_binary_by_expr(bop.borrow().deep_copy()))
                        }
                        BinaryOperations::Impl => panic!("initial expr is not in cnf"),
                    }
                }
                Expr::Quant(_) => panic!("initial expr is not in cnf"),
                _ => { system.push(exp.deep_copy()) }
            }
        }

        let mut clauses = vec![];
        for x in system { clauses.push(OneClause::new(x)) }
        clauses
    }

    
    pub fn set_unique_var_index(&mut self) {
        for (index, clause) in self.system.iter_mut().enumerate() { clause.set_var_index(index + 1) }
    }
}



pub enum ResolventResult{
    EmptyClauseDerivable,
    TooManyIterations,
    EmptyClauseNotDerivable,
}

impl<N:Name, T:Hash + Eq> ClauseSystem<N, T>{
    fn get_new_var_index(&self) -> usize { self.system.len() + 1 }

    /// return line index (not index of var)
    fn add_in_system(&mut self, mut clause: OneClause<N>) -> usize{
        clause.set_var_index(self.get_new_var_index());
        self.system.push(clause);
        self.system.len() - 1
    }

    /// always ended
    pub fn made_all_gluing(&mut self){
        let start_indexes: Vec<_> = self.system.iter().enumerate().map(|(ind, _)|ind).collect();
        let mut need_gluing = LinkedList::new();
        need_gluing.push_back(start_indexes);
        while !need_gluing.is_empty() {
            let next = need_gluing.pop_front().unwrap();
            for line in next{
                let new_lines = self.made_gluing_only_for_line(line);
                if new_lines.len() > 0 { need_gluing.push_back(new_lines); }
            }
        }
    }

    /// it may be infinity looped => use max_iteration
    pub fn made_all_resolvent(&mut self, max_iteration: Option<usize>) -> ResolventResult{
        if self.system.len() < 2 { return ResolventResult::EmptyClauseNotDerivable } // ? or is_empty ? 
        
        let mut cur_index = self.system.len() - 1;
        let mut second_index = self.system.len() - 2;

        let mut rest_iter = max_iteration.unwrap_or(1);
        while rest_iter > 0 {
            let exception = self.action_info.get_resolv((second_index, cur_index));
            let left = self.system.get(second_index).unwrap();
            let right = self.system.get(cur_index).unwrap();
            let indexes = OneClause::get_potential_contrary_pairs(left, right, exception);

            let mut tried = 0;
            let mut new_clause = None;
            
            'contrary_pair: 
            for index_lr_pair in &indexes {
                tried = tried + 1;
                let x = OneClause::resolvent(left, right, *index_lr_pair, |_,_|{});
                if x.is_some() { new_clause = x; break'contrary_pair } 
            }

            // add all that we checked (we can checked not all)
            'add_tried:
            for (ind, try_pair) in indexes.iter().enumerate(){
                if ind == tried { break'add_tried }
                self.action_info.add_resolv((second_index, cur_index), *try_pair)
            }

            if let Some(clause) = new_clause{
                cur_index = self.add_in_system(clause);
                self.made_gluing_only_for_line(cur_index);
                second_index = cur_index - 1;
            } else {
                if second_index == 0 {
                    if cur_index == 1 { return ResolventResult::EmptyClauseNotDerivable }
                    cur_index = cur_index - 1; 
                } else {
                    second_index = second_index - 1;
                }
            }

            if max_iteration.is_some() { rest_iter = rest_iter - 1; }
        }
        return ResolventResult::EmptyClauseNotDerivable 
    }

    /// return all line index that was created while gluing
    pub fn made_gluing_only_for_line(&mut self, line: usize) -> Vec<usize> {
        let mut new_line_index = vec![];
        let x = self.system.get(line).unwrap();
        let all_gl = x.get_gluing_pairs();
        for pair in all_gl {
            if self.action_info.is_already_glued(line, pair) { continue }

            let x = self.system.get(line).unwrap();
            let gl_res = x.gluing(pair, |_, _|{}); //TODO:printer
            if let Some(clause) = gl_res { new_line_index.push(self.add_in_system(clause)); }

            self.action_info.add_glued(line, pair);
        }
        new_line_index
    }
}


struct ActionInfo{
    /// which gluing was already try: 
    /// `key: (clause index + (left, right))`. 
    /// where left & right - pair of index for gluing (left < right)
    try_gluing: HashSet<(usize, (usize, usize))>,

    /// `key : (clause_index, clause_index)` `values: aready checked pair` 
    try_resolv: HashMap<(usize, usize), HashSet<(usize, usize)>>
}
impl ActionInfo{
    fn new() -> Self { Self{ try_gluing: HashSet::new(), try_resolv: HashMap::new() } }
    fn is_already_glued(&self, clause_index: usize, gluing_pair: (usize, usize)) -> bool {
        self.try_gluing.contains(&(clause_index, gluing_pair))
    }
    fn add_glued(&mut self, clause_index: usize, gluing_pair: (usize, usize)) {
        self.try_gluing.insert((clause_index, gluing_pair));
    }

    fn get_resolv(&mut self, clause_pair_index: (usize, usize)) -> &HashSet<(usize, usize)>{
        if clause_pair_index.0 >= clause_pair_index.1 { panic!("get it normal way!") }
        if self.try_resolv.get(&clause_pair_index).is_none() { 
            let new_set = HashSet::new();       
            self.try_resolv.insert(clause_pair_index, new_set);
        } 
        self.try_resolv.get(&clause_pair_index).unwrap()   
    }

    fn add_resolv(&mut self, clause_pair_index: (usize, usize), resolv_pair: (usize, usize)) {
        if clause_pair_index.0 >= clause_pair_index.1 { panic!("get it normal way!") }
        if let Some(set) = self.try_resolv.get_mut(&clause_pair_index) {
            set.insert(resolv_pair);
        } else {
            let mut clause_already = HashSet::new();
            clause_already.insert(resolv_pair);
            self.try_resolv.insert(clause_pair_index, clause_already);
            panic!("we not must be here, we create this on get_resolv")
        }
    }

}


// DISPLAY   ###   DISPLAY   ###   DISPLAY   ###   DISPLAY   ###   DISPLAY   ###   DISPLAY   ###   DISPLAY
// [+]       

impl<N:Name, T: Hash + Eq + std::fmt::Display> std::fmt::Display for ClauseSystem<N, T>{
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        writeln!(f, "  {{")?;
        for clause in &self.system {
            write!(f, "    ")?;
            let mut first = true;
            for x in clause.get_elems() { 
                if !first { write!(f, " ∨ ")?; }
                if x.is_negative() { write!(f, "¬")?; }
                display_predicate_helper(f, &self.name_holder, x.get_predicate())?;
                first = false;
             }
            writeln!(f, "")?;
        }
        writeln!(f, "  }}")?;
        Ok(())
    }    
}
