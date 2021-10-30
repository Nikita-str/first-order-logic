use std::{collections::LinkedList};
use crate::{common::name::Name, logic::substit::{Substitution, SubstitutionApply}};
use super::terms::Term;

#[derive(Debug, PartialEq, PartialOrd)]
pub struct PredicateExpr<N: Name>{
    pub name: N,
    pub params: Vec<Term<N>>,
}

impl<N:Name> Default for PredicateExpr<N>{
    fn default() -> Self { Self { name: N::bad_name(), params: vec![] } }
}

impl<N: Name> PredicateExpr<N>{
    pub fn new(name: N, params: Vec<Term<N>>) -> Self{ Self{name, params} }

    pub fn get_name(&self) -> &N { &self.name }
    pub fn get_params(&self) -> &Vec<Term<N>> { &self.params }
    pub fn get_params_mut(&mut self) -> &mut Vec<Term<N>> { &mut self.params }

    pub fn set_var_index(&mut self, index: usize){
        for x in self.params.iter_mut() { x.set_var_index(index) }
    }
}

impl<N: Name> PredicateExpr<N>{
    pub fn most_comon_unifier<F>(p1: &Self, p2: &Self, printer: F) -> Option<Substitution<N>>
    where F: Fn(&Term<N>, &Term<N>)
    {
        if p1.params.len() != p2.params.len() || p1.name != p2.name { return None }
        if p1.params.len() == 0 { return Some(Substitution::new_empty()) }

        struct Line<N0: Name>{
            ok: bool,
            left: Term<N0>,
            right: Term<N0>,
        }
        let lr_is_ok = |l: &Term<_>, r: &Term<_>| l.is_var() && r.without_var() || l.is_var() && r.is_var();
        //let line_is_ok = |line: &Line<_>| line.ok || lr_is_ok(&line.left, &line.right);

        struct Lines<N0: Name>{
            sz: usize,
            ok: usize,
            lines: LinkedList<Line<N0>>,
        }
        let lines_subst = |lines: &mut Lines<_>, subst: &Substitution<_>|{
            for line in (&mut lines.lines).into_iter() {
                subst.apply(&mut line.left);
                subst.apply(&mut line.right);
            }
        };
        let lines_add = |lines: &mut Lines<_>, left, right| {
            lines.sz += 1;
            let ok = lr_is_ok(&left, &right);
            if ok { lines.ok += 1; }
            lines.lines.push_back(Line{ok, left, right});
        };

        let mut lines = Lines{sz: 0, ok: 0, lines: LinkedList::new() };
        for (t1, t2) in (& p1.params).into_iter().zip((& p2.params).into_iter()) {
            lines.lines.push_back(Line{ ok: false, left: Term::new_from_other(t1), right: Term::new_from_other(t2) });
            lines.sz += 1;
        }

        while lines.ok != lines.sz {
            lines.sz-= 1;
            let cur_line = lines.lines.pop_front().unwrap();
            if cur_line.ok { lines.ok -= 1 }
            let left = cur_line.left; 
            let right = cur_line.right; 

            //println!("L:{}   R:{}", left, right);
            //println!("TODO:DEL: {:?}", all_gl);
            printer(&left, &right);

            if left.is_func() && right.is_func() {
                let f_sz = left.func_size();
                if f_sz != right.func_size() { return None }
                if left.get_func().get_name() != right.get_func().get_name() { return None }
                
                for (l, r) in left.get_func().get_params().into_iter().zip(right.get_func().get_params().into_iter()){
                    lines_add(&mut lines, l.clone(), r.clone())
                }
                continue
            }
            else if left.is_func() && right.is_const() 
                || left.is_const() && right.is_func() { return None }
            else if left.is_func() && right.is_var() 
                || left.is_var() && right.is_func() 
                || left.is_var() && right.is_const()
                || left.is_const() && right.is_var() {
                let (left, right) = if left.is_var() { (left, right) } else { (right, left) }; 
                // left:Var & right:{Const, Func} 

                if right.contain(&left) { return None } // for {right is Func} case

                let mut subst = Substitution::new_empty();
                subst.add_new_rule(left.get_var().clone(), right.clone());
                lines_subst(&mut lines, &subst);

                lines_add(&mut lines, left, right);

                continue
            }
            else if left.is_const() && right.is_const() { 
                if *left.get_const() != *right.get_const() { return None } 
                continue
            }
            else if left.is_var() && right.is_var(){
                let mut subst = Substitution::new_empty();
                subst.add_new_rule(left.get_var().clone(), right.clone());
                lines_subst(&mut lines, &subst);

                lines_add(&mut lines, left, right);
                
                continue
            } else { panic!("this case must not exist!") }
        }

        let mut subst = Substitution::new_empty();
        for line in lines.lines {
            subst.add_new_rule(line.left.get_var().clone(), line.right)
        }

        Some(subst)
    }
}
