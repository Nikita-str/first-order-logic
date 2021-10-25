use std::{collections::LinkedList};
use crate::{common::name::Name, logic::substit::{Substitution, SubstitutionApply}};
use super::terms::Term;

pub struct PredicateExpr<N: Name>{
    name: N,
    params: Vec<Term<N>>,
}

impl<N: Name> PredicateExpr<N>{
    pub fn most_comon_unifier<'a>(p1: &Self, p2: &Self) -> Option<Substitution<N>>{
        if p1.params.len() != p2.params.len() || p1.name != p2.name { return None }
        if p1.params.len() == 0 { return Some(Substitution::new_empty()) }

        struct Line<N0: Name>{
            left: Term<N0>,
            right: Term<N0>,
        }
        
        struct Lines<N0: Name>{
            cur_ind: usize,
            sz: usize,
            lines: LinkedList<Line<N0>>,
        }
        let lines_subst = |lines: &mut Lines<_>, subst: &Substitution<_>|{
            for line in (&mut lines.lines).into_iter() {
                subst.apply(&mut line.left);
                subst.apply(&mut line.right);
            }
        };

        let mut lines = Lines{cur_ind: 0, sz: 0, lines: LinkedList::new() };
        for (t1, t2) in (& p1.params).into_iter().zip((& p2.params).into_iter()) {
            lines.lines.push_back(Line{ left: Term::new_from_other(t1), right: Term::new_from_other(t2) });
            lines.sz += 1;
        }

        lines.sz-= 1;
        let cur_line = lines.lines.pop_front().unwrap();
        let mut left = cur_line.left; 
        let mut right = cur_line.right; 



        if left.is_func() && right.is_func() {
            let f_sz = left.func_size();
            if f_sz != right.func_size() { return None }
            if left.get_func().get_name() != right.get_func().get_name() { return None }
            lines.sz += f_sz;
            for (l, r) in left.get_func().get_params().into_iter().zip(right.get_func().get_params().into_iter()){
                lines.lines.push_back(Line{ left: l.clone(), right: r.clone() });
            }
            todo!() //next circ
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

            lines.sz += 1;
            lines.lines.push_back(Line{left, right});

            todo!() //next circ
        }
        else if left.is_const() && right.is_const() { 
            if *left.get_const() != *right.get_const() { return None } 
            todo!() // next circ
        }
        else if left.is_var() && right.is_var(){
            let mut subst = Substitution::new_empty();
            subst.add_new_rule(left.get_var().clone(), right.clone());
            lines_subst(&mut lines, &subst);

            lines.sz += 1;
            lines.lines.push_back(Line{left, right});
            
            todo!()
        } else { panic!("this case must not exist!") }

        todo!()

    }
}
