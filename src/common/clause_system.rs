use std::{fmt::Formatter, hash::Hash};
use crate::{common::{deep_copy::DeepCopy, ok_parse::display_predicate_helper}, logic::{expr::Expr, operations::BinaryOperations}};
use super::{name::Name, name_holder::NameHolder, ok_parse::display_expr_help_func, one_clause::OneClause};


pub struct ClauseSystem<N:Name, T:Hash + Eq>{
    name_holder: NameHolder<N, T>,
    system: Vec<OneClause<N>>,
}

impl<N:Name, T:Hash + Eq> ClauseSystem<N, T>{
    pub fn new(expr: &Expr<N>, name_holder: NameHolder<N, T>) -> Self{
        Self { name_holder, system: Self::get_expr_system(expr) }
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
}




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
