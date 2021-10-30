use std::{fmt::Display, hash::Hash};

use crate::logic::{default_term::DefaultTerm, expr::{Expr}, operations::{BinaryOperations, UnaryOperations}, quants::Quants, substit::{Substitution, SubstitutionApply}, term_type::TermType, terms::{Term, VarTerm}};

use super::{name::Name, name_holder::{NameHolder}};

pub struct OkParse<N:Name, T: Hash + Eq>{
    expr: Expr<N>,
    name_holder: NameHolder<N, T>
}
impl<N:Name, T: Hash + Eq> OkParse<N, T>{
    pub fn new(expr: Expr<N>, name_holder: NameHolder<N, T>) -> Self { Self{ expr, name_holder } }
    pub fn get_expr(&self) -> &Expr<N> { &self.expr }
    pub fn get_mut_expr(&mut self) -> &mut Expr<N> { &mut self.expr }
    pub fn get_name_holder(&self) -> &NameHolder<N, T> { &self.name_holder }
    pub fn get_mut_name_holder(&mut self) -> &mut NameHolder<N, T> { &mut self.name_holder }
    
    pub fn disassemble(self) -> (Expr<N>, NameHolder<N, T>) { (self.expr, self.name_holder) }

    /// close the logic formula
    /// 
    /// it means that any free vars will be limited with ∀ quantor before whole formula
    /// 
    /// for example: `R(x) -> ∃y P(y)` will transformed into `∀x (R(x) -> ∃y P(y))` 
    pub fn expr_close(self) -> Self{
        let (mut expr, mut name_holder) = (self.expr, self.name_holder);

       for var_name in name_holder.get_free_vars().values(){
            expr = expr.apply_quant(Quants::All, var_name.clone());
        }
        name_holder.clear_free_vars();
        
        Self{ expr, name_holder }
    }

    pub fn apply_expr_action<F>(self, f: F) -> Self
    where F: FnOnce(Expr<N>) -> Expr<N>
    { Self { expr: f(self.expr), name_holder: self.name_holder } }
}


impl<N:Name, T:Hash + Eq> OkParse<N,T> 
where 
T: DefaultTerm + Clone
{

    pub fn exist_quant_transform(&mut self) {
        self.exist_quant_transform_impl_term(T::default_term(TermType::Const), T::default_term(TermType::Func));
    }
}

impl<N:Name, T:Hash + Eq + Clone> OkParse<N,T>{
    /// (from ПНФ to ССФ)
    /// 
    /// any a1 ... any a_n exist x formula(a_1, ..., a_n, x) => formula(a_1, ..., a_n, new_f(a_1, ..., a_n)) 
    pub fn exist_quant_transform_impl_term(&mut self, term_const: T, term_func: T) {
        // let mut expr = &mut self.expr;
        // let mut params = vec![];
        // Self::exist_quant_create_subst(&mut self.name_holder, &mut self.expr, &mut params);
        // params
        let subst = Self::exist_quant_create_subst_42(&mut self.name_holder, &mut self.expr, term_const, term_func);
        subst.apply(&mut self.expr);
    }

    /* 
    // need inner struct for params
    // cause it work with unsafe ... ok just comment it

    fn _exist_quant_create_subst(
        nh: &mut NameHolder<N, T>, 
        expr: &mut Expr<N>, 
        params: &mut Vec<Term<N>>, 
        def_terms: &(T, T), 
        substit: &mut Substitution<N>)
    {
        if !expr.is_quant() { return }
        let quant = expr.get_expr_quant().get_quant();
        match quant {
            Quants::Exist => {
                let del_var_name = expr.get_expr_quant().get_var_name().clone();
                let q = expr.get_expr_quant().get_expr().clone();
                *expr = q;
                // here create substitute : TODO

                Self::exist_quant_create_subst(nh, expr, params, def_terms, substit)
            }
            Quants::All => {
                let var_name = expr.get_expr_quant().get_var_name().clone();
                params.push(Term::new_var_by_param(var_name));
                Self::exist_quant_create_subst(nh,expr.get_expr_quant_mut().get_expr_mut(), params, def_terms)
            }
        }
    }

    */

    fn exist_quant_create_subst_42(nh: &mut NameHolder<N, T>, expr: &mut Expr<N>, term_const: T, term_func: T) -> Substitution<N> {
        let mut params = vec![];
        let mut term_type = TermType::Const;
        let mut term = term_const;
        let mut substit = Substitution::new_empty();
        let mut cur: *mut Expr<N> = expr;
        loop {
            if ! unsafe{cur.as_ref()}.unwrap().is_quant() { return substit }
            let quant = unsafe {cur.as_ref()}.unwrap().get_expr_quant().get_quant();
            match quant {
                Quants::Exist => {
                    let del_var_name = unsafe{cur.as_ref()}.unwrap().get_expr_quant().get_var_name().clone();
                    let temp = unsafe{cur.as_mut()}.unwrap().get_expr_quant_mut().get_expr_mut().clone();
                    unsafe { *cur = temp };
                    let n_const_name = nh.get_name_uncond_new(term_type, term.clone());
                    substit.add_new_rule(VarTerm{name: del_var_name}, Term::gen_new_n_const(n_const_name, &params))
                }
                Quants::All => {
                    let var_name = unsafe{cur.as_ref()}.unwrap().get_expr_quant().get_var_name().clone();
                    term_type = TermType::Func;
                    if params.is_empty() { term = term_func.clone() }
                    params.push(Term::new_var_by_param(var_name));
                    cur = match unsafe {cur.as_ref()}.unwrap() { 
                        Expr::Quant(q) => { &mut q.borrow_mut().expr } 
                        _ => panic!("never")
                    }; 
                }
            }
        }
    }
}

impl<N:Name, T: Hash + Eq + Display> OkParse<N, T>{
    pub fn display_free_vars(&self) { self.name_holder._display_vars_helper(self.name_holder.get_free_vars().values()); }
    pub fn display_warning_vars(&self) { self.name_holder._display_vars_helper(self.name_holder.get_waring_vars()); }
}

impl<N, T> Display for OkParse<N, T>
where  N: Name, T: Hash + Eq + Display, 
{
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result 
    { display_expr_help_func(&self.expr, &self.name_holder, f) }
}

pub fn display_expr_help_func<N, T>(expr: &Expr<N>, nh: &NameHolder<N, T>, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result 
where 
N: Name,
T: Hash + Eq + Display,
{
    // TODO: del rec / stay only tail-rec

    let wr_name = |f: &mut std::fmt::Formatter<'_>, n: &N|{
        let (sh, token) = nh.token_by_name_unchecked(n);
        if sh == 0 { write!(f, "{}", token) } else { write!(f, "{}_{}", token, sh) }
    };

    struct RecClosure<'rc, N: Name> { cl: &'rc dyn Fn(&RecClosure<N>, &mut std::fmt::Formatter<'_>, &Vec<Term<N>>) -> std::fmt::Result }
    let wr_terms = RecClosure{
        cl: &|wr_terms, f: &mut std::fmt::Formatter<'_>, terms: &Vec<Term<N>>|{
            let mut first = true;
            for term in terms{
                if !first { write!(f, ", ")?; }
                match term {
                    Term::Const(_) => {wr_name(f, term.get_const().get_name())?; }
                    Term::Var(_) => { wr_name(f, term.get_var().get_name())?; }
                    Term::Func(_) => { 
                        let func = term.get_func();
                        wr_name(f, func.get_name())?;
                        write!(f, "(")?;
                        let f_terms =  func.get_params();
                        (wr_terms.cl)(&wr_terms, f, f_terms)?;
                        write!(f, ")")?;
                    }
                };
                first = false;
            }
            Ok(())
        }
    };

    match expr {
        Expr::Quant(_) => {
            let q = expr.get_expr_quant();
            match q.get_quant() {
                Quants::All => write!(f, "∀")?,
                Quants::Exist => write!(f, "∃")?,
            };
            wr_name(f, q.get_var_name())?;
            write!(f, ": ")?;
            let need_br  = q.get_expr().is_binary_op();
            if need_br { write!(f, "[")?; }
            display_expr_help_func(q.get_expr(), nh, f)?;
            if need_br { write!(f, "]")?; }
            Ok(())
        }
        Expr::UnaryOp(_) => {
            let uop = expr.get_expr_unary();
            match uop.get_op() { 
                UnaryOperations::Not => write!(f, "¬")?,
            };
            let need_br  = uop.get_expr().is_binary_op();
            if need_br { write!(f, "[")?; }
            display_expr_help_func(uop.get_expr(), nh, f)?;
            if need_br { write!(f, "]")?; }
            Ok(())
        }
        Expr::BinaryOp(_) => {
            let bop = expr.get_expr_binary();
            let prior = expr.get_priority().unwrap_or(0);
            let op = match bop.get_op() {
                BinaryOperations::Or => "∨",
                BinaryOperations::And => "&",
                BinaryOperations::Impl => "→",
            };

            let lexpr = bop.get_lexpr();
            let other_prior = lexpr.get_priority().unwrap_or(0);
            let need_br = lexpr.is_binary_op() && other_prior < prior;
            if need_br { write!(f, "[")?; }
            display_expr_help_func(lexpr, nh, f)?;
            if need_br { write!(f, "]")?; }
            
            write!(f, " {} ", op)?;

            let rexpr = bop.get_rexpr();
            let other_prior = rexpr.get_priority().unwrap_or(0);
            let need_br = rexpr.is_binary_op() && other_prior < prior;
            if need_br { write!(f, "[")?; }
            display_expr_help_func(rexpr, nh, f)?;
            if need_br { write!(f, "]")?; }

            Ok(())
        }
        Expr::Predicate(_) => {
            let p = expr.get_expr_predicate();
            wr_name(f, p.get_name())?;
            write!(f, "(")?;
            (wr_terms.cl)(&wr_terms, f, p.get_params())?;
            write!(f, ")")          
        }
        Expr::Empty => write!(f, "#"),
    }
}
