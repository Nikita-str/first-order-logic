use std::{fmt::Display, hash::Hash};

use crate::logic::{expr::Expr, operations::{BinaryOperations, UnaryOperations}, quants::Quants, terms::Term};

use super::{name::Name, name_holder::NameHolder};

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

impl<N:Name, T: Hash + Eq + Display> OkParse<N, T>{
    fn display_vars_helper<'i, I>(&self, iter: I)
    where I: IntoIterator<Item = &'i N>, N: 'i
    {
        print!("{{ ");
        let mut first = true;
        for var in iter.into_iter(){
            if !first { print!(", "); }
            let (sh, token) = self.name_holder.token_by_name_unchecked(var);
            if sh == 0 { print!("{}", token) } else { print!("{}_{}", token, sh) };
            first = false;
        }
        println!(" }}");        
    }

    pub fn display_free_vars(&self) { self.display_vars_helper(self.name_holder.get_free_vars().values()); }
    pub fn display_warning_vars(&self) { self.display_vars_helper(self.name_holder.get_waring_vars()); }
}

impl<N, T> Display for OkParse<N, T>
where  N: Name, T: Hash + Eq + Display, 
{
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result 
    { display_help_func(&self.expr, &self.name_holder, f) }
}

fn display_help_func<N, T>(expr: &Expr<N>, nh: &NameHolder<N, T>, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result 
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
            display_help_func(q.get_expr(), nh, f)?;
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
            display_help_func(uop.get_expr(), nh, f)?;
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
            display_help_func(lexpr, nh, f)?;
            if need_br { write!(f, "]")?; }
            
            write!(f, " {} ", op)?;

            let rexpr = bop.get_rexpr();
            let other_prior = rexpr.get_priority().unwrap_or(0);
            let need_br = rexpr.is_binary_op() && other_prior < prior;
            if need_br { write!(f, "[")?; }
            display_help_func(rexpr, nh, f)?;
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
