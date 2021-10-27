use first_order_logic::{common::name::{StdName}, logic::{term_type::TermType, predicate_expr::PredicateExpr, terms::{ConstTerm, FuncTerm, Term, VarTerm}}};

#[allow(non_snake_case)]
fn main() {
    let t_c = ConstTerm{ name: StdName{ name_type: TermType::Const, name: 1, index: 0 } };
    let t_X = VarTerm{ name: StdName{ name_type: TermType::Var, name: 1, index: 0 } };
    let t_Y = VarTerm{ name: StdName{ name_type: TermType::Var, name: 2, index: 0 } };

    let t_c = Term::new_const(t_c);
    let t_X = Term::new_var(t_X);
    let t_Y = Term::new_var(t_Y);
    let t_f = Term::new_func(FuncTerm{ 
        name: StdName{ name_type: TermType::Func, name: 1, index: 0 },
        params: vec![t_X.clone()],
    });

    let P_left = PredicateExpr{ 
        name: StdName{ name_type: TermType::Pred, name: 1, index: 0 },
        params: vec![t_c.clone(), t_X.clone(), t_f.clone()], 
    };

    let P_right = PredicateExpr{ 
        name: StdName{ name_type: TermType::Pred, name: 1, index: 0 },
        params: vec![t_c.clone(), t_Y.clone(), t_Y.clone()], 
    };

    if let Some(unif) =  PredicateExpr::most_comon_unifier(&P_left, &P_right){
        println!("UNIF: {}", unif)
    } else {
        println!("N0 :(")
    }

    println!("");

    let t_a = Term::new_const(ConstTerm{ name: StdName{ name_type: TermType::Const, name: 1, index: 0 } });
    let t_b = Term::new_const(ConstTerm{ name: StdName{ name_type: TermType::Const, name: 2, index: 0 } });
    let t_X = Term::new_var(VarTerm{ name: StdName{ name_type: TermType::Var, name: 1, index: 0 } });
    let t_Y = Term::new_var(VarTerm{ name: StdName{ name_type: TermType::Var, name: 2, index: 0 } });
    let t_Z = Term::new_var(VarTerm{ name: StdName{ name_type: TermType::Var, name: 3, index: 0 } });
    let t_h = Term::new_func(FuncTerm{ 
        name: StdName{ name_type: TermType::Func, name: 1, index: 0 },
        params: vec![t_X.clone()],
    });
    let t_g = Term::new_func(FuncTerm{ 
        name: StdName{ name_type: TermType::Func, name: 2, index: 0 },
        params: vec![t_a.clone()],
    });
    let t_f1 = Term::new_func(FuncTerm{ 
        name: StdName{ name_type: TermType::Func, name: 3, index: 0 },
        params: vec![t_X.clone(), t_b.clone(), t_Z.clone()],
    });
    let t_f2 = Term::new_func(FuncTerm{ 
        name: StdName{ name_type: TermType::Func, name: 3, index: 0 },
        params: vec![t_g.clone(), t_Y.clone(), t_Z.clone()],
    });
    
    let P_left = PredicateExpr{ 
        name: StdName{ name_type: TermType::Pred, name: 1, index: 0 },
        params: vec![t_Z.clone(), t_f1.clone()], 
    };

    let P_right = PredicateExpr{ 
        name: StdName{ name_type: TermType::Pred, name: 1, index: 0 },
        params: vec![t_h.clone(), t_f2.clone()], 
    };

    if let Some(unif) =  PredicateExpr::most_comon_unifier(&P_left, &P_right){
        println!("UNIF: {}", unif)
    } else {
        println!("N0 :(")
    }
}
