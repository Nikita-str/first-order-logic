
#[cfg(test)]
mod parse_str_test{
    use crate::common::clause_system::ClauseSystem;
    use crate::common::{name::StdName, parse, ok_parse::OkParse};
    use crate::common::parse_str::ParseStr;
    use crate::logic::expr::Expr;
    use crate::logic::predicate_expr::PredicateExpr;
    use crate::logic::substit::{DisplaySubst, SubstitutionApply};

    fn test_help(ps: ParseStr, expects: Vec<&str>){
        let mut iter = ps.into_iter(); 
        for expect in expects{
            assert_eq!(iter.next().unwrap().get_s(), expect)
        }
        assert!(iter.next().is_none()) 
    }

    #[test]
    fn iterator_tests(){
        test_help(
            ParseStr::new("∃x∀y P(x,y)   →∀yi∃x    P(x,  yi)"), 
        vec!["∃", "x", "∀", "y", " ", "P", "(", "x", ",", "y", ")", "   ", "→", "∀", "yi", "∃", "x", "    ", "P", "(", "x", ",", "  ", "yi", ")"]
        );

        test_help(
            ParseStr::new("∃∃ ||| &&& ---> wow_so_cool_name_101 25x25 (())"),
            vec!["∃", "∃", " ", "|||", " ", "&&&", " ", "--->", " ", "wow_so_cool_name_101", " ", "25", "x25", " ", "(", "(", ")", ")"],
        );
    }


    fn parse_wrong_test_help(parse_str: &str){
        let ruleset = ParseStr::create_std_ruleset();
        let ps = ParseStr::new(parse_str);
        assert!(parse::parse::<StdName, _, _>(&ruleset, &mut ps.into_iter()).is_err());
    }

    fn parse_ok_test_help(parse_str: &str){
        let ruleset = ParseStr::create_std_ruleset();
        let ps = ParseStr::new(parse_str);
        assert!(parse::parse::<StdName, _, _>(&ruleset, &mut ps.into_iter()).is_ok());
    }

    #[test]
    fn parse_wrong_test(){
        parse_wrong_test_help("a");
        parse_wrong_test_help("∃");
        parse_wrong_test_help("∃x");
        parse_wrong_test_help("∃x&");
        parse_wrong_test_help("P(f)");
        parse_wrong_test_help("P(a, b, c");
        parse_wrong_test_help("P(a)&");
        parse_wrong_test_help("P(a) & & P(b)");
        parse_wrong_test_help("P(a)->c");
        parse_wrong_test_help("P(a)->∃");
        parse_wrong_test_help("P(a)->∃x");
        parse_wrong_test_help("P(a)->∃x P");
        parse_wrong_test_help("P(f(c), P(a,b))");

        parse_wrong_test_help("()");
        parse_wrong_test_help("(P(x) -> (P(y))");
        
        parse_wrong_test_help("for_any x exist P(x)");
        parse_wrong_test_help("for_any x P(x))");
        parse_wrong_test_help("P(x))");
        parse_wrong_test_help("(P(x)");
        parse_ok_test_help("((( P(x) )))");
        parse_wrong_test_help("((( P(x) ))))");
        parse_wrong_test_help("(((( P(x) )))");

        parse_ok_test_help("P(x) -> ((( P(x) )))");
        parse_ok_test_help("( P(x) & (P(y)) )    --->    (( P(x) & ( P(x) ) | Q(x) ))");
        parse_ok_test_help("( P(x) & (P(y) |  exist z P(z)) ) & P(y)    --->    (( P(x) & ( P(x) ) | Q(x) )) & P(x)");
        parse_wrong_test_help("( P(x) & (P(y) | | exist z P(z)) ) & P(y)    --->    (( P(x) & ( P(x) ) | Q(x) )) & P(x)");
        parse_wrong_test_help("( P(x) & (P(y) | P(z))) ) & P(y)    --->    (( P(x) & ( P(x) ) | Q(x) )) & P(x)");
        parse_wrong_test_help("( P(x) & (P((y) |  exist z P(z)) ) & P(y)    --->    (( P(x) & ( P(x) ) | Q(x) )) & P(x)");
        parse_wrong_test_help("( P(x) & ((P(y) |  exist z P(z)) ) & P(y)    --->    (( P(x) & ( P(x) ) | Q(x) )) & P(x)");
        parse_wrong_test_help("( P(x) & (P(y) |  exist z P(z)) ) & P(y)    --->    (( P(x) & ( P(x) )) | Q(x) )) & P(x)");
        parse_wrong_test_help("( P(x) & (P(y) |  exist z P(z)) ) & P(y)    --->    (( P(x) & ( P(x) ) |) Q(x) ) & P(x)");

        parse_wrong_test_help("P(x) & P(x, y)");
        parse_ok_test_help("P(x, y) & R(y)");
        parse_ok_test_help("P(x, y) & P(x, y)");
        parse_wrong_test_help("P(x, y) & P(z, x) & P(x) & P(x, y)");
        parse_ok_test_help("P(x, y) & P(z, x) & Q(x) & P(x, y)");

        parse_ok_test_help("P(f(a, b), f(a, b))");
        parse_wrong_test_help("P(f(a, b), f(a))");
        parse_ok_test_help("P(f(a, f(b, c)), c)");
        parse_wrong_test_help("P(f(a, f(b, c, a)), c)");
        parse_ok_test_help("P(a, f(b, c)) & P(f(a, b), c)");
        parse_wrong_test_help("P(a, f(b, c)) & P(f(b), c)");

        parse_ok_test_help("for_any x for_any y exist z P(x, y, z)");
        parse_ok_test_help("for_any x for_any y for_any z P(x, y, z)");
        //it no more err! parse_wrong_test_help("for_any x for_any y exist x P(x, y, x)");
        //                parse_wrong_test_help("for_any x for_any y for_any x P(x, y, x)");
        parse_ok_test_help("for_any x for_any y exist x P(x, y, x)");
        parse_ok_test_help("for_any x for_any y for_any x P(x, y, x)");

        parse_ok_test_help("for_any x P(x, b)");
        parse_wrong_test_help("for_any a P(a, b)");

        //it no more err! (but very dubious struct)  parse_wrong_test_help("exist x exist x P(x, x)");
        parse_ok_test_help("exist x exist x P(x, x)");
        
        parse_ok_test_help("exist x P(x) --> exist x P(x)");
        parse_ok_test_help("exist x P(a, x) --> exist x P(x, b)");
         //it no more err! (but very dubious struct)  parse_wrong_test_help("exist x P(a, x) --> exist x any x P(x, x)");
        parse_ok_test_help("exist x P(a, x) --> exist x any x P(x, x)");
        parse_wrong_test_help("exist x P(a, x) --> exist x P(x)");

        parse_ok_test_help("! exist x P(x) --> for_all x !P(x)");

        parse_wrong_test_help("P(x) P(x)");
        parse_wrong_test_help("P(x) & (P(x) P(x))");
        parse_wrong_test_help("P(x) & P(x) (P(x))");
    }

    #[test]
    fn parse_test(){
        let ruleset = ParseStr::create_std_ruleset();

        let pr = |expr: Result<OkParse<_, _>, _>|{            
            match expr {
                Err(_) => println!("NONE :("),
                Ok(ok) => {
                    println!("EXPR : {:?}", ok.get_expr())
                }
            }
        };

        let ps = ParseStr::new("P(f(a, b), g(b, h(c)))"); // +
        let expr = parse::parse::<StdName, _, _>(&ruleset, &mut ps.into_iter());
        pr(expr);

        println!("\n\n\n");

        let ps = ParseStr::new("∃x∀y P(x,y)   →∀y_i∃x    P(x,  y_i)"); // +
        let expr = parse::parse::<StdName, _, _>(&ruleset, &mut ps.into_iter());
        pr(expr);


        println!("\n\n\n");

        let ps = ParseStr::new("P(a,b)&P(b,c)&P(c,a)"); // +
        let expr = parse::parse::<StdName, _, _>(&ruleset, &mut ps.into_iter());
        pr(expr);


        println!("\n\n\n");

        // must : And(Or(.., And(...)), ...)
        let ps = ParseStr::new("(P(a,b)|P(b,c)&P(c,a))&P(a,b)"); 
        let expr = parse::parse::<StdName, _, _>(&ruleset, &mut ps.into_iter());
        pr(expr);


        println!("\n\n\n");

        // must : Or(And(..), And(..))
        let ps = ParseStr::new("P(a,b)&P(b,c)|P(c,a)&P(a,b)"); 
        let expr = parse::parse::<StdName, _, _>(&ruleset, &mut ps.into_iter());
        pr(expr);

    }

    #[test]
    fn move_not_test(){
        let ruleset = ParseStr::create_std_ruleset();

        let ps = ParseStr::new("! ! ! ! ! ! P(a, x, f(b, c))"); 
        let ok = parse::parse::<StdName, _, _>(&ruleset, &mut ps.into_iter());
        let mut ok = match ok { Ok(ok) => ok, Err(_) => panic!("not parsed") };
        ok.get_mut_expr().logical_not_moving();
        let expr = ok.get_expr();
        match expr {
            Expr::Predicate(_) => {}
            _ => panic!(" not must be just dissapear"),
        }

        let ps = ParseStr::new("! ! ! ! ! P(a, x, f(b, c))"); 
        let ok = parse::parse::<StdName, _, _>(&ruleset, &mut ps.into_iter());
        let mut ok = match ok { Ok(ok) => ok, Err(_) => panic!("not parsed") };
        ok.get_mut_expr().logical_not_moving();
        let expr = ok.get_expr();
        match expr {
            Expr::UnaryOp(uop) => {
                match uop.borrow().get_expr() {
                    Expr::Predicate(_) => {}
                    _ => panic!("after not must stay predicate")
                }
            }
            _ => panic!("must stay only one not and one predicate"),
        }

    }
    
    #[test]
    fn parse_test_3(){
        let ruleset = ParseStr::create_std_ruleset();

        //let ps = ParseStr::new("! exist x P(x) --> for_all x !P(x) & R(y)"); 
        //let ps = ParseStr::new("P(x, y) & exist x (R(x) & P(y, x)) & R(y) & R(x)"); // free vars: x_0, y_0, x_3 => 1, 2, 4 
        //let ps = ParseStr::new("for_all x (P(x, y) & R(x, y)) ---> (Q(x) & (Q(y) | Q(x))) "); 
        
        let ps = ParseStr::new("P(x, y) --> ( P(b,b) & P(y, y)->P(a,b) )"); // HERE ONLY 2 VARS!!!
        let expr = parse::parse::<StdName, _, _>(&ruleset, &mut ps.into_iter());
        
        match expr {
            Err(_) => println!("NONE :("),
            Ok(mut ok) => {
                print!("free vars: ");
                ok.display_free_vars();
                println!("EXPR : {}", ok);
                println!("try impl transformation: ");
                ok.get_mut_expr().impl_transformation();
                println!("EXPR : {}", ok);
            }
        }
    }

    #[test]
    fn parse_test_4(){
        let ruleset = ParseStr::create_std_ruleset();
        //let ps = ParseStr::new("¬ (P(x, c) & ! R(f(a, b, c)))");
        //let ps = ParseStr::new("(exist x_1 any y_1 exist z_1 P(x_1,y_1, z_1)) & (any x_2 exist y_2 R(x_2, y_2) | exist x_3 any y_3 R(x_3, y_3))");
        
        //let ps = ParseStr::new("¬ ∃x( (P(x) & (∀x P(x) → ∃y R(x, y))) → ∃y R(x, y) )");
        let ps = ParseStr::new("exist x (P(x) or any x P(x) or P(x)) or P(x) or any x (P(x) & P(x)) and P(x)");
        let expr = parse::parse::<StdName, _, _>(&ruleset, &mut ps.into_iter());
        match expr {
            Err(err) => println!("NONE :(  [err={:?}]", err),
            Ok(mut ok) => {
                println!("EXPR : {}", ok);
                println!("move not:");
                ok.get_mut_expr().logical_not_moving();
                println!("EXPR : {}", ok);
                println!("move out quants: ");
                ok.get_mut_expr().quant_outing();
                println!("expr: {}", ok);
            }
        }
    }
    #[test]
    fn parse_test_5(){
        let ruleset = ParseStr::create_std_ruleset();

        //let ps = ParseStr::new("(P(x_c, y_c) or (P(x_d, y_d) and P(x_g, y_g))) and (P(x_a, y_a) or P(x_b, y_b))");
        //let ps = ParseStr::new("(P(x_c, y_c) or (P(x_d, y_d) and P(x_g, y_g))) or (P(x_a, y_a) or P(x_b, y_b))");
        let ps = ParseStr::new("(P(x_a) & (P(x_e) ∨ P(x_b) ∨ P(x_c)))  and (P(x_n) & P(x_p) ∨ (P(x_f) & P(x_g)))");
        let expr = parse::parse::<StdName, _, _>(&ruleset, &mut ps.into_iter());
        match expr {
            Err(err) => println!("NONE :(  [err={:?}]", err),
            Ok(mut ok) => {
                println!("EXPR : {}", ok);
                println!("to cnf:");
                ok.get_mut_expr().to_cnf();
                println!("EXPR : {}", ok);
            }
        }
    }

    #[test]
    fn parse_test_6(){
        let ruleset = ParseStr::create_std_ruleset();
        //let ps = ParseStr::new("exist x (P(x, f(y, g(x, a), a), b) | exist y P(y, x, x)) | P(h(x), g(x, y), a)");

        //let ps = ParseStr::new("exist x any x_any exist y exist z any y_any exist w (P(f(a, x), y_any, y, g(z, x_any, w), x, x_any, y, w) & (R(x, y_any) | R(a, z)))");
        //let ps = ParseStr::new("exist x any y exist z ((P(x, y) & P(x, z)) | (P(y, z) & P(y, z)))");
        //let ps = ParseStr::new("exist x any y any z ((P(x, y) | (!P(x, z) & R(z, c, y))) | (P(a, b) & P(b, h(z))) | any z P(a, z))");
        let ps = ParseStr::new("exist x any y exist z ((P(x, y) & !P(x, x)) | (P(y, y) & P(z, z)))");
        let expr = parse::parse::<StdName, _, _>(&ruleset, &mut ps.into_iter());
        match expr {
            Err(err) => println!("NONE :(  [err={:?}]", err),
            Ok(ok) => {
                println!("EXPR : {}", ok);
                println!("to cnf:");
                let mut ok = ok.expr_close();
                ok.get_mut_expr().quant_outing();
                ok.get_mut_expr().to_cnf();
                println!("EXPR : {}", ok);

                println!("");
                println!("");
                println!("TEST:");
                let subst = ok.exist_quant_transform();
                println!("for deleting exist quantor we use substitution: {}", DisplaySubst{nh: ok.get_name_holder(), substs: &subst} );
                println!("now our expr: {}", ok);

                let (expr, nh) = ok.disassemble();
                let mut cs = ClauseSystem::new(&expr, nh);
                println!("clause system: ");
                println!("{}", cs);

                println!("add index for each clause: ");
                cs.set_unique_var_index();
                println!("{}", cs);

                println!("");
                println!("make all possible gluing: ");
                cs.made_all_gluing();
                println!("{}", cs);

            }
        }
    }

    #[test]
    fn parse_test_7(){
        let ruleset = ParseStr::create_std_ruleset();

        //let ps = ParseStr::new("P(a)");
        //let ps = ParseStr::new("P(y, f(x)) & (Q(y) | !Q(z) | !P(y, f(z)) | !Q(w)) & Q(b) & !Q(z)");
        //let ps = ParseStr::new("(Q(a) or !Q(a)) & R(b) & !Q(a) & P(y, f(x)) & Q(b) & (Q(y) | !Q(z) | !P(y, f(z))) & R(c)");
        //let ps = ParseStr::new("(P(a) or R(b)) and (R(x) or not R(y) or P(c)) and Q(a) and not Q(x) and (P(c) or not P(x))");

        //cur init example: "any x (P(x) --> exist y R(x, f(y))) --> (exist x !P(x) or for_all x exist z R(x, z))"
        // it eq: 
        let ps = ParseStr::new("(not P(x) or R(x, f(f_1(x_1, x))) ) & P(x_1) & !R(c, z)");
        let expr = parse::parse::<StdName, _, _>(&ruleset, &mut ps.into_iter());
        match expr {
            Err(err) => println!("NONE :(  [err={:?}]", err),
            Ok(ok) => {
                let (expr, nh) = ok.disassemble();
                let mut cs = ClauseSystem::new(&expr, nh);
                println!("clause system: ");
                println!("{}", cs);

                println!("add index for each clause: ");
                cs.set_unique_var_index();
                println!("{}", cs);

                println!("");
                println!("make all possible gluing: ");
                cs.made_all_gluing();
                println!("{}", cs);
                
                println!("");
                println!("try resolvent (no more than 15): ");
                cs.made_all_resolvent(Some(15));
                println!("{}", cs);
            }
        }
    }


    /// not actual (here bug work, but when need we use deep_copy)
    #[test]
    fn show_one_bug_test(){
        let ruleset = ParseStr::create_std_ruleset();
        let ps = ParseStr::new("(P(a) & P(b)) | P(c)");
        let expr = parse::parse::<StdName, _, _>(&ruleset, &mut ps.into_iter());
        match expr {
            Err(err) => println!("NONE :(  [err={:?}]", err),
            Ok(mut ok) => {
                println!("EXPR : {}", ok);
                println!("to cnf:");
                ok.get_mut_expr().to_cnf();
                println!("EXPR : {}", ok);
                
                println!("now show bug (why cant just split it into system of logical clauses):");
                let a_name = match ok.get_mut_expr().get_expr_binary_mut().get_lexpr_mut().get_expr_binary_mut().get_rexpr_mut() {
                    Expr::Predicate(p) => { p.borrow_mut().params.get(0).unwrap().clone() }
                    _ => panic!("it predicate, honestly")
                };

                match ok.get_mut_expr().get_expr_binary_mut().get_lexpr_mut().get_expr_binary_mut().get_lexpr_mut() {
                    Expr::Predicate(p) => {
                        let old_name = p.borrow_mut().params.get(0).unwrap().clone();
                        assert_eq!(old_name.get_const().get_name().name, 3);
                        p.borrow_mut().params[0] = a_name;
                    }
                    _ => panic!("it predicate, honestly")
                }

                println!("see on P(c), i change only one! :");
                println!("EXPR : {}", ok);
            }
        }
    }

    fn get_mcu(str_predicate_a: &str, str_predicate_b: &str, await_exist: bool){
        let ruleset = ParseStr::create_std_ruleset();
        let expr_str = "".to_owned() + str_predicate_a + " | " + str_predicate_b;
        let ps = ParseStr::new(&expr_str);
        let expr = parse::parse::<StdName, _, _>(&ruleset, &mut ps.into_iter());
        let mut expr = expr.unwrap();
        println!("expr before subst: {}", expr);
        let subst = match expr.get_expr() { 
            Expr::BinaryOp(bop) => 
                PredicateExpr::most_comon_unifier(
                    &bop.borrow().get_lexpr().get_expr_predicate(), 
                    &bop.borrow().get_rexpr().get_expr_predicate(), 
                    |_,_|{}
                ),
            _ => panic!("give two predicate, please"),
        };
        if subst.is_none() { 
            assert!(!await_exist, "awaited existing of substitution");
            println!("subst is empty (ok)") 
        }
        else { 
            let subst = subst.unwrap();
            assert!(await_exist, "awaited not existing of substitution");
            println!("mcu: {}", DisplaySubst{ nh: &expr.get_name_holder(), substs: &subst});
            subst.apply(expr.get_mut_expr());
            println!("expr after subst: {}", expr);
            let (expr, nh) = expr.disassemble(); 
            match expr {
                Expr::BinaryOp(bop) => {
                    let borrow = bop.borrow();
                    let left = borrow.get_lexpr();    
                    let right = borrow.get_lexpr();
                    let ok_parse = OkParse::new(left.clone(), nh);
                    let left_str = format!("{}", ok_parse);
                    let ok_parse = OkParse::new(right.clone(), ok_parse.disassemble().1);
                    let right_str = format!("{}", ok_parse);
                    //println!("l:{}  r:{}", left_str, right_str);
                    assert_eq!(left_str, right_str, "after subst they must eq");
                }
                _ => panic!("never here, it 100% binary op"),
            }
        };
        println!("");
    }

    #[test]
    fn check_mcu(){
        get_mcu("Q()", "P()", false);
        get_mcu("Q(x)", "P(a)", false);
        get_mcu("Q(f(x, x))", "P(f(x, x))", false);

        get_mcu("P(f(x, x))", "P(f(x, x))", true);
        get_mcu("P(x, x)", "P(x, x)", true);
        get_mcu("P(x, x)", "P(a, b)", false);
        
        get_mcu("P(a)", "P(b)", false);
        get_mcu("P(a)", "P(z)", true);
        get_mcu("P(f(a, b))", "P(f(z, b))", true);
        get_mcu("P(f(a, b))", "P(f(z, a))", false);
        get_mcu("P(f(a, b))", "P(f(z, x))", true);
        get_mcu("P(f(a, x))", "P(f(z, x))", true);
        get_mcu("P(f(z, x))", "P(f(z, x))", true);
        get_mcu("P(f(z, x), y)", "P(f(z, x), x)", true);
        get_mcu("P(f(z, x), x)", "P(f(z, x), y)", true);
        get_mcu("P(f(z, x), x)", "P(f(z, x), f(y, g(x)))", false);
        get_mcu("P(f(z, x), g(x, a))", "P(f(z, x), f(y, g(x, a)))", false);

        get_mcu("P(f(z, x), f(w, z))", "P(f(z, x), f(y, g(x, a)))", true);
        
        get_mcu("Q(x, f(x))", "Q(z, z)", false);
        get_mcu("Q(x, f(x))", "Q(a, z)", true);
        get_mcu("Q(z, z)", "Q(a, z)", true);

        get_mcu("R(c, x, f(x))", "R(c, y, y)", false);
        get_mcu("R(f(x, y), z, h(z, y))", "R(f(y, x), g(y), w)", true);
        get_mcu("R(z, f(x, b, z))", "R(h(x), f(g(a), y, z))", true);
        get_mcu("R(x, f(y), h(z, x))", "R(f(y), x, h(f(y), f(z)))", false);
        get_mcu("R(x_1, x_2, x_3, x_4)", "R(f(c, c), f(x_1, x_1), f(x_2, x_2), f(x_3, x_3))", true);

        get_mcu("R(f(x_2, y_2), g(y_2), w_2)", "R(f(x_1, y_1), z_1, h(z_1, y_1))", true);
    }

}
