
#[cfg(test)]
mod parse_str_test{
    use crate::common::{name::StdName, parse, ok_parse::OkParse};
    use crate::common::parse_str::ParseStr;
    use crate::logic::expr::Expr;

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
        parse_wrong_test_help("for_any x for_any y exist x P(x, y, x)");
        parse_wrong_test_help("for_any x for_any y for_any x P(x, y, x)");

        parse_ok_test_help("for_any x P(x, b)");
        parse_wrong_test_help("for_any a P(a, b)");

        parse_wrong_test_help("exist x exist x P(x, x)");

        parse_ok_test_help("exist x P(x) --> exist x P(x)");
        parse_ok_test_help("exist x P(a, x) --> exist x P(x, b)");
        parse_wrong_test_help("exist x P(a, x) --> exist x any x P(x, x)");
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
        //let ps = ParseStr::new("¬ ∃x( (P(x) & (∀x_2 P(x_2) → ∃y R(x, y))) → ∃y R(x, y) )");
        let ps = ParseStr::new("(exist x_1 any y_1 exist z_1 P(x_1,y_1, z_1)) & (any x_2 exist y_2 R(x_2, y_2) | exist x_3 any y_3 R(x_3, y_3))");
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

}
