
#[cfg(test)]
mod parse_str_test{
    use crate::common::{name::StdName, parse, ok_parse::OkParse};
    use crate::common::parse_str::ParseStr;

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
    fn parse_test_2(){
        let ruleset = ParseStr::create_std_ruleset();

        let ps = ParseStr::new("exist x Q(x) & (P(x) -> exist x R(x, x))"); // ERROR: must add for_all for second x ! => 3 vars
        let expr = parse::parse::<StdName, _, _>(&ruleset, &mut ps.into_iter());

        match expr {
            Err(_) => println!("NONE :("),
            Ok(ok) => {
                println!("EXPR : {:?}", ok.get_expr())
            }
        }
    }
    
    #[test]
    fn parse_test_3(){
        let ruleset = ParseStr::create_std_ruleset();

        //let ps = ParseStr::new("! exist x P(x) --> for_all x !P(x) & R(y)"); 
        //let ps = ParseStr::new("P(x, y) & exist x (R(x) & P(y, x)) & R(y) & R(x)"); // free vars: x_0, y_0, x_3 => 1, 2, 4 
        //let ps = ParseStr::new("for_all x (P(x, y) & R(x, y)) ---> (Q(x) & (Q(y) | Q(x))) "); 
        
        let ps = ParseStr::new("P(x) & exist x P(x) & P(x)"); // HERE ONLY 2 VARS!!!
        let expr = parse::parse::<StdName, _, _>(&ruleset, &mut ps.into_iter());
        
        match expr {
            Err(_) => println!("NONE :("),
            Ok(ok) => {
                print!("free vars: ");
                ok.display_free_vars();
                println!("EXPR : {}", ok)
            }
        }
    }
}
