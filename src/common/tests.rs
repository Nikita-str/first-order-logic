
#[cfg(test)]
mod parse_str_test{
    use crate::common::{name::StdName, parse::{self}};
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
        let expr = parse::parse::<StdName, _, _>(&ruleset, &mut ps.into_iter());
        assert!(expr.is_none());
    }

    fn parse_ok_test_help(parse_str: &str){
        let ruleset = ParseStr::create_std_ruleset();
        let ps = ParseStr::new(parse_str);
        let expr = parse::parse::<StdName, _, _>(&ruleset, &mut ps.into_iter());
        assert!(expr.is_some());
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

        //TODO : wrong : exist x exist !![x]!!  P(x, x)
        //TODO : wrong : P(x) & P(x, x)
    }

    #[test]
    fn parse_test(){
        let ruleset = ParseStr::create_std_ruleset();

        let ps = ParseStr::new("P(f(a, b), g(b, h(c)))"); // +
        let expr = parse::parse::<StdName, _, _>(&ruleset, &mut ps.into_iter());

        match expr {
            None => println!("NONE :("),
            Some(expr) => {
                println!("EXPR : {:?}", expr)
            }
        }

        println!("\n\n\n");

        let ps = ParseStr::new("∃x∀y P(x,y)   →∀y_i∃x    P(x,  y_i)"); // +
        let expr = parse::parse::<StdName, _, _>(&ruleset, &mut ps.into_iter());

        match expr {
            None => println!("NONE :("),
            Some(expr) => {
                println!("EXPR : {:?}", expr)
            }
        }

        println!("\n\n\n");

        let ps = ParseStr::new("P(a,b)&P(b,c)&P(c,a)"); // +
        let expr = parse::parse::<StdName, _, _>(&ruleset, &mut ps.into_iter());

        match expr {
            None => println!("NONE :("),
            Some(expr) => {
                println!("EXPR : {:?}", expr)
            }
        }

        println!("\n\n\n");

        // must : And(Or(.., And(...)), ...)
        let ps = ParseStr::new("(P(a,b)|P(b,c)&P(c,a))&P(a,b)"); 
        let expr = parse::parse::<StdName, _, _>(&ruleset, &mut ps.into_iter());

        match expr {
            None => println!("NONE :("),
            Some(expr) => {
                println!("EXPR : {:?}", expr)
            }
        }

        println!("\n\n\n");

        // must : Or(And(..), And(..))
        let ps = ParseStr::new("P(a,b)&P(b,c)|P(c,a)&P(a,b)"); 
        let expr = parse::parse::<StdName, _, _>(&ruleset, &mut ps.into_iter());

        match expr {
            None => println!("NONE :("),
            Some(expr) => {
                println!("EXPR : {:?}", expr)
            }
        }
    }

    #[test]
    fn parse_test_2(){
        let ruleset = ParseStr::create_std_ruleset();

        let ps = ParseStr::new("( P(x) & (P(y) | exist z P(z)) ) & P(y)    --->    (( P(x) & ( P(x) ) | Q(x) )) & P(x)"); 
        let expr = parse::parse::<StdName, _, _>(&ruleset, &mut ps.into_iter());

        match expr {
            None => println!("NONE :("),
            Some(expr) => {
                println!("EXPR : {:?}", expr)
            }
        }
    }
}
