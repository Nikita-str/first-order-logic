use crate::logic::{all_symbs::AllSymbs, operations::{BinaryOperations, Operations, UnaryOperations}, quants::Quants, syntax_symbs::SyntaxSymbs, term_type::TermType, terms::Term};

use super::parse::{ExactTesteable, ParseRuleType, ParserRuleset, PostfixTesteable, PrefixTesteable};


#[derive(PartialEq, Eq, Hash, Debug)]
pub struct ParseStr<'a>{ 
    s: &'a str 
}

impl<'a> ParseStr<'a>{
    pub fn new(s: &'a str) -> Self { Self{ s } }
    fn get_len_while<F>(&self, f: F) -> usize
    where F: Fn(char) -> bool{
        let mut sh = 0;
        self.s.chars().any(|x| if f(x) { sh = sh + x.len_utf8(); false } else { true });
        sh
    }

    pub fn create_std_ruleset() -> ParserRuleset<ParseStr<'a>>{
        let mut ruleset = ParserRuleset::new();

        ruleset.add_rule(AllSymbs::Empty, ParseRuleType::Prefix, ParseStr::new(" "));

        let mut add_rules = |term_type, vec:Vec<&'a str>|{
            for v in vec{
                ruleset.add_rule(AllSymbs::Term(term_type), ParseRuleType::Exact, ParseStr::new(v.trim_end_matches('_')));
                ruleset.add_rule(AllSymbs::Term(term_type), ParseRuleType::Prefix, ParseStr::new(v));
            }
        };

        let const_vec = vec!["a_", "b_", "c_"];
        add_rules(TermType::Const, const_vec);
        
        let var_vec = vec!["X_", "x_", "Y_", "y_", "Z_", "z_", "W_", "w_"];
        add_rules(TermType::Var, var_vec);

        let func_vec = vec!["f", "g", "h"];
        add_rules(TermType::Func, func_vec);

        let pred_vec = vec!["P", "R", "Q"];
        add_rules(TermType::Pred, pred_vec);

        let mut add_exact_rules = |symb_type, vec: Vec<&'a str>|{
            for v in vec{
                ruleset.add_rule(symb_type, ParseRuleType::Exact, ParseStr::new(v));
            }
        };

        add_exact_rules(AllSymbs::Quant(Quants::Exist), vec!["exist", "∃"]);
        add_exact_rules(AllSymbs::Quant(Quants::All), vec!["for_all", "all", "for_any", "any", "∀"]);

        add_exact_rules(AllSymbs::Syntax(SyntaxSymbs::Comma), vec![","]);
        add_exact_rules(AllSymbs::Syntax(SyntaxSymbs::OpenBr), vec!["("]);
        add_exact_rules(AllSymbs::Syntax(SyntaxSymbs::CloseBr), vec![")"]);

        add_exact_rules(AllSymbs::Op(Operations::Unary(UnaryOperations::Not)), vec!["!", "¬"]);

        add_exact_rules(AllSymbs::Op(Operations::Binary(BinaryOperations::Impl)), vec!["→", "->", "-->", "--->"]);
        add_exact_rules(AllSymbs::Op(Operations::Binary(BinaryOperations::And)), vec!["∧", "&", "&&"]);
        add_exact_rules(AllSymbs::Op(Operations::Binary(BinaryOperations::Or)), vec!["∨", "|", "||"]);

        ruleset
    }
}

const ONE_CHARS: &'static str = "(,)∀∃¬∨∧→";
const MULTI_CHARS: &'static str = "|&->!";

impl<'a> Iterator for ParseStr<'a>{
    type Item = Self;
    fn next(&mut self) -> Option<Self::Item>{
        let c = self.s.chars().next();

        // TODO:LAZY: [only if write normal ~full parser]
        // may create ~ruleset for check next term(token-splitter sounds well)
        //          but i'm lazy to do this (and add it as ref into ParserStr)
        match c {
            None => None,
            Some(c) => {
                let sh = if c.is_whitespace() {
                    self.get_len_while(|x|x.is_whitespace())
                } else if ONE_CHARS.contains(c) {
                    c.len_utf8()
                } else if c.is_ascii_digit() {
                    self.get_len_while(|x|x.is_ascii_digit())
                } else if c.is_ascii_alphabetic() {
                    self.get_len_while(|x|x.is_ascii_alphanumeric() || x == '_')
                } else if MULTI_CHARS.contains(c){
                    self.get_len_while(|x|MULTI_CHARS.contains(x))
                } else {
                    self.get_len_while(|x|!(
                        x.is_whitespace() || 
                        x.is_ascii_alphanumeric() ||
                        MULTI_CHARS.contains(x) || 
                        ONE_CHARS.contains(x) || 
                        x == '_'
                    ))
                };
                
                let left = self.s.get(0..sh).unwrap();
                let right = self.s.get(sh..).unwrap();

                *self = Self{ s: right};
                Some(Self { s: left })
            }
        }
    }
}


impl<'a> PrefixTesteable for ParseStr<'a>{
    fn test_prefix(&self, other: &Self) -> bool { self.s.test_prefix(other.s) }
}

impl<'a> PostfixTesteable for ParseStr<'a>{
    fn test_postfix(&self, other: &Self) -> bool { self.s.test_postfix(other.s) }
}

impl<'a> ExactTesteable for ParseStr<'a>{
    fn test_exact(&self, other: &Self) -> bool { self.s.test_exact(other.s) }
}

#[cfg(test)]
mod parse_str_test{
    use crate::common::{name::StdName, parse};
    use super::ParseStr;

    fn test_help(ps: ParseStr, expects: Vec<&str>){
        let mut iter = ps.into_iter(); 
        for expect in expects{
            assert_eq!(iter.next().unwrap().s, expect)
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
}