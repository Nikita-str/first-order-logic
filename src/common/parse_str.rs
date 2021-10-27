use std::{collections::{HashMap, HashSet}, hash::Hash};

use crate::logic::{all_symbs::{AllSymbs, SYMBS_AMOUNT}, expr::Expr, operations::{BinaryOperations, Operations, UnaryOperations}, quants::Quants, syntax_symbs::SyntaxSymbs, term_type::TermType};

use super::name::Name;
//use super::parse_info::Fit;


#[derive(PartialEq, Eq, Hash)]
pub enum ParseRuleType{
    Prefix,
    Postfix,
    Exact,
}
const PARSE_RULE_TYPE_LEN: usize = 3;

pub struct ParseRuleset<T>{
    ruleset: HashMap<ParseRuleType, HashSet<T>>
}
impl<T> ParseRuleset<T>{
    pub fn new() -> Self{
        let mut map = HashMap::new();
        map.insert(ParseRuleType::Exact, HashSet::new());
        map.insert(ParseRuleType::Prefix, HashSet::new());
        map.insert(ParseRuleType::Postfix, HashSet::new());
        assert_eq!(map.len(), PARSE_RULE_TYPE_LEN);
        Self{ ruleset: map }
    }
}

impl<T: Eq + Hash> ParseRuleset<T>{
    pub fn add_rule(&mut self, rtype: ParseRuleType, value: T){
        self.ruleset.get_mut(&rtype).unwrap().insert(value);
    }
}

impl<T: PpeTesteable> ParseRuleset<T>{
    fn is_fit(&self, test: &T) -> bool {
        let res = self.ruleset.get(&ParseRuleType::Exact).unwrap().into_iter().any(|x|x.test_exact(test));
        let res = res || self.ruleset.get(&ParseRuleType::Prefix).unwrap().into_iter().any(|x|x.test_prefix(test));
        let res = res || self.ruleset.get(&ParseRuleType::Postfix).unwrap().into_iter().any(|x|x.test_postfix(test));
        res
    }
}

//TODO: other: &O where O is generic type
pub trait PrefixTesteable{ fn test_prefix(&self, other: &Self) -> bool; }
pub trait PostfixTesteable{ fn test_postfix(&self, other: &Self) -> bool; }
pub trait ExactTesteable{ fn test_exact(&self, other: &Self) -> bool; }
pub trait PpeTesteable: PrefixTesteable + PostfixTesteable + ExactTesteable {}
impl<X> PpeTesteable for X where X: PrefixTesteable + PostfixTesteable + ExactTesteable {}

impl PrefixTesteable for str { fn test_prefix(&self, other: &Self) -> bool { self.starts_with(other) } }
impl PostfixTesteable for str { fn test_postfix(&self, other: &Self) -> bool { self.ends_with(other) } }
impl ExactTesteable for str { fn test_exact(&self, other: &Self) -> bool { self.eq(other) } }

pub struct ParserRuleset<T>{
    token_rulesets: HashMap<AllSymbs, ParseRuleset<T>>
}

impl<T> ParserRuleset<T>{
    pub fn new() -> Self{
        let mut symbs_token = HashMap::new();

        symbs_token.insert(AllSymbs::Empty, ParseRuleset::new());

        symbs_token.insert(AllSymbs::Quant(Quants::All), ParseRuleset::new());
        symbs_token.insert(AllSymbs::Quant(Quants::Exist), ParseRuleset::new());

        symbs_token.insert(AllSymbs::Term(TermType::Var), ParseRuleset::new());
        symbs_token.insert(AllSymbs::Term(TermType::Pred), ParseRuleset::new());
        symbs_token.insert(AllSymbs::Term(TermType::Func), ParseRuleset::new());
        symbs_token.insert(AllSymbs::Term(TermType::Const), ParseRuleset::new());

        symbs_token.insert(AllSymbs::Op(Operations::Unary(UnaryOperations::Not)), ParseRuleset::new());
        symbs_token.insert(AllSymbs::Op(Operations::Binary(BinaryOperations::Or)), ParseRuleset::new());
        symbs_token.insert(AllSymbs::Op(Operations::Binary(BinaryOperations::And)), ParseRuleset::new());
        symbs_token.insert(AllSymbs::Op(Operations::Binary(BinaryOperations::Impl)), ParseRuleset::new());

        symbs_token.insert(AllSymbs::Syntax(SyntaxSymbs::Comma), ParseRuleset::new());
        symbs_token.insert(AllSymbs::Syntax(SyntaxSymbs::OpenBr), ParseRuleset::new());
        symbs_token.insert(AllSymbs::Syntax(SyntaxSymbs::CloseBr), ParseRuleset::new());

        assert_eq!(symbs_token.len(), SYMBS_AMOUNT);

        Self { token_rulesets: symbs_token }
    }
}

impl<T: Eq + Hash> ParserRuleset<T>{
    pub fn add_rule(&mut self, symbs: AllSymbs, rtype: ParseRuleType, value: T) { 
        self.token_rulesets.get_mut(&symbs).unwrap().add_rule(rtype, value) 
    }
}

impl<T: PpeTesteable> ParserRuleset<T>{
    pub fn get_type(&self, token: &T) -> Option<AllSymbs>{
        for (key, ruleset) in &self.token_rulesets{
            if ruleset.is_fit(token) { return Some(*key) }
        }
        None
    }
}

/*
pub struct Parseable<'a, T>{
    token_parse: &'a TokenParse<T>,
    parse_obj: Iterator<T>,
}

pub struct ParseStr<'s>{
    s: &'s str 
}
*/

pub fn parse<N:Name, T: PpeTesteable, I: Iterator<Item = T>>(parser_rs: &ParserRuleset<T>, tokens: &mut I) -> Option<Expr<N>>{
    _parse(parser_rs, tokens, ParserParam::new())
}

struct ParserParam{
    pub lvl: usize,
    //prev_token_type: Option<AllSymbs>,
    pub can_be_next: HashSet<AllSymbs>,
    pub need_parse_len: Option<usize>
}
impl ParserParam{
    fn new_with_lvl(lvl: usize) -> Self{
        let mut can_be_next = HashSet::new();

        can_be_next.insert(AllSymbs::Op(Operations::Unary(UnaryOperations::Not)));
        can_be_next.insert(AllSymbs::Quant(Quants::All));
        can_be_next.insert(AllSymbs::Quant(Quants::Exist));
        can_be_next.insert(AllSymbs::Term(TermType::Pred));
        can_be_next.insert(AllSymbs::Syntax(SyntaxSymbs::OpenBr));

        Self{
            lvl,
            //prev_token_type: None,
            can_be_next,
            need_parse_len: None,
        }
    }

    pub fn new() -> Self{ Self::new_with_lvl(0) }

    pub fn new_after_quant(&self) -> Self{
        let mut can_be_next = HashSet::new();
        can_be_next.insert(AllSymbs::Term(TermType::Var));

        Self{
            lvl: self.lvl + 1,
            //prev_token_type: 
            can_be_next,
            need_parse_len: Some(1)
        }
    }

    pub fn next(&self) -> Self { Self::new_with_lvl(self.lvl + 1) }
}

fn _parse<N:Name, T: PpeTesteable, I: Iterator<Item = T>> 
(parser_rs: &ParserRuleset<T>, tokens: &mut I, pp: ParserParam) 
-> Option<Expr<N>>{    
    //TODO: only tail-rec or without rec

    loop{
        let token = tokens.next();
        if token.is_none(){
            if pp.lvl != 0 { 
                //TODO:ERR:OUT: unexpected ending
                return None
            } else {
                return Some(Expr::Empty)
            }
        }
        let token = token.unwrap();

        let token_type = parser_rs.get_type(&token);  
        if token_type.is_none() {
            //TODO:ERR:OUT: wrong token
            return None
        }
        let token_type = token_type.unwrap();
        if token_type.is_empty() { continue }
        if !pp.can_be_next.contains(&token_type) {
            //TODO:ERR:OUT: wrong token
            return None
        }

        return match token_type {
            AllSymbs::Op(Operations::Unary(uop)) => 
                _parse(parser_rs, tokens, pp.next()).and_then(|x|Some(x.apply_unary_op(uop))),
            AllSymbs::Quant(qua) => {
                
            }
            _ => panic!("cant be here"),
        }


    }
}