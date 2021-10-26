use std::{collections::{HashMap, HashSet}};

use crate::logic::{all_symbs::{AllSymbs, SYMBS_AMOUNT}, operations::{BinaryOperations, Operations, UnaryOperations}, quants::Quants, term_type::TermType};
use super::parse_info::Fit;


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
        let map = HashMap::new();
        map.insert(ParseRuleType::Exact, HashSet::new());
        map.insert(ParseRuleType::Prefix, HashSet::new());
        map.insert(ParseRuleType::Postfix, HashSet::new());
        assert_eq!(map.len(), PARSE_RULE_TYPE_LEN);
        Self{ ruleset: map }
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

pub struct TokenParse<T>{
    empty_token: ParseRuleset<T>,
    symbs_token: HashMap<AllSymbs, ParseRuleset<T>>
}

impl<T> TokenParse<T>{
    pub fn new() -> Self{
        let empty_token = ParseRuleset::new();
        let mut symbs_token = HashMap::new();

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

        assert_eq!(symbs_token.len(), SYMBS_AMOUNT);

        Self { empty_token, symbs_token }
    }
}

pub struct ParseStr<'s>{
    s: &'s str 
}

impl<'s> Fit<AllSymbs> for ParseStr<'s> {
    fn get_type(&self) -> Option<AllSymbs>{

    }
} 