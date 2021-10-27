use std::{collections::{HashMap, HashSet}, hash::Hash};

use crate::logic::{all_symbs::{AllSymbs, SYMBS_AMOUNT}, expr::Expr, operations::{BinaryOperations, Operations, UnaryOperations}, predicate_expr::PredicateExpr, quants::Quants, syntax_symbs::SyntaxSymbs, term_type::TermType, terms::Term};

use super::{name::Name, name_holder::NameHolder};
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
    // TODO: symb priority
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

pub fn parse<N:Name, T: PpeTesteable + Hash + Eq, I: Iterator<Item = T>>
(parser_rs: &ParserRuleset<T>, tokens: &mut I) 
//-> Option<Expr<N>>{
    {
    let mut name_holder = NameHolder::<N, T>::new();
    _parse(parser_rs, tokens, ParserParam::new(& mut name_holder));
}

struct ParserParam<'a, N:Name, T:Hash + Eq>{
    pub lvl: usize,
    prev_token_type: Option<AllSymbs>,
    pub can_be_next: HashSet<AllSymbs>,
    pub name_holder: &'a mut NameHolder<N, T>
}

impl<'a, N:Name, T:Hash + Eq> ParserParam<'a, N, T>{


    fn get_can_start() -> HashSet<AllSymbs>{
        let mut can_be_next = HashSet::new();

        can_be_next.insert(AllSymbs::Op(Operations::Unary(UnaryOperations::Not)));
        can_be_next.insert(AllSymbs::Quant(Quants::All));
        can_be_next.insert(AllSymbs::Quant(Quants::Exist));
        can_be_next.insert(AllSymbs::Term(TermType::Pred));
        can_be_next.insert(AllSymbs::Syntax(SyntaxSymbs::OpenBr));
        
        can_be_next
    }
    fn get_can_binary() -> HashSet<AllSymbs>{
        let mut can_be_next = HashSet::new();
        can_be_next.insert(AllSymbs::Op(Operations::Binary(BinaryOperations::Impl)));
        can_be_next.insert(AllSymbs::Op(Operations::Binary(BinaryOperations::And)));
        can_be_next.insert(AllSymbs::Op(Operations::Binary(BinaryOperations::Or)));
        return  can_be_next;
    }
    fn get_can_comma_or_close() -> HashSet<AllSymbs>{
        let mut can_be_next = HashSet::new();
        can_be_next.insert(AllSymbs::Syntax(SyntaxSymbs::CloseBr));
        can_be_next.insert(AllSymbs::Syntax(SyntaxSymbs::Comma));
        can_be_next
    }
    fn get_can_term() -> HashSet<AllSymbs>{
        let mut can_be_next = HashSet::new();
        can_be_next.insert(AllSymbs::Term(TermType::Var));
        can_be_next.insert(AllSymbs::Term(TermType::Func));
        can_be_next.insert(AllSymbs::Term(TermType::Const));
        can_be_next
    }
    fn get_can_symb(symb: AllSymbs) -> HashSet<AllSymbs>{
        let mut can_be_next = HashSet::new();
        can_be_next.insert(symb);
        can_be_next
    }

    pub fn set_can_term(&mut self){ self.can_be_next = Self::get_can_term(); }
    pub fn set_can_binary(&mut self){ self.can_be_next = Self::get_can_binary(); }
    pub fn set_can_binary_or_close(&mut self){
        self.set_can_binary();
        self.can_be_next.insert(AllSymbs::Syntax(SyntaxSymbs::CloseBr)); 
    }
    pub fn set_can_comma_or_close(&mut self){ self.can_be_next = Self::get_can_comma_or_close(); }

    fn new_with_lvl(lvl: usize, prev_token_type: Option<AllSymbs>, can_be_next: HashSet<AllSymbs>, name_holder: &'a mut NameHolder<N, T>) -> Self{
        Self{
            lvl,
            prev_token_type,
            can_be_next,
            name_holder
        }
    }

    pub fn new(name_holder: &'a mut NameHolder<N, T>) -> Self
    { Self::new_with_lvl(0, None, Self::get_can_start(), name_holder) }

    pub fn new_after_quant<'b>(&'b mut self, quant: Quants) -> ParserParam<'b, N, T> where 'a: 'b{
        self.symb_await(AllSymbs::Quant(quant), AllSymbs::Term(TermType::Var))
    }

    pub fn new_after_pred_or_func<'b>(&'b mut self, cur_token: AllSymbs, first: bool) -> ParserParam<'b, N, T> where 'a: 'b{
        let mut can_be_next = 
            if first { Self::get_can_term() } 
            else { Self::get_can_symb(AllSymbs::Syntax(SyntaxSymbs::Comma)) };
        can_be_next.insert(AllSymbs::Syntax(SyntaxSymbs::CloseBr));
        ParserParam::<'b, N, T>::new_with_lvl(self.lvl + 1, Some(cur_token), can_be_next, self.name_holder)
    }
    
    pub fn next<'b>(&'b mut self, cur_token: AllSymbs) -> ParserParam<'b, N, T> where 'a: 'b
    { ParserParam::<'b, N, T>::new_with_lvl(self.lvl + 1, Some(cur_token), Self::get_can_start(), self.name_holder) }

    pub fn symb_await<'b>(&'b mut self, cur_token: AllSymbs, symb_await: AllSymbs) ->  ParserParam<'b, N, T> where 'a: 'b{
        let mut can_be_next = HashSet::new();
        can_be_next.insert(symb_await);
        ParserParam::<'b, N, T>{ 
            lvl: self.lvl + 1,
            prev_token_type: Some(cur_token), 
            can_be_next,
            name_holder: self.name_holder
         }
    }
}

enum ParserRet<N:Name>{
    Expr(Expr<N>),
    Term(Term<N>),
    Name(N),
    Bad
}

impl<N:Name> ParserRet<N>{
    pub fn new_expr(expr: Expr<N>) -> Self { Self::Expr(expr) }
    pub fn new_term(term: Term<N>) -> Self { Self::Term(term) }
    pub fn new_name(name: N) -> Self { Self::Name(name) }
    pub fn new_bad() -> Self { Self::Bad }

    pub fn is_name(&self) -> bool { if let ParserRet::Name(_) = self { true } else { false }}
    pub fn is_expr(&self) -> bool { if let ParserRet::Expr(_) = self { true } else { false }}
    pub fn is_term(&self) -> bool { if let ParserRet::Term(_) = self { true } else { false }}
    pub fn is_bad(&self) -> bool { if let ParserRet::Bad = self { true } else { false }}

    pub fn get_expr(self) -> Expr<N> { if let Self::Expr(expr) = self { expr } else { panic!("not expr") } }
    pub fn get_term(self) -> Term<N> { if let Self::Term(term) = self { term } else { panic!("not term") } }
    pub fn get_name(self) -> N { if let Self::Name(name) = self { name } else { panic!("not name") } }

    pub fn if_expr<F>(self, f: F) -> Self 
    where F: FnOnce(Expr<N>) -> Expr<N>{
        if self.is_expr() { Self::new_expr(f(self.get_expr())) } 
        else { Self::new_bad() }
    }
    pub fn if_expr_then_pret<F>(self, f: F) -> Self 
    where F: FnOnce(Expr<N>) -> Self{
        if self.is_expr() { f(self.get_expr()) } 
        else { Self::new_bad() }
    }

    pub fn if_name_then_pret<F>(self, f: F) -> Self
    where F: FnOnce(N) -> Self{
        if self.is_name() { f(self.get_name()) }
        else { Self::new_bad() }
    }
} 


fn _parse<N:Name, T: PpeTesteable + Eq + Hash, I: Iterator<Item = T>> 
(parser_rs: &ParserRuleset<T>, tokens: &mut I, mut pp: ParserParam<N, T>) 
-> ParserRet<N>{    
    //TODO: only tail-rec or without rec

    let mut ret_expr = Expr::Empty;

    loop{
        let token = tokens.next();
        if token.is_none(){
            if pp.lvl != 0 { 
                //TODO:ERR:OUT: unexpected ending
                return ParserRet::new_bad()
            } 
            return ParserRet::new_expr(ret_expr)
        }
        let token = token.unwrap();

        let token_type = parser_rs.get_type(&token);  
        if token_type.is_none() {
            //TODO:ERR:OUT: wrong token
            return ParserRet::new_bad()
        }
        let token_type = token_type.unwrap();
        if token_type.is_empty() { continue }
        if !pp.can_be_next.contains(&token_type) {
            //TODO:ERR:OUT: wrong token
            return ParserRet::new_bad()
        }

        return match token_type {
            AllSymbs::Op(Operations::Unary(uop)) => 
                _parse(parser_rs, tokens, pp.next(token_type)).if_expr(|x|x.apply_unary_op(uop)),
            AllSymbs::Quant(qua) => 
                _parse(parser_rs, tokens, pp.new_after_quant(qua)).if_name_then_pret(
                    |x|_parse(parser_rs, tokens, pp.next(token_type)).if_expr(
                        |y|y.apply_quant(qua, x)
                    )
                ),
            AllSymbs::Term(term_type) => {
                let obj_name = pp.name_holder.get_name(term_type, token);
                match term_type{
                    TermType::Var | TermType::Const => ParserRet::new_name(obj_name),
                    _ => {
                        // after Func/Pred name stay always '(' :
                        let open_br = _parse(
                            parser_rs, 
                            tokens, 
                            pp.symb_await(token_type, AllSymbs::Syntax(SyntaxSymbs::OpenBr))
                        );
                        if !(open_br.is_expr() && open_br.get_expr().is_empty()) { return ParserRet::new_bad() }
                      
                        let mut first = true;
                        let mut terms = Vec::new();  
                        loop{
                            let name = _parse(parser_rs, tokens, pp.new_after_pred_or_func(token_type, first));
                            if name.is_bad() { return name }
                            if name.is_name() { 
                                let name = name.get_name();
        
                                if name.name_type() == TermType::Const {
                                    terms.push(pp.name_holder.get_const_term(&name))
                                } else if name.name_type() == TermType::Var {
                                    terms.push(pp.name_holder.get_var_term(&name))
                                } else {
                                    panic!("we cant get that type of name!")
                                }
                            } else if name.is_term() { 
                                terms.push(name.get_term()) 
                            } else if name.is_expr(){ // empty expr when ')'
                                 let expr = name.get_expr();
                                 if !expr.is_empty() { panic!("we cant take such type of expr!") }
                                 if term_type == TermType::Func{
                                    return ParserRet::new_term(Term::new_func_by_param(obj_name, terms))
                                 } else {
                                    return ParserRet::new_expr(Expr::new_predicate(obj_name, terms))
                                    //here must be contniue?
                                 }
                            }
                            first = false;
                        }
                    }
                }
            }
            
            AllSymbs::Syntax(SyntaxSymbs::Comma) => {
                pp.set_can_term();
                continue
            } 
            AllSymbs::Syntax(SyntaxSymbs::CloseBr) => ParserRet::new_expr(ret_expr),
            AllSymbs::Syntax(SyntaxSymbs::OpenBr) => {
                if let Some(AllSymbs::Term(_)) = pp.prev_token_type{
                    return ParserRet::new_expr(ret_expr)
                } else {
                    let ret_expr_temp = _parse(parser_rs, tokens, pp.next(token_type));
                    if !ret_expr_temp.is_expr() { return ParserRet::new_bad() }
                    
                    ret_expr = ret_expr_temp.get_expr();
                    if ret_expr.is_empty() { return ParserRet::new_bad() }
                    
                    pp.set_can_binary_or_close();
                    continue;
                }
            }
            AllSymbs::Op(Operations::Binary(bop)) => 
                if ret_expr.is_empty() { 
                    ParserRet::new_bad() 
                }
                else {
                    _parse(parser_rs, tokens, pp.next(token_type)).if_expr_then_pret(
                        |left|_parse(parser_rs, tokens, pp.next(token_type)).if_expr(
                            |right|Expr::apply_binary_op(bop, left, right)
                        )
                    )
                }
            
            _ => panic!("cant be here"),
        }


    }
}