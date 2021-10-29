use std::{collections::{HashMap, HashSet}, hash::Hash, convert::TryInto};

use crate::{common::ok_parse::OkParse, logic::{all_symbs::{AllSymbs, SYMBS_AMOUNT}, expr::Expr, operations::{BinaryOperations, Operations, UnaryOperations}, quants::Quants, syntax_symbs::SyntaxSymbs, term_type::TermType, terms::Term}};

use super::{name::Name, name_holder::{NameHolder}};
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
        let res = self.ruleset.get(&ParseRuleType::Exact).unwrap().into_iter().any(|x|test.test_exact(x));
        let res = res || self.ruleset.get(&ParseRuleType::Prefix).unwrap().into_iter().any(|x|test.test_prefix(x));
        let res = res || self.ruleset.get(&ParseRuleType::Postfix).unwrap().into_iter().any(|x|test.test_postfix(x));
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


pub fn parse<N:Name+std::fmt::Debug, T: PpeTesteable + Hash + Eq + std::fmt::Debug + Clone, I: Iterator<Item = T>>
(parser_rs: &ParserRuleset<T>, tokens: &mut I) 
-> Result<OkParse<N, T>, ParseError>{
    let mut name_holder = NameHolder::<N, T>::new();
    let mut tokens = Tokens::<_, _, 1>::new(tokens);
    let expr = _parse(parser_rs, &mut tokens, ParserParam::new(& mut name_holder));
    //println!("free vars: {:?}", name_holder.get_free_vars());
    //println!("init of renaming: {:?}", name_holder.get_init_of_renaming());
    match expr {
        ParserRet::Expr(expr) => Ok(OkParse::new(expr, name_holder)),
        ParserRet::Bad(err) => Err(err),
        _ => { 
            // TODO: INFORM ABOUT ERROR 
            Err(ParseError::ExpectedExpr) 
        }
    }
}

struct ParserParam<'a, N:Name, T:Hash + Eq>{
    pub lvl: usize,
    prev_token_type: Option<AllSymbs>,
    pub allow_back_save: HashSet<AllSymbs>,
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

    #[allow(dead_code)]
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

    pub fn clear_can(&mut self){ self.can_be_next = HashSet::new(); }
    pub fn set_can(&mut self, can: AllSymbs){ self.can_be_next.insert(can); }
    pub fn set_can_term(&mut self){ self.can_be_next = Self::get_can_term(); }
    pub fn set_can_binary(&mut self){ self.can_be_next = Self::get_can_binary(); }
    pub fn set_can_binary_or_close(&mut self){
        self.set_can_binary();
        self.can_be_next.insert(AllSymbs::Syntax(SyntaxSymbs::CloseBr)); 
    }
    #[allow(dead_code)]
    pub fn set_can_comma_or_close(&mut self){ self.can_be_next = Self::get_can_comma_or_close(); }

    fn new_with_lvl(lvl: usize, prev_token_type: Option<AllSymbs>, can_be_next: HashSet<AllSymbs>, name_holder: &'a mut NameHolder<N, T>) -> Self{
        Self{
            lvl,
            prev_token_type,
            allow_back_save: HashSet::new(),
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
            allow_back_save: HashSet::new(),
            can_be_next,
            name_holder: self.name_holder
         }
    }

    pub fn allow_save(&mut self, value: AllSymbs) { self.allow_back_save.insert(value); }
    pub fn disallow_save(&mut self, value: AllSymbs) { self.allow_back_save.remove(&value); }
}

#[derive(Debug)]
pub enum ParseError{
    UnexpectedToken,
    UnexpectedEnd,
    ExpectedExpr,
    ExpectedName,
    UnclassifiableToken,
    MoreThanOneQuantLimitation,
    ThereMustBe(AllSymbs),
    DifferentParamLen,
    EmptyExpr,
}

enum ParserRet<N:Name>{
    Expr(Expr<N>),
    Term(Term<N>),
    Name(N),
    Bad(ParseError),
}

impl<N:Name> ParserRet<N>{
    pub fn new_expr(expr: Expr<N>) -> Self { Self::Expr(expr) }
    pub fn new_term(term: Term<N>) -> Self { Self::Term(term) }
    pub fn new_name(name: N) -> Self { Self::Name(name) }
    pub fn new_bad(parse_error: ParseError) -> Self { Self::Bad(parse_error) }

    pub fn is_name(&self) -> bool { if let ParserRet::Name(_) = self { true } else { false }}
    pub fn is_expr(&self) -> bool { if let ParserRet::Expr(_) = self { true } else { false }}
    pub fn is_term(&self) -> bool { if let ParserRet::Term(_) = self { true } else { false }}
    pub fn is_bad(&self) -> bool { if let ParserRet::Bad(_) = self { true } else { false }}

    pub fn get_expr(self) -> Expr<N> { if let Self::Expr(expr) = self { expr } else { panic!("not expr") } }
    pub fn get_term(self) -> Term<N> { if let Self::Term(term) = self { term } else { panic!("not term") } }
    pub fn get_name(self) -> N { if let Self::Name(name) = self { name } else { panic!("not name") } }

    pub fn if_expr<F>(self, f: F) -> Self 
    where F: FnOnce(Expr<N>) -> Expr<N>{
        if self.is_expr() { Self::new_expr(f(self.get_expr())) } 
        else if self.is_bad() { self } 
        else { Self::new_bad(ParseError::ExpectedExpr) }
    }
    #[allow(dead_code)]
    pub fn if_expr_then_pret<F>(self, f: F) -> Self 
    where F: FnOnce(Expr<N>) -> Self{
        if self.is_expr() { f(self.get_expr()) } 
        else if self.is_bad() { self } 
        else { Self::new_bad(ParseError::ExpectedExpr) }
    }

    pub fn if_name_then_pret<F>(self, f: F) -> Self
    where F: FnOnce(N) -> Self{
        if self.is_name() { f(self.get_name()) }
        else if self.is_bad() { self } 
        else { Self::new_bad(ParseError::ExpectedName) }
    }
} 

struct Tokens<'a, T, I: Iterator<Item = T>, const SZ: usize>{
    tokens: &'a mut I,
    start_index: usize,
    cur_prev_saved: usize,
    previous_token: [Option<T>; SZ]
}
impl<'a, T, I: Iterator<Item = T>, const SZ: usize> Tokens<'a, T, I, SZ>{
    pub fn new(tokens: &'a mut I) -> Self {
        let mut vec = vec![];
        for _ in 0..SZ { vec.push(None); }
        let boxed_array: Box<[Option<T>; SZ]> = match vec.into_boxed_slice().try_into() {
            Ok(ba) => ba,
            Err(o) => panic!("Expected a Vec of length {} but it was {}", SZ, o.len()),
        };
        Self { tokens, start_index: 0, cur_prev_saved: 0, previous_token: *boxed_array }
    }

    pub fn save_prev_token(&mut self, prev: T){
        if self.cur_prev_saved == SZ { panic!("too many save!") }
        if self.cur_prev_saved == 0 { self.start_index = 0; }
        let ind = (self.start_index + self.cur_prev_saved) % SZ; 
        self.previous_token[ind] = Some(prev);
        self.cur_prev_saved = self.cur_prev_saved + 1;
    }
}
impl<'a, T, I: Iterator<Item = T>, const SZ: usize> Iterator for Tokens<'a, T, I, SZ>{
    type Item = T;
    fn next(&mut self) -> Option<Self::Item> {
        if self.cur_prev_saved > 0 {
            self.cur_prev_saved = self.cur_prev_saved - 1;
            let mut x = None;
            std::mem::swap(&mut x, &mut self.previous_token[self.start_index]);
            self.start_index = self.start_index + 1;
            if self.start_index == SZ { self.start_index = 0 }
            x
        } else {
            self.tokens.next()
        }
    }
}

#[cfg(test)]
mod tokens_test{
    use super::Tokens;
    #[test]
    fn create_tokens() {
        let iter = vec![1, 4, 8];
        let mut iter = iter.into_iter();
        let mut x = Tokens::<_, _, 2>::new(&mut iter);
        assert_eq!(x.next(), Some(1));
        assert_eq!(x.next(), Some(4));
        x.save_prev_token(8);
        assert_eq!(x.next(), Some(8));
        assert_eq!(x.next(), Some(8));
        assert_eq!(x.next(), None);
        assert_eq!(x.next(), None);
        x.save_prev_token(1);
        x.save_prev_token(3);
        assert_eq!(x.next(), Some(1));
        x.save_prev_token(5);
        assert_eq!(x.next(), Some(3));
        x.save_prev_token(7);
        assert_eq!(x.next(), Some(5));
        assert_eq!(x.next(), Some(7));
        assert_eq!(x.next(), None);
        
        let iter = vec![1, 2, 3];
        let mut iter = iter.into_iter();
        let mut x = Tokens::<_, _, 4>::new(&mut iter);
        x.save_prev_token(10);
        x.save_prev_token(11);
        x.save_prev_token(12); // 10 11 12 x
        assert_eq!(x.next(), Some(10)); // x 11 12 x
        x.save_prev_token(13); // x 11 12 13
        assert_eq!(x.next(), Some(11)); // xx xx 12 13
        x.save_prev_token(14); 
        x.save_prev_token(15); // 14 15 12 13
        assert_eq!(x.next(), Some(12)); 
        assert_eq!(x.next(), Some(13)); // 14 15 x x
        x.save_prev_token(16); // 14 15 16 x
        assert_eq!(x.next(), Some(14)); 
        assert_eq!(x.next(), Some(15));  // x x 16 x
        assert_eq!(x.next(), Some(16));  
        x.save_prev_token(17);  
        x.save_prev_token(18);  // 17 18 x x
        assert_eq!(x.next(), Some(17));  
        x.save_prev_token(19);  
        x.save_prev_token(20);  // x 18 19 20
        assert_eq!(x.next(), Some(18));  
        x.save_prev_token(21);  // 21 x 19 20
        assert_eq!(x.next(), Some(19));  
        assert_eq!(x.next(), Some(20));  
        assert_eq!(x.next(), Some(21));  
        assert_eq!(x.next(), Some(1));  
        assert_eq!(x.next(), Some(2));  
        assert_eq!(x.next(), Some(3));  
        assert_eq!(x.next(), None);  
    }
}


fn _parse<N:Name+std::fmt::Debug, T: PpeTesteable + Eq + Hash + Clone + std::fmt::Debug, I: Iterator<Item = T>> 
(parser_rs: &ParserRuleset<T>, tokens: &mut Tokens<T, I, 1>, mut pp: ParserParam<N, T>) 
-> ParserRet<N>{    
    //TODO: only tail-rec or without rec

    let try_take_binary = |pp: &mut ParserParam<_,_>|{
        let prev = pp.prev_token_type; 
        let prev_prior = if let Some(x) = prev { x.get_priority() } else { None };
        let prev_prior = prev_prior.unwrap_or(0);
        
        let op_vec = vec![BinaryOperations::And, BinaryOperations::Or, BinaryOperations::Impl]; 
        pp.clear_can();
        let mut can_any = false;
        for op in op_vec{
            let symb = AllSymbs::Op(Operations::Binary(op));
            if prev_prior <= symb.get_priority().unwrap() {
                pp.set_can(symb);
                can_any = true;
            } else {
                pp.allow_save(symb);
            }
        }
        can_any
    };

    let mut ret_expr = Expr::Empty;
    if pp.lvl == 0 { pp.allow_save(AllSymbs::End) }

    'main_loop: loop{
        let token = tokens.next();
        
        if token.is_none(){
            if pp.allow_back_save.contains(&AllSymbs::End) {
                return ParserRet::new_expr(ret_expr)
            }
            return ParserRet::new_bad(ParseError::UnexpectedEnd)
        }
        let token = token.unwrap();

        let token_type = parser_rs.get_type(&token);  

        if token_type.is_none() {
            return ParserRet::new_bad(ParseError::UnclassifiableToken)
        }
        let token_type = token_type.unwrap();
        if token_type.is_empty() { continue }
        if !pp.can_be_next.contains(&token_type) {
            if pp.lvl != 0 && pp.allow_back_save.contains(&token_type){
                tokens.save_prev_token(token);
                return ParserRet::new_expr(ret_expr)
            }
            return ParserRet::new_bad(ParseError::UnexpectedToken)
        }

        return match token_type {
            AllSymbs::Op(Operations::Unary(uop)) => {
                    let expr = _parse(parser_rs, tokens, pp.next(token_type)).if_expr(|x|x.apply_unary_op(uop));
                    if !expr.is_expr() { return expr }
                    ret_expr = expr.get_expr();
                    
                    if  try_take_binary(&mut pp) {
                        pp.allow_save(AllSymbs::Syntax(SyntaxSymbs::CloseBr));
                        pp.allow_save(AllSymbs::End);
                        continue 'main_loop
                    } else {
                        return ParserRet::new_expr(ret_expr)
                    }
                }
            AllSymbs::Quant(qua) => {
                    let expr = _parse(parser_rs, tokens, pp.new_after_quant(qua)).if_name_then_pret(
                        |x|{
                            let r = _parse(parser_rs, tokens, pp.next(token_type)).if_expr(
                                |y|y.apply_quant(qua, x.clone())
                            );
                            pp.name_holder.remove_last_var_restr(&x);
                            r
                        }
                    );
                    if !expr.is_expr() { return expr }
                    ret_expr = expr.get_expr();

                    if  try_take_binary(&mut pp) {
                        pp.allow_save(AllSymbs::Syntax(SyntaxSymbs::CloseBr));
                        pp.allow_save(AllSymbs::End);
                        continue 'main_loop
                    } else {
                        return ParserRet::new_expr(ret_expr)
                    }
                }

            AllSymbs::Term(TermType::Var) if matches!(pp.prev_token_type, Some(AllSymbs::Quant(_))) => {
                let term_type = TermType::Var; 
                let obj_name = pp.name_holder.get_name_uncond_new(term_type, token);
                if pp.name_holder.is_var_restr_by_quant(&obj_name) {
                    // var already restr ==> WARNING
                    pp.name_holder.add_waring_var(&obj_name)
                }
                pp.name_holder.restr_var_name(obj_name.clone());
                ParserRet::new_name(obj_name)
            }
            AllSymbs::Term(TermType::Var) => {
                let term_type = TermType::Var; 
                if !pp.name_holder.exist_name(term_type, &token) { 
                    // it not after Q and it is new... => it is free var!
                    let obj_name = pp.name_holder.add_new_free_var(token);
                    return ParserRet::new_name(obj_name)
                }
                let obj_name = pp.name_holder.get_last_existing_name(term_type, &token).unwrap();
                let obj_name = 
                    if !pp.name_holder.is_var_restr_by_quant(obj_name) {
                        // this var is free var! 
                        pp.name_holder.get_free_var(token)
                    } else {
                        // okey, it just old var (get name return the newest from old)
                        pp.name_holder.get_name(term_type, token)
                    };
                ParserRet::new_name(obj_name)
            }
            AllSymbs::Term(term_type) => {
                let obj_name = pp.name_holder.get_name(term_type, token);
                match term_type{
                    TermType::Const => ParserRet::new_name(obj_name),
                    TermType::Var => panic!("we cant be here"),
                    _ => {
                        // after Func/Pred name stay always '(' :
                        let open_br = _parse(
                            parser_rs, 
                            tokens, 
                            pp.symb_await(token_type, AllSymbs::Syntax(SyntaxSymbs::OpenBr))
                        );
                        if !(open_br.is_expr() && open_br.get_expr().is_empty()) { 
                            return ParserRet::new_bad(ParseError::ThereMustBe(AllSymbs::Syntax(SyntaxSymbs::OpenBr))) 
                        }
                      
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
                                // [+] check params len
                                if let Some(old_size) = pp.name_holder.get_param_len(term_type, &obj_name) {
                                    if old_size != terms.len() { 
                                        //println!("previously params len was {:?} but now its len {:?}", old_size, terms.len());
                                        return ParserRet::new_bad(ParseError::DifferentParamLen)
                                    }
                                } else {
                                    pp.name_holder.set_param_len(term_type, obj_name.clone(), terms.len());
                                }
                                // [-] check params len
                                if term_type == TermType::Func{
                                    return ParserRet::new_term(Term::new_func_by_param(obj_name, terms))
                                } else {
                                    if !ret_expr.is_empty() { panic!("expr must be empty") }

                                    ret_expr = Expr::new_predicate(obj_name, terms);

                                    if try_take_binary(&mut pp) {
                                        pp.allow_save(AllSymbs::Syntax(SyntaxSymbs::CloseBr));
                                        pp.allow_save(AllSymbs::End);
                                        continue 'main_loop
                                    } else {
                                        return ParserRet::new_expr(ret_expr)
                                    }
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
            AllSymbs::Syntax(SyntaxSymbs::CloseBr) => {
                if let Some(AllSymbs::Term(_)) = pp.prev_token_type{
                    ParserRet::new_expr(ret_expr)
                } else {
                    if ret_expr.is_empty() { return ParserRet::new_bad(ParseError::EmptyExpr) }
                    if try_take_binary(&mut pp) {
                        pp.allow_save(AllSymbs::Syntax(SyntaxSymbs::CloseBr));
                        pp.allow_save(AllSymbs::End);
                        continue 'main_loop
                    } else {
                        return ParserRet::new_expr(ret_expr)
                    }
                }
            }
            AllSymbs::Syntax(SyntaxSymbs::OpenBr) => {
                if let Some(AllSymbs::Term(_)) = pp.prev_token_type{
                    return ParserRet::new_expr(ret_expr)
                } else {
                    let ret_expr_temp = _parse(parser_rs, tokens, pp.next(token_type));
                    if ret_expr_temp.is_bad() { return ret_expr_temp }
                    if !ret_expr_temp.is_expr() { return ParserRet::new_bad(ParseError::ExpectedExpr) }
                    
                    ret_expr = ret_expr_temp.get_expr();
                    if ret_expr.is_empty() { return ParserRet::new_bad(ParseError::EmptyExpr) }
                    
                    pp.disallow_save(AllSymbs::End);
                    pp.set_can_binary_or_close();
                    continue;
                }
            }
            AllSymbs::Op(Operations::Binary(bop)) => 
                if ret_expr.is_empty() { 
                    ParserRet::new_bad(ParseError::EmptyExpr) 
                }
                else {
                    let expr = _parse(parser_rs, tokens, pp.next(token_type)).if_expr(
                            |right|Expr::apply_binary_op(bop, ret_expr, right)
                    );
                    if !expr.is_expr() { return expr }
                    ret_expr = expr.get_expr();

                    if try_take_binary(&mut pp) {
                        pp.allow_save(AllSymbs::Syntax(SyntaxSymbs::CloseBr));
                        pp.allow_save(AllSymbs::End);
                        continue 'main_loop
                    } else {
                        return ParserRet::new_expr(ret_expr)
                    }
                }
            
            _ => panic!("cant be here"),
        }


    }
}