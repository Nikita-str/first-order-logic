use std::{cell::{Ref, RefCell, RefMut}, collections::HashMap, rc::Rc};
use crate::{common::name::Name};
use super::{all_symbs::AllSymbs, operations::{BinaryOperations, Operations, UnaryOperations}, predicate_expr::PredicateExpr, quants::Quants, term_type::TermType, terms::Term};


#[derive(Debug)]
pub struct UnaryOpExpr<N: Name>{
    op: UnaryOperations,
    formula: Expr<N>,  
}
impl<N:Name> UnaryOpExpr<N>{
    pub fn new(op: UnaryOperations, formula: Expr<N>) -> Self { Self { op, formula } }
    
    pub fn get_op(&self) -> UnaryOperations { self.op }
    pub fn get_expr(&self) -> &Expr<N> { &self.formula }
    pub fn get_expr_mut(&mut self) -> &mut Expr<N> { &mut self.formula }
}

#[derive(Debug)]
pub struct BinaryOpExpr<N: Name>{
    op: BinaryOperations,
    left_formula: Expr<N>,  
    right_formula: Expr<N>,  
}
impl<N:Name> BinaryOpExpr<N>{
    pub fn new(op: BinaryOperations, left: Expr<N>, right: Expr<N>) -> Self 
    { Self { op, left_formula: left, right_formula: right } }

    pub fn get_op(&self) -> BinaryOperations { self.op }
    pub fn get_lexpr(&self) -> &Expr<N> { &self.left_formula }
    pub fn get_lexpr_mut(&mut self) -> &mut Expr<N> { &mut self.left_formula }
    pub fn get_rexpr(&self) -> &Expr<N> { &self.right_formula }
    pub fn get_rexpr_mut(&mut self) -> &mut Expr<N> { &mut self.right_formula }
}

#[derive(Debug)]
pub struct ExprQuant<N: Name>{
    quant: Quants,
    var_name: N,
    pub expr: Expr<N>,
}
impl<N:Name> ExprQuant<N>{
    pub fn new(q: Quants, var_name: N, expr: Expr<N>) -> Self { Self { quant: q, var_name, expr } }

    pub fn get_quant(&self) -> Quants { self.quant }
    pub fn get_var_name(&self) -> &N { &self.var_name }
    pub fn get_expr(&self) -> &Expr<N> { &self.expr }
    pub fn get_expr_mut(&mut self) -> &mut Expr<N> { &mut self.expr }
}

#[derive(Debug)]
pub enum Expr<N: Name>{
    Empty,
    Predicate(Rc<RefCell<PredicateExpr<N>>>),
    UnaryOp(Rc<RefCell<UnaryOpExpr<N>>>),
    BinaryOp(Rc<RefCell<BinaryOpExpr<N>>>),
    Quant(Rc<RefCell<ExprQuant<N>>>),
}

impl<N:Name> Expr<N>{
    pub fn is_empty(&self) -> bool { if let Expr::Empty = self { true } else { false }  }
    pub fn is_binary_op(&self) -> bool { if let Expr::BinaryOp(_) = self { true } else { false }  }
    pub fn is_predicate(&self) -> bool { if let Expr::Predicate(_) = self { true } else { false }  }
    pub fn is_quant(&self) -> bool { if let Expr::Quant(_) = self { true } else { false }  }

    pub fn apply_unary_op(self, op: UnaryOperations) -> Self {
        Expr::UnaryOp(Rc::new(RefCell::new(UnaryOpExpr::new(op, self)))) 
    }
    pub fn apply_binary_op(op: BinaryOperations, left: Self, right: Self) -> Self {
        Expr::BinaryOp(Rc::new(RefCell::new(BinaryOpExpr::new(op, left, right)))) 
    }
    pub fn apply_quant(self, quant: Quants, var_name: N) -> Self {
        Expr::Quant(Rc::new(RefCell::new(ExprQuant::new(quant, var_name, self)))) 
    }

    pub fn new_predicate(name: N, params: Vec<Term<N>>) -> Self{
        Expr::Predicate(Rc::new(RefCell::new(PredicateExpr::new(name, params))))
    }

    
    pub fn get_expr_predicate(&self) -> Ref<'_, PredicateExpr<N>>{
        match self {
            Expr::Predicate(p) => Rc::as_ref(p).borrow(),
            _ => panic!("not predicate expr")
        }
    }

    pub fn get_expr_quant(&self) -> Ref<ExprQuant<N>>{
        match self {
            Expr::Quant(q) => Rc::as_ref(q).borrow(),
            _ => panic!("not quant expr")
        }
    }

    pub fn get_expr_quant_mut(&self) -> RefMut<ExprQuant<N>>{
        match self {
            Expr::Quant(q) => Rc::as_ref(q).borrow_mut(),
            _ => panic!("not quant expr")
        }
    }

    pub fn get_expr_unary(&self) -> Ref<'_, UnaryOpExpr<N>>{
        match self {
            Expr::UnaryOp(uop) => Rc::as_ref(uop).borrow(),
            _ => panic!("not unary op expr")
        }
    }
    
    pub fn get_expr_binary(&self) -> Ref<'_, BinaryOpExpr<N>>{
        match self {
            Expr::BinaryOp(bop) => Rc::as_ref(bop).borrow(),
            _ => panic!("not binary op expr")
        }
    }

    pub fn get_priority(&self) -> Option<usize> {
        match self {
            Expr::Empty => AllSymbs::Empty.get_priority(),
            Expr::Predicate(_) => AllSymbs::Term(TermType::Pred).get_priority(),
            Expr::Quant(q) => AllSymbs::Quant(q.borrow().get_quant()).get_priority(),
            Expr::UnaryOp(uop) => AllSymbs::Op(Operations::Unary(uop.borrow().get_op())).get_priority(),
            Expr::BinaryOp(bop) => AllSymbs::Op(Operations::Binary(bop.borrow().get_op())).get_priority(),
        }
    }
}


impl<N:Name> Expr<N>{
    pub fn impl_transformation(& mut self){
        match self {
            Expr::BinaryOp(op) => {
                op.borrow_mut().left_formula.impl_transformation();
                op.borrow_mut().right_formula.impl_transformation();
                let old_left = op.borrow().left_formula.clone();
                let bin_op = op.borrow().op;
                if let BinaryOperations::Impl = bin_op{
                    op.borrow_mut().op = BinaryOperations::Or; 
                    op.borrow_mut().left_formula = 
                        Expr::UnaryOp(Rc::new(RefCell::new(UnaryOpExpr::new(UnaryOperations::Not, old_left))));
                }
            },
            
            Expr::Quant(q) => { q.borrow_mut().expr.impl_transformation(); }
            Expr::UnaryOp(uop) => { uop.borrow_mut().formula.impl_transformation(); }

            Expr::Predicate(_) | Expr::Empty => {}
        };
    }

    pub fn logical_not_moving(&mut self){ self._logical_not_moving(false) }
    fn _logical_not_moving(&mut self, need_not: bool){
        if self.is_predicate() {
            if need_not { 
                let pred = self.clone();
                *self = pred.apply_unary_op(UnaryOperations::Not); 
            }
            return
        }

        match self {
            Expr::BinaryOp(bop) => {
                let bin_op = bop.borrow_mut().op;
                match bin_op {
                    BinaryOperations::Impl => {
                        // not(A impl B) = not (not A or B) = (A and not B)
                        bop.borrow_mut().left_formula._logical_not_moving(!need_not);
                        bop.borrow_mut().right_formula._logical_not_moving(need_not);
                        bop.borrow_mut().op = 
                            if need_not { BinaryOperations::And }
                            else { BinaryOperations::Or };
                    }
                    _ => {
                        bop.borrow_mut().left_formula._logical_not_moving(need_not);
                        bop.borrow_mut().right_formula._logical_not_moving(need_not);
                        let op = bop.borrow_mut().op;
                        if need_not { bop.borrow_mut().op = op.after_logical_not(); }
                    }

                }
            }
            Expr::UnaryOp(uop) => {
                let un_op =  uop.borrow_mut().op;
                match un_op {
                    UnaryOperations::Not => {
                        if !need_not && uop.borrow_mut().formula.is_predicate() { return }
                        let old_expr = uop.borrow_mut().formula.clone();
                        *self =  old_expr;
                        self._logical_not_moving(!need_not);
                    }
                }
            }
            Expr::Quant(q) => {
                let old_quant = q.borrow_mut().quant;
                if need_not { q.borrow_mut().quant = old_quant.not(); }
                q.borrow_mut().expr._logical_not_moving(need_not)
            }
            Expr::Predicate(_) => panic!("it must process outside match expr!"),
            Expr::Empty => panic!("not(empty) : empty may mean false so this need to be true, may not mean"),
        }
    }
}



struct QuantOutingInfo<N>{ 
    ///  contain pairs: ( `[lvl, quant]`, `Vec<N>` )  
    /// 
    /// where Vec<N> - names of limited vars by quantor `quant` on level `lvl` 
    quants_by_lvl: HashMap<(usize, Quants), Vec<N>>,
    max_quant_lvl: usize, 
    empty_vec: Vec<N>,
}

impl<N> QuantOutingInfo<N>{
    pub fn new() -> Self { Self { quants_by_lvl: HashMap::new(), max_quant_lvl: 0, empty_vec: vec![] } }

    pub fn add_name(&mut self, lvl: usize, quant: Quants, value: N) {
        if lvl > self.max_quant_lvl { panic!("wrong lvl of quant") }
        if lvl == self.max_quant_lvl { self.max_quant_lvl = self.max_quant_lvl + 1; }
        if let Some(vec) = self.quants_by_lvl.get_mut(&(lvl, quant)) {
            vec.push(value)
        } else {
            let vec = vec![value];
            self.quants_by_lvl.insert((lvl, quant), vec);
        }
    }

    pub fn exist_lvl(&self, lvl: usize) -> bool { lvl < self.max_quant_lvl }

    /// # panic
    /// - if `!self.exist_lvl(lvl)`
    pub fn get_values(&self, lvl: usize, quant: Quants) -> &Vec<N>{
        if !self.exist_lvl(lvl) { panic!("wrong lvl") }
        if let Some(vec) = self.quants_by_lvl.get(&(lvl, quant)) { vec }
        else { &self.empty_vec } 
    }

    pub fn get_top_lvl(&self) -> usize { self.max_quant_lvl }
}

impl<N:Name> Expr<N>{
    /// # restriction
    /// you can do it only if all logical-not stay after quants (only after call `self.logical_not_moving`)
    /// 
    /// in other way it is not correct transformation !
    pub fn quant_outing(&mut self) {
        let mut qo_info = QuantOutingInfo::new();
        self.pre_quant_outing(0, &mut qo_info);
        self.post_quant_outing(qo_info);
    }
   
    fn pre_quant_outing(&mut self, cur_quant_lvl:usize, qo_info: &mut QuantOutingInfo<N>){
        match self {
            Expr::BinaryOp(bop) => {
                bop.borrow_mut().left_formula.pre_quant_outing(cur_quant_lvl, qo_info); // <--- NOT TAIL-REC 
                bop.borrow_mut().right_formula.pre_quant_outing(cur_quant_lvl, qo_info)
            }
            Expr::UnaryOp(uop) => uop.borrow_mut().formula.pre_quant_outing(cur_quant_lvl, qo_info),
            Expr::Quant(_) => {
                let expr = self.get_expr_quant().expr.clone();
                let quant = self.get_expr_quant().quant;
                let name = self.get_expr_quant().var_name.clone();
                qo_info.add_name(cur_quant_lvl, quant, name);
                *self = expr;
                self.pre_quant_outing(cur_quant_lvl + 1, qo_info)
            }
            Expr::Empty | Expr::Predicate(_) => {}
        }
    }

    fn post_quant_outing(&mut self, qo_info: QuantOutingInfo<N>){
        let mut lvl = qo_info.get_top_lvl();
        while lvl > 0 {
            // cause we start from end => firstly All quants, then Exist quants

            lvl = lvl - 1;
            for name_of_all_q in qo_info.get_values(lvl, Quants::All){
                *self = self.clone().apply_quant(Quants::All, name_of_all_q.clone());
            }
            for name_of_exist_q in qo_info.get_values(lvl, Quants::Exist){
                *self = self.clone().apply_quant(Quants::Exist, name_of_exist_q.clone());
            }
        }
    }
}

impl<N:Name> Expr<N>{
    /// # MAYBE ALGO-ERROR: maybe need real clone 
    pub fn to_cnf(&mut self){
        return match self {
            Expr::BinaryOp(bop) => {
                if bop.borrow_mut().left_formula.is_quant() { panic!("initial formula cant be guarantee transfor to knf") }
                if bop.borrow_mut().right_formula.is_quant() { panic!("initial formula cant be guarantee transfor to knf") }
                bop.borrow_mut().left_formula.to_cnf();
                bop.borrow_mut().right_formula.to_cnf();
                
                const OR: BinaryOperations =  BinaryOperations::Or;
                // it is not or-binary-op => not interesting 
                if !bop.borrow().get_op().eq(&OR) { return }

                const AND: BinaryOperations = BinaryOperations::And;
                let right_is_and = {
                    let e = &bop.borrow_mut().right_formula;
                    if e.is_binary_op() { e.get_expr_binary().get_op().eq(&AND) } else { false }
                };
                let left_is_and = {
                    let e = &bop.borrow_mut().left_formula;
                    if e.is_binary_op() { e.get_expr_binary().get_op().eq(&AND) } else { false }
                };
                

                // (*): [L, L]; [L, R]; [R, L]; [R; R]
                // we can take clone by path (*) but i'm lazy to do this, 
                //    so we look carefully and try to do no mistake 
                type BOP<N> = Rc<RefCell<BinaryOpExpr<N>>>;
                let get_ll = |bop:&BOP<_>|bop.borrow().get_lexpr().get_expr_binary().get_lexpr().clone(); 
                let get_lr = |bop:&BOP<_>|bop.borrow().get_lexpr().get_expr_binary().get_rexpr().clone(); 
                let get_rl = |bop:&BOP<_>|bop.borrow().get_rexpr().get_expr_binary().get_lexpr().clone(); 
                let get_rr = |bop:&BOP<_>|bop.borrow().get_rexpr().get_expr_binary().get_rexpr().clone(); 

                if left_is_and & right_is_and {
                    // (X & Y) ∨ (Z & W)
                    //  TO CNF:
                    // (X ∨ Z) & (X ∨ W) & (Y ∨ Z) & (Y ∨ W)
                    let x = get_ll(bop);
                    let y = get_lr(bop);
                    let z = get_rl(bop); 
                    let w = get_rr(bop); 
                    
                    bop.borrow_mut().left_formula = 
                        Self::apply_binary_op(
                            AND, 
                            Self::apply_binary_op(OR, x.clone(), z.clone()), 
                            Self::apply_binary_op(OR, x, w.clone()), 
                        );
                    bop.borrow_mut().right_formula = 
                        Self::apply_binary_op(
                            AND, 
                            Self::apply_binary_op(OR, y.clone(), z), 
                            Self::apply_binary_op(OR, y, w), 
                        ); 
                } else if left_is_and {
                    // (B & C) ∨ A : (A ∨ B) & (A ∨ C)
                    let b = get_ll(bop); 
                    let c = get_lr(bop);
                    let a = bop.borrow().get_rexpr().clone();
                    bop.borrow_mut().left_formula = Self::apply_binary_op(OR, a.clone(), b);
                    bop.borrow_mut().right_formula = Self::apply_binary_op(OR, a, c);
                } else if right_is_and {
                    // A ∨ (B & C)  :  (A ∨ B) & (A ∨ C)
                    let a = bop.borrow().get_lexpr().clone();
                    let b = get_rl(bop); 
                    let c = get_rr(bop);
                    bop.borrow_mut().left_formula = Self::apply_binary_op(OR, a.clone(), b);
                    bop.borrow_mut().right_formula = Self::apply_binary_op(OR, a, c);
                }

                if left_is_and || right_is_and {  bop.borrow_mut().op = AND; }
            }
            Expr::UnaryOp(uop) => {
                if !uop.borrow_mut().formula.is_predicate() { panic!("initial formula cant be guarantee transfor to knf") }
            }
            Expr::Quant(q) => q.borrow_mut().expr.to_cnf(),
            Expr::Predicate(_) | Expr::Empty => {}
        }
    }
}


impl<N: Name> Clone for Expr<N>{
    fn clone(&self) -> Self {
        match self {
            Expr::Empty => Expr::Empty,
            Expr::Predicate(p) => Expr::Predicate(Rc::clone(p)),
            Expr::Quant(q) => Expr::Quant(Rc::clone(q)),
            Expr::UnaryOp(uop) => Expr::UnaryOp(Rc::clone(uop)),
            Expr::BinaryOp(bop) => Expr::BinaryOp(Rc::clone(bop)),
        }
    }
}