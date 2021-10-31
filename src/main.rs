use std::io;
use std::sync::atomic::AtomicBool;
use first_order_logic::common::clause_system::ClauseSystem;
use first_order_logic::common::name::StdName;
use first_order_logic::common::ok_parse::OkParse;
use first_order_logic::common::parse_str::ParseStr;
use first_order_logic::common::parse;
use first_order_logic::logic::expr::Expr;
use first_order_logic::logic::operations::UnaryOperations;
use first_order_logic::logic::predicate_expr::PredicateExpr;
use first_order_logic::logic::substit::{DisplaySubst, SubstitutionApply};

fn main(){
    let std_in = io::stdin();
    let mut std_info = StdInfo{ max_tries: STD_INFO_INIT_MAX_TRIES };
    loop {
        match get_cmd(&std_in) {
            CMD::Help => help(), 
            CMD::Std => std(&std_in, &std_info),
            CMD::NewMaxResolvTry(x) => std_info.max_tries = x,
            
            CMD::MCU => if !mcu(&std_in) { break },

            CMD::ReadError | CMD::Exit => break,
            CMD::WrongCmd => {},
        }
        println!("");
    }
}

#[derive(PartialEq, Eq)]
enum CMD {
    WrongCmd,
    ReadError,
    Help,
    Exit,

    MCU,
    Std,
    NewMaxResolvTry(usize),
}

fn get_cmd(std_in: &io::Stdin) -> CMD {
    static FIRST: AtomicBool = AtomicBool::new(false);
    print!("CMD ");
    if !FIRST.load(std::sync::atomic::Ordering::Relaxed) { print!("[for help: `help`]") }
    print!(":  ");
    if !flush() { return CMD::ReadError }
    let mut input_str = String::new();
    if let Err(_) = std_in.read_line(&mut input_str) { 
        println!("failed to read line!");
        return CMD::ReadError
    }

    let mut x = input_str.trim().split(" ");
    let cmd = if let Some(cmd) = x.next() { cmd } else { 
        println!("empty cmd"); 
        FIRST.store(true, std::sync::atomic::Ordering::Relaxed);
        return CMD::WrongCmd
    };

    let cmd = match cmd {
        "h" | "hell" | "help" => CMD::Help, 
        "e" | "exit" => CMD::Exit, 
        "mcu" => CMD::MCU,
        "std" => CMD::Std,
        "set-mrt" | "max-res-try" => {
            let len = x.next();
            if let Some(len) = len {
                if let Ok(len) = len.parse() {
                    CMD::NewMaxResolvTry(len)
                } else {
                    println!("parameter must be num, but was: `{}`", len);
                    CMD::WrongCmd 
                }
            } else {
                println!("here must stay num parameter - amount of resolving try"); 
                CMD::WrongCmd 
            }
        },
        _ => {
            println!("wrong cmd: `{}`", cmd); 
            CMD::WrongCmd
        }
    };
    FIRST.store(true, std::sync::atomic::Ordering::Relaxed);
    cmd
}

fn help(){
    println!("[h | help]: print this msg");
    println!("[e | exit]: exit from the program");
    println!("[mcu]: most common unifier for two predicate");
    println!("       | currently it not print each action,");
    println!("       | just final result");
    println!("[std]: step-by-step algorithm for getting empty clause");
    println!("       | currently end step (resolution) work with max-iter");
    println!("[max-res-try][x:int]: set amount of tries in resolution method to x");
}

fn std(std_in: &io::Stdin, info: &StdInfo){
    let ruleset = ParseStr::create_std_ruleset();
    let mut input_str = String::new();
    println!("helpful symbs: ∃∃∃ ∀∀∀ ¬¬¬ →→→ ∧∧∧ ∨∨∨");
    println!("please write a first-order-logic expr:");
    if let Err(_) = std_in.read_line(&mut input_str) { 
        println!("failed to read line!");
        return 
    }
    let ps = ParseStr::new(input_str.trim_end()); 
    let expr = parse::parse::<StdName, _, _>(&ruleset, &mut ps.into_iter());
    println!();
    match expr {
        Err(err) => println!("invalid input {:?}", err),
        Ok(ok) => {
            println!("expr:  {}", ok);
            let warns = ok.get_name_holder().get_waring_vars();
            if warns.len() != 0 {
                println!(""); 
                println!("WARNING"); 
                println!("initial name of the following vars was restricted more than one quantor in the same time: ");
                ok.display_warning_vars();
                println!(""); 
            }
            let ok = if ok.get_name_holder().exist_free_vars()  {
                print!("here exist free vars: ");
                ok.display_free_vars();
                let ok = ok.expr_close();
                println!("so after closing the formula it will look like: ");
                println!("expr:  {}", ok);
                ok
            } else {
                println!("here no free vars, it fine!");
                ok
            };
            println!("now add logical-not before formula: ");
            let mut ok = ok.apply_expr_action(|expr|expr.apply_unary_op(UnaryOperations::Not));
            println!("expr:  {}", ok);
            println!("now transform [A → B] into [¬A ∨ B] : ");
            ok.get_mut_expr().impl_transformation();
            println!("expr:  {}", ok);
            println!("move in logical-not: ");
            ok.get_mut_expr().logical_not_moving();
            println!("expr:  {}", ok);
            println!("move out quants: ");
            ok.get_mut_expr().quant_outing();
            println!("expr:  {}", ok);
            println!("transform to cnf: ");
            ok.get_mut_expr().to_cnf();
            println!("expr:  {}", ok);

            println!("");
            let subst = ok.exist_quant_transform();
            println!("for deleting exist quantor we use substitution: {}", DisplaySubst{nh: ok.get_name_holder(), substs: &subst} );
            println!("now our expr: {}", ok);
            
            println!("");
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
            println!("try resolvent (no more than {}): ", info.max_tries);
            cs.made_all_resolvent(Some(info.max_tries));
            println!("{}", cs);
        }
    }
}

fn flush() -> bool {
    if let Err(_) = io::Write::flush(&mut io::stdout()) {
        println!("failed to print line!");
        return false        
    }
    return true
}

const STD_INFO_INIT_MAX_TRIES: usize = 12;
struct StdInfo{ max_tries: usize, }

fn mcu(std_in: &io::Stdin) -> bool {
    print!("write left predicate:    ");
    if !flush() { return false } 
    let mut left = String::new();
    if let Err(_) = std_in.read_line(&mut left) { 
        println!("failed to read line!");
        return false
    } 
    print!("write right predicate:   ");
    if !flush() { return false } 
    let mut right = String::new();
    if let Err(_) = std_in.read_line(&mut right) { 
        println!("failed to read line!");
        return false
    } 

    mcu_inner(left.trim(), right.trim());
    true
}

fn mcu_inner(str_predicate_a: &str, str_predicate_b: &str){
    let ruleset = ParseStr::create_std_ruleset();
    let expr_str = "".to_owned() + str_predicate_a + " | " + str_predicate_b;
    let ps = ParseStr::new(&expr_str);
    let expr = parse::parse::<StdName, _, _>(&ruleset, &mut ps.into_iter());
    if let Err(err) = expr {
        println!("invalid input {:?}", err);
        return
    } 
    let mut expr = expr.unwrap();
    //println!("expr before subst: {}", expr);
    let subst = match expr.get_expr() { 
        Expr::BinaryOp(bop) 
            if bop.borrow().get_lexpr().is_predicate() 
            && bop.borrow().get_rexpr().is_predicate() => 
                PredicateExpr::most_comon_unifier(
                    &bop.borrow().get_lexpr().get_expr_predicate(), 
                    &bop.borrow().get_rexpr().get_expr_predicate(), 
                    |_,_|{}
                ),
        _ => {
            println!("invalid input! both must be just predicate ( `P(x, f(y, c), ..)` )");
            return        
        }
    };
    if subst.is_none() { println!("subst no exist"); }
    else { 
        let subst = subst.unwrap();
        println!("mcu: {}", DisplaySubst{ nh: &expr.get_name_holder(), substs: &subst});
        subst.apply(expr.get_mut_expr());
        let (expr, nh) = expr.disassemble(); 
        match expr {
            Expr::BinaryOp(bop) => {
                let borrow = bop.borrow();
                let left = borrow.get_lexpr();    
                let ok_parse = OkParse::new(left.clone(), nh);
                println!("expr after subst (second the same): {}", ok_parse);
            }
            _ => panic!("never here, it 100% binary op"),
        }
    };
    println!("");
}
