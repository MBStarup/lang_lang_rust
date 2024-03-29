use core::panic;
use std::{
    collections::VecDeque,
    env::args,
    fmt::Debug,
    fs::{self, File},
    io::{stdout, Write},
    iter::Peekable,
};

struct CharProvider {
    position: usize,
    data: String,
}

impl Iterator for CharProvider {
    type Item = char;

    fn next(&mut self) -> Option<Self::Item> {
        if self.position < (self.data.len()/* / std::mem::size_of::<Self::Item>() */) {
            // len says it's the length in bytes, not chars, but uhmmm, that's kinda bs, either that or the char is one byte, and the sizeof is wrong
            let res = self.data.chars().nth(self.position);
            self.position += 1;
            res
        } else {
            Option::None
        }
    }
}

struct TokenProvider<T>
where
    T: Iterator<Item = char>,
{
    char_provider: std::iter::Peekable<T>,
}

impl<T> TokenProvider<T>
where
    T: Iterator<Item = char>,
{
    fn new(char_provider: T) -> TokenProvider<T> {
        TokenProvider { char_provider: char_provider.peekable() }
    }
}

impl<T> Iterator for TokenProvider<T>
where
    T: Iterator<Item = char>,
{
    type Item = Token;

    fn next(&mut self) -> Option<Self::Item> {
        match self.char_provider.next() {
            Option::Some(c) if c.is_whitespace() => {
                while let Option::Some(c) = self.char_provider.peek() {
                    if c.is_whitespace() {
                        self.char_provider.next();
                    } else {
                        break;
                    }
                }
                // Option::Some(Token::WhiteSpace)
                self.next()
            },

            Option::Some(c) if c.is_ascii_digit() => {
                //* Int declaration
                let mut num_str = c.to_string();
                while let Option::Some(c) = self.char_provider.peek() {
                    if c.is_ascii_digit() {
                        num_str += &c.to_string();
                        self.char_provider.next();
                    } else {
                        break;
                    }
                }
                Option::Some(Token::Number(num_str.parse().expect(format!("Could not parse number {}", num_str).as_str())))
            },
            Option::Some(c) if c.is_ascii_alphabetic() || c == '_' || c == '!' => {
                let mut symbol_str = c.to_string();
                while let Option::Some(c) = self.char_provider.peek() {
                    if c.is_ascii_alphabetic() || c == &'_' || c == &'!' {
                        symbol_str += &c.to_string();
                        self.char_provider.next();
                    } else {
                        break;
                    }
                }
                Option::Some(Token::Symbol(symbol_str))
            },
            Option::Some(c) if c == '{' => Some(Token::CurlyStart),
            Option::Some(c) if c == '}' => Some(Token::CurlyEnd),
            Option::Some(c) if c == '#' => {
                while let Option::Some(c) = self.char_provider.peek() {
                    if c != &'#' {
                        self.char_provider.next();
                    } else {
                        self.char_provider.next();
                        break;
                    }
                }
                // Option::Some(Token::Comment)
                self.next()
            },
            Option::Some(c) if c == ':' => Some(Token::Colon),
            Option::Some(c) if c == ';' => Some(Token::SemiColon), //* Function declaration
            Option::Some(c) if c == '(' => Some(Token::ParensStart),
            Option::Some(c) if c == ')' => Some(Token::ParensEnd),
            //Operators
            Option::Some(c) if c == '+' => Some(Token::Operator(Operator::Add)),
            Option::Some(c) if c == '-' => Some(Token::Operator(Operator::Subtract)),
            Option::Some(c) if c == '*' => Some(Token::Operator(Operator::Multiply)),
            Option::Some(c) if c == '/' => Some(Token::Operator(Operator::Divide)),
            Option::Some(c) if c == '&' => Some(Token::Operator(Operator::And)),
            Option::Some(c) if c == '|' => Some(Token::Operator(Operator::Or)),
            Option::Some(c) if c == '=' => Some(Token::Operator(Operator::Equals)),
            Option::Some(c) if c == '<' => Some(Token::Operator(Operator::LessThan)),
            Option::Some(c) if c == '>' => Some(Token::Operator(Operator::GreaterThan)),
            Option::None => Option::None,

            Option::Some(c) => panic!("Error parsing: {}", c),
        }
    }
}

struct LoggingIter<T, I>
where
    T: Iterator<Item = I>,
    I: Debug,
{
    internal_iter: T,
    logging: bool,
}

impl<T, I> LoggingIter<T, I>
where
    T: Iterator<Item = I>,
    I: Debug,
{
    fn new(iter: T, log: bool) -> LoggingIter<T, I> {
        LoggingIter { internal_iter: iter, logging: log }
    }
}

impl<T, I> Iterator for LoggingIter<T, I>
where
    T: Iterator<Item = I>,
    I: Debug,
{
    type Item = I;

    fn next(&mut self) -> Option<Self::Item> {
        let x = self.internal_iter.next();
        if self.logging {
            println!("{x:?}");
        }
        x
    }
}

#[derive(Debug, Clone)]
enum Token {
    Operator(Operator),
    ParensStart,
    ParensEnd,
    Number(i32),
    Symbol(String),
    CurlyStart,
    CurlyEnd,
    SemiColon,
    Colon,
    /*
    Comma,
    WhiteSpace,
    Comment,
    */
}

#[derive(Debug, Clone)]
enum Operator {
    Add,
    Subtract,
    Multiply,
    Divide,
    And,
    Or,
    Equals,
    LessThan,
    GreaterThan,
}

fn parse<'a, T: Iterator<Item = Token>>(tokens: T) -> Option<Expr> {
    fn parse_rec<T: Iterator<Item = Token>>(tokens: &mut Peekable<T>) -> Option<Expr> {
        match tokens.next() {
            Some(Token::CurlyStart) => {
                let mut res: Vec<Expr> = Vec::new();

                while match tokens.peek() {
                    Some(Token::CurlyEnd) => false,
                    Some(_) => true,
                    None => panic!("Unclosed multiExpr"),
                } {
                    res.push(parse_rec(tokens).unwrap())
                }

                tokens.next(); // Remove curly end

                Some(Expr::MultiExpr(res))
            },
            //Cringe clone occurs
            Some(Token::Symbol(symbol)) => match tokens.peek().cloned() {
                Some(Token::Colon) => {
                    tokens.next();
                    Some(Expr::Assign(Box::new(parse_rec(tokens).unwrap_or_else(|| panic!("Cannot assign empty value to {symbol:?}"))), symbol))
                },
                Some(_) => parse_rec_after_expr(tokens, Expr::Eval(symbol)),
                None => None,
            },
            //Cringe clone occurs
            Some(Token::Number(number)) => parse_rec_after_expr(tokens, Expr::Int(number)),
            Some(Token::SemiColon) => {
                let mut params: Vec<String> = Vec::new();

                while match tokens.next() {
                    Some(Token::SemiColon) => false,
                    Some(Token::Symbol(symbol)) => {
                        params.push(symbol.to_owned()); // idk about this one homie...
                        true
                    },
                    Some(token) => panic!("Unknown token ({token:?}) in function params"),
                    None => panic!("Unclosed function declaration"),
                } {}
                let function_expression = Expr::Func(Box::new(parse_rec(tokens).expect("Function body missing")), params);
                parse_rec_after_expr(tokens, function_expression)
            },
            None => None,
            Some(token) => panic!("Bad token: {token:?}"),
        }
    }

    fn parse_rec_after_expr<T: Iterator<Item = Token>>(tokens: &mut Peekable<T>, last_expr: Expr) -> Option<Expr> {
        match tokens.peek().clone() {
            Some(Token::Operator(_)) => {
                if let Some(Token::Operator(operator)) = tokens.next() {
                    Some(Expr::BiExpr(Box::new(last_expr), Box::new(parse_rec(tokens).unwrap_or_else(|| panic!("Missing right side of BiExpr {operator:?}"))), operator))
                } else {
                    todo!("unreachable")
                }
            },
            Some(Token::ParensStart) => {
                let mut params: Vec<Expr> = Vec::new();
                tokens.next();

                while match tokens.peek() {
                    Some(Token::ParensEnd) => false,
                    Some(_) => true,
                    None => panic!("Unclosed function call"),
                } {
                    params.push(parse_rec(tokens).unwrap())
                }

                tokens.next(); // Remove ParensEnd

                parse_rec_after_expr(tokens, Expr::Call(Box::new(last_expr), params))
            },
            Some(_) => Some(last_expr),
            None => Some(last_expr),
        }
    }

    parse_rec(&mut tokens.peekable())
}

#[derive(Debug)]
enum Expr {
    MultiExpr(Vec<Expr>),
    Int(i32),
    Eval(String), // TODO: think about simmilarities between this and function (and int?)
    BiExpr(Box<Expr>, Box<Expr>, Operator),
    Func(Box<Expr>, Vec<String>),
    Assign(Box<Expr>, String),
    Call(Box<Expr>, Vec<Expr>),
}

fn compile_to_asm(entry: Expr, symbol_map: &mut VecDeque<(VecDeque<SymbolTableEntry>, i32)>) -> String {
    // TODO: better way to build strings pepela

    static mut FUNCTION_COUNTER: u64 = 0; // TODO: be better

    let mut result = "
    ;compiled with rust lang lang compiler

        global    _start

        section   .text
        _start:
        push rbp
        mov rbp, rsp
        
        lea rax, [__builtin__print] ; for print, todo, something better pepela
        push rax
        "
    .to_owned();

    // put print in the scope
    let scope = symbol_map.iter_mut().next().expect("No current scope");
    scope.1 += 1;
    let print = SymbolTableEntry { symbol: "print".to_owned(), position: scope.1 * -8 };
    scope.0.push_front(print);
    // end builtin print stuff

    unsafe fn compile_to_asm_rec_helper(entry: Expr, symbol_map: &mut VecDeque<(VecDeque<SymbolTableEntry>, i32)>) -> String {
        match entry {
            Expr::Assign(expr, symbol) => {
                // TODO: evaluate expr
                let mut result = ";Assign\n".to_owned();
                result += &compile_to_asm_rec_helper(*expr, symbol_map);

                let scope = symbol_map.iter_mut().next().expect("No current scope");
                // Check that it's a new assignment
                if let Some(entry) = scope.0.iter().find(|x| x.symbol.eq(&symbol)) {
                    // store value at exisitng location
                    let offset = entry.position;
                    result += &format!("mov [rbp{offset:+}], rax\n");
                } else {
                    // TODO: store at pointer location
                    result += "push rax\n";

                    // TODO: store pointer location in some sort of table by symbol
                    scope.1 += 1;
                    let entry = SymbolTableEntry { symbol: symbol, position: scope.1 * -8 };
                    scope.0.push_front(entry);
                }

                // TODO: make sure designated eval return register is set to the value (I think it shoul dbe already as that's the last thing we evaled but :shrug: make sure)

                result
            },
            Expr::Int(num) => {
                format!(";Number\nmov rax, {num }\n")
            },
            Expr::BiExpr(expr_1, expr_2, operator) => match operator {
                // TODO: again, please just make the operators enums, this is pain
                Operator::Add => {
                    let mut result = ";Addition\n".to_owned();
                    result += "push rdi\n"; //cache rdi on stack
                    result += &compile_to_asm_rec_helper(*expr_2, symbol_map);
                    result += "mov rdi, rax\n"; //move rax (result of expr_1) into rdi
                    result += &compile_to_asm_rec_helper(*expr_1, symbol_map);
                    result += "add rax, rdi\n"; //add them and store in rax
                    result += "pop rdi\n"; //restore rdi
                    result
                },
                Operator::Multiply => {
                    let mut result = ";Multiplication\n".to_owned();
                    result += "push rdi\n"; //cache rdi on stack
                    result += &compile_to_asm_rec_helper(*expr_2, symbol_map);
                    result += "mov rdi, rax\n"; //move rax (result of expr_1) into rdi
                    result += &compile_to_asm_rec_helper(*expr_1, symbol_map);
                    result += "mul rdi\n"; //multiply them and store in rax (rax as des is implicit for mul )
                    result += "pop rdi\n"; //restore rdi
                    result
                },
                Operator::Subtract => {
                    let mut result = ";Subtraction\n".to_owned();
                    result += "push rdi\n"; //cache rdi on stack
                    result += &compile_to_asm_rec_helper(*expr_2, symbol_map);
                    result += "mov rdi, rax\n"; //move rax (result of expr_1) into rdi
                    result += &compile_to_asm_rec_helper(*expr_1, symbol_map);
                    result += "sub rax, rdi\n"; //subtract them and store in rax
                    result += "pop rdi\n"; //restore rdi
                    result
                },
                Operator::Divide => {
                    let mut result = ";Division\n".to_owned();
                    result += "push rdi\n"; //cache rdi on stack
                    result += &compile_to_asm_rec_helper(*expr_2, symbol_map);
                    result += "mov rdi, rax\n"; //move rax (result of expr_1) into rdi
                    result += &compile_to_asm_rec_helper(*expr_1, symbol_map);
                    result += "div rdi\n"; //divide them and store in rax (implied)
                    result += "pop rdi\n"; //restore rdi
                    result
                },
                Operator::And => {
                    FUNCTION_COUNTER = FUNCTION_COUNTER + 1;
                    let end_label = format!("and_end_{FUNCTION_COUNTER}");
                    let false_label = format!("and_false_{FUNCTION_COUNTER}");
                    let mut result = ";And\n".to_owned();
                    result += "push rdi\n"; //cache rdi on stack
                    result += &compile_to_asm_rec_helper(*expr_1, symbol_map);
                    result += "cmp rax, 0\n"; //check if zero
                    result += &format!("je {false_label}\n"); //jump if 0 (shortcircuit)
                    result += "mov rdi, rax\n"; //move rax (result of expr_1) into rdi
                    result += &compile_to_asm_rec_helper(*expr_2, symbol_map);
                    result += "cmp rax, 0\n"; //check if zero
                    result += &format!("je {false_label}\n"); //jump if 0 (shortcircuit)
                    result += "mov rax, 1\n"; //true case
                    result += &format!("jmp {end_label}\n").to_owned(); //jump to skip false case
                    result += &format!("{false_label}:\n"); //label
                                                            // result += "mov rax, 0"; //false case // redundant, case is only reached if rax == 0
                    result += &format!("{end_label}:\n"); //label
                    result += "pop rdi\n"; //restore rdi
                    result
                },
                o => todo!("{o:?}"),
            },
            Expr::MultiExpr(exprs) => {
                let mut result = ";MultiExpr\n".to_owned();
                symbol_map.push_front((VecDeque::new(), 0)); //new scope

                // result += "mov rdi, rbp\n";
                result += "push rbp\n";
                result += "mov rbp, rsp\n";

                for expr in exprs {
                    result += &compile_to_asm_rec_helper(expr, symbol_map);
                }
                // result += "mov rbp, rdi\n";

                result += "mov rsp, rbp\n";
                result += "pop rbp\n";

                symbol_map.pop_front();
                result
            },
            Expr::Eval(symbol) => {
                // TODO: figure out what offset the symbol is in the current stack frame, and load the value into rax

                // only curr scope for now
                let mut result = "; Symbol eval \n".to_owned();
                let mut offset_option = None;
                // let map = symbol_map.iter().next().expect("No current scope");
                result += "mov rax, rbp\n";

                for map in symbol_map.iter() {
                    eprintln!("Lookig for {symbol} in ");
                    eprintln!("{map:?}");
                    if let Some(entry) = map.0.iter().find(|x| x.symbol.eq(&symbol)) {
                        eprintln!("Found {symbol}");
                        offset_option = Some(entry.position);
                        break;
                    }
                    result += "mov rax, [rax]\n"; //only happens AFTER the potential break, so basically applies to next iteration, if there is no next iteration this isn't a problem since were gonna panic when unpacking the offset in the next line
                }
                let offset = offset_option.expect(&format!("Could not evaluate variable: {symbol}"));

                result += &format!("mov rax, [rax {offset:+}]\n");

                // result += &format!("mov rax, [rbp{offset:+}]\n");

                // loads the variable at offset 8 in the CURRENT scope
                // mov rax [rbp-8]

                // loads the variable at offset 8 in the PARENT scope
                // mov rax, [rbp]
                // mov rax, [rax-8]

                // -----This doesn't work, rethink it lule------
                // but why?
                // this loads the stacks in CALL order, not definition order...
                // so:
                // x: 60
                // a: ;; {b: ;; {x + 9}}
                // b()
                // would mean that when x is read in b, it was only one scope above it's current
                // as it's called straight from main, where x is defined
                // as new rbp layers are only added on function call

                // loads the variable at offset 8 in the PARENTS PARENT  scope
                // mov rax, [rbp]
                // mov rax, [rax]
                // mov rax, [rax-8]

                // generalizing (replace 8 with offset)

                // mov rax, rbp

                //  ; repeat for each level of scope
                // mov rax, [rax]

                // ; then load
                // mov rax, [rax-8]

                // -----------------------------------------------

                result
            },
            // TODO: later: figure out larger return values, see: https://stackoverflow.com/questions/24741218/how-c-compiler-treats-a-struct-return-value-from-a-function-in-asm
            Expr::Func(expr, param_symbols) => {
                let mut result = "; func\n".to_owned();
                FUNCTION_COUNTER = FUNCTION_COUNTER + 1;
                let end_tag = &format!("end_{FUNCTION_COUNTER}");
                let start_tag = &format!("begin_{FUNCTION_COUNTER}");
                result += &format!("jmp {end_tag}\n");

                // TODO: label?
                result += &format!("{start_tag}:\n");
                result += "push rbp\n";
                result += "mov rbp, rsp\n";

                // TODO: do some shit with the symbol table for fucntion params, should be at some positive offset to rbp after we move it
                // TODO: make the function code:

                let mut map: VecDeque<(VecDeque<SymbolTableEntry>, i32)> = VecDeque::new();
                let mut scope = (VecDeque::new(), 1); //start at 1 to account for the return address

                // TODO: Add params at positive offset in new scope
                for symbol in param_symbols.into_iter().rev() {
                    // TODO: store pointer location in some sort of table by symbol
                    scope.1 += 1;
                    let entry = SymbolTableEntry { symbol: symbol, position: scope.1 * 8 }; //positive 8 because params are placed before the bsp
                    scope.0.push_front(entry);
                }
                map.push_front(scope); //function scope

                // TODO:    save rbp (stack base pointer) (push rbp)
                // TODO:    (mov rbp, rsp)
                // ! result += "push rbp\nmov rbp, rsp\n";

                // TODO:    maybe backup non-volatile registers on the stack? or be smart about it somehow...

                // TODO:    evaluate the expr (stores value in rax)
                result += &compile_to_asm_rec_helper(*expr, &mut map); //use the map for the function specifically

                // TODO:    if touched non-volatile registers, restore them

                // TODO:    restore rsp (stack top pointer) (mov rsp, rbp)
                // TODO:    restore rbp (stack base pointer) (pop rbp) NOT (mov rbp, [rbp]) as that fucks rsp I think
                // result += "mov rsp, rbp\npop rbp\n";

                // TODO:    (ret)? idk, maybe just (jmp [rsp])?
                result += "mov rsp, rbp\n";
                result += "pop rbp\n";
                result += "ret\n";

                result += &format!("{end_tag}:\n");
                // TODO: somehow move the pointer to this function code into rax... again, label?
                result += &format!("lea rax, [{start_tag}]\n");
                result
                // TODO: later: figure out closures / enclosed variables
            },
            Expr::Call(func, params) => {
                let mut result = "; func call \n".to_owned();
                // TODO: move parameters on stack (or registers)
                let mut param_count = 0;

                for param in params {
                    //Reverse order I think but eh
                    result += &compile_to_asm_rec_helper(param, symbol_map);
                    result += "push rax\n";
                    param_count += 8;
                }

                // TODO: add params to new scope under name?

                // move ret addres on stack
                // jump to function
                // <- ret addr here:

                //Stolen from the symbol eval TODO: consolidate
                // result += "; Function call symbol eval \n";
                // let mut offset_option = None;
                // // let map = symbol_map.iter().next().expect("No current scope");
                // result += "mov rax, rbp\n";

                // for map in symbol_map.iter() {
                // if let Some(entry) = map.0.iter().find(|x| x.symbol.eq(&symbol)) {
                // offset_option = Some(entry.position);
                // break;
                // }
                // result += "mov rax, [rax]\n"; //only happens AFTER the potential break, so basically applies to next iteration, if there is no next iteration this isn't a problem since were gonna panic when unpacking the offset in the next line
                // }
                // let offset = offset_option.expect(&format!("Could not evaluate variable: {symbol}"));

                // result += &format!("mov rax, [rax {offset:+}]\n");
                //end stolen

                result += "; Function eval \n";
                result += &compile_to_asm_rec_helper(*func, symbol_map);
                result += "call rax\n";

                // TODO: clean params off stack

                result += &format!("add rsp, {param_count}\n");

                result
            },
        }
    }

    unsafe {
        // TODO: I am cringe
        result += &compile_to_asm_rec_helper(entry, symbol_map);
    }

    result += "
    mov rsp, rbp
    pop rbp
    mov rdi, rax ; exit code is stored in rdi
    mov rax, 60 ; linux system call for exit
    syscall ; invoke operating system to exit
    ";

    // print function
    result += "
    __builtin__print:
    push rbp
    mov rbp, rsp

    push rdi

    lea rax, [rbp+16]

    mov       rdi, 1                  ; file handle 1 is stdout
    mov       rsi, rax                ; address of string to output
    mov       rdx, 1                  ; number of bytes
    mov       rax, 1                  ; system call for write
    syscall                           ; invoke operating system to do the write

    pop rdi

    mov rsp, rbp
    pop rbp
    ret
    ";
    result
}

// struct LayeredHashMap<K, V>
// where
//     K: Eq + Hash,
// {
//     // to recurse or not to recurse
//     map_stack: VecDeque<HashMap<K, V>>,
// }

// impl<K, V> LayeredHashMap<K, V>
// where
//     K: Eq + Hash,
// {
//     fn new() -> Self {
//         Self { map_stack: VecDeque::new() }
//     }
// }

// impl<K, V> LayeredHashMap<K, V>
// where
//     K: Eq + Hash,
// {
//     fn pop_layer(&mut self) -> Option<HashMap<K, V>> {
//         self.map_stack.pop_front()
//     }

//     fn push_layer(&mut self) {
//         self.map_stack.push_front(HashMap::new())
//     }

//     fn search(&mut self, key: K) -> Option<&V> {
//         // why the fuck couldn't I figure out how to write this using find() and stuff??? BRUH 3 am moment
//         let mut iter = self.map_stack.iter();
//         while let Some(map) = iter.next() {
//             if let Some(value) = map.get(&key) {
//                 return Some(value);
//             }
//         }
//         None
//     }

//     fn insert(&mut self, key: K, value: V) -> Option<()> {
//         let mut iter = self.map_stack.iter_mut();
//         match iter.next() {
//             Some(map) => {
//                 map.insert(key, value);
//                 Some(())
//             },
//             None => None,
//         }
//     }
// }

#[derive(Debug)]
struct SymbolTableEntry {
    symbol: String,
    position: i32,
}

struct CmdArgs {
    debug: bool,
    out_path: Option<String>,
    src_path: String,
}

impl CmdArgs {
    fn parse(mut arg_list: std::env::Args) -> CmdArgs {
        let mut debug = false;
        let mut out_path = None;
        let mut src_path = None;

        while let Some(arg) = arg_list.next() {
            match &arg[..] {
                "-d" => debug = true,
                "-o" => out_path = Some(arg_list.next().expect("provide output location after -o flag like: -o ./foo.asm")),
                path => src_path = Some(path.to_string()),
            }
        }

        CmdArgs { debug, out_path, src_path: src_path.expect("Please provide a src file") }
    }
}

#[allow(dead_code, unused, unused_mut)]
fn main() {
    let cmd_args = CmdArgs::parse(args());

    let mut text = CharProvider { position: 0, data: fs::read_to_string(&cmd_args.src_path).expect(&format!("Unable to read file: {}\n\t", &cmd_args.src_path)) };

    let mut lexer = LoggingIter::new(TokenProvider::new(&mut text), cmd_args.debug);
    let mut ast = parse(lexer).expect("Empty ast");
    eprintln!("AST:\n{ast:?}\n");
    let mut symbol_stack: VecDeque<(VecDeque<SymbolTableEntry>, i32)> = VecDeque::new();
    symbol_stack.push_front((VecDeque::new(), 0)); //global scope
    let asm = compile_to_asm(ast, &mut symbol_stack);

    match cmd_args.out_path {
        Some(path) => File::create(&path).expect(&format!("unable to access output file: {path}")).write_all(asm.as_bytes()),
        None => stdout().write_all(asm.as_bytes()),
    };
}
