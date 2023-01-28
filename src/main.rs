use core::panic;
use std::{fmt::format, iter::Peekable, result};

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
            Option::Some(c) if "+-*/&|=<>".contains(c) => Some(Token::Operator(c.to_string())),
            Option::None => Option::None,

            Option::Some(c) => panic!("Error parsing: {}", c),
        }
    }
}
#[derive(Debug, Clone)]
enum Token {
    WhiteSpace,
    Operator(String), // TODO: make an Operator enum instead of using strings lule
    ParensStart,
    ParensEnd,
    Number(i32),
    Symbol(String),
    CurlyStart,
    CurlyEnd,
    SemiColon,
    Comma,
    Colon,
    Comment,
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
                Some(Token::Operator(operator)) => {
                    tokens.next();
                    Some(Expr::BiExpr(
                        Box::new(Expr::Eval(symbol)),
                        Box::new(parse_rec(tokens).unwrap_or_else(|| panic!("Missing right side of BiExpr {operator:?}"))),
                        operator,
                    ))
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

                    match tokens.peek().cloned() {
                        Some(Token::Operator(operator)) => {
                            tokens.next();
                            Some(Expr::BiExpr(
                                Box::new(Expr::Call(params, symbol)),
                                Box::new(parse_rec(tokens).unwrap_or_else(|| panic!("Missing right side of BiExpr {operator:?}"))),
                                operator,
                            ))
                        },
                        _ => Some(Expr::Call(params, symbol)),
                    }
                },
                Some(_) => Some(Expr::Eval(symbol)),
                None => None,
            },
            //Cringe clone occurs
            Some(Token::Number(number)) => match tokens.peek().cloned() {
                Some(Token::Operator(operator)) => {
                    tokens.next();
                    Some(Expr::BiExpr(
                        Box::new(Expr::Int(number)),
                        Box::new(parse_rec(tokens).unwrap_or_else(|| panic!("Missing right side of BiExpr {operator:?}"))),
                        operator,
                    ))
                },
                _ => Some(Expr::Int(number)),
            },
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

                Some(Expr::Func(Box::new(parse_rec(tokens).expect("Function body missing")), params))
            },
            None => None,
            Some(token) => panic!("Bad token: {token:?}"),
        }
    }

    parse_rec(&mut tokens.peekable())
}

#[derive(Debug)]
enum Expr {
    MultiExpr(Vec<Expr>),
    Int(i32),
    Eval(String), // TODO: think about simmilarities between this and function (and int?)
    BiExpr(Box<Expr>, Box<Expr>, String),
    Func(Box<Expr>, Vec<String>),
    Assign(Box<Expr>, String),
    Call(Vec<Expr>, String),
}

fn compile_to_asm(entry: Expr) -> String {
    // TODO: better way to build strings pepela

    let mut result = "
    ;compiled with rust lang lang compiler

        global    _start

        section   .text
    _start:
        
    mov rdi, rbp ; save old stack base pointer in rbi, by my convention rbi is preserved after operations
    mov rbp, rsp ; move rbp to the to of the current stack, as anything before that is unrelated to our program, don't think this should actually do anything\n"
        .to_owned();

    fn compile_to_asm_rec_helper(entry: Expr) -> String {
        match entry {
            Expr::Assign(expr, symbol) => {
                // TODO: evaluate expr
                let mut result = compile_to_asm_rec_helper(*expr);

                // TODO: store at pointer location
                result += "push rax\n";

                // TODO: store pointer location in some sort of table by symbol
                //table[sybol] = "[stack + 8]";

                // TODO: advance stack by size of expr return value (always int so far)
                // result += "

                // ";

                // TODO: make sure designated eval return register is set to the value (I think it shoul dbe already as that's the last thing we evaled but :shrug: make sure)

                result
            },
            Expr::Int(num) => {
                format!(";Number\nmov rax, {num }\n")
            },
            Expr::BiExpr(expr_1, expr_2, operator) => match operator {
                o if "+".contains(o.chars().next().unwrap()) => {
                    //* Sets RAX to the result, alters RBX, MAYBE BAD!

                    let mut result = ";Addition\n".to_owned();
                    result += "push rdi\n"; //cache rdi on stack
                    result += &compile_to_asm_rec_helper(*expr_1);
                    result += "mov rdi, rax\n"; //move rdi (result of expr_1) into rax
                    result += &compile_to_asm_rec_helper(*expr_2);
                    result += "add rax, rdi\n"; //add them and store in rax
                    result += "pop rdi\n"; //restore rdi
                    result
                },
                _ => ";TODO!".to_owned(),
            },
            Expr::MultiExpr(exprs) => {
                let mut result = ";MultiExpr\n".to_owned();
                for expr in exprs {
                    result += &compile_to_asm_rec_helper(expr);
                }
                result
            },
            Expr::Eval(symbol) => {
                // TODO: figure out what offset the symbol is in the current stack frame, and load the value into rax
                "TODO!!".to_owned()
            },
            _ => "\nTODO!\n".to_owned(),

            // TODO: later: figure out larger return values, see: https://stackoverflow.com/questions/24741218/how-c-compiler-treats-a-struct-return-value-from-a-function-in-asm
            Expr::Func(expr, symbol) => {
                // TODO: label?
                // TODO: do some shit with the symbol table for fucntion params, should eb at some positive offset to rbp after we move it
                // TODO: make the function code:
                // TODO:    save rbp (stack base pointer) (push rbp)
                // TODO:    (mov rbp, rsp)
                // TODO:    maybe backup non-volatile registers on the stack? or be smart about it somehow...
                // TODO:    evaluate the expr (stores value in rax)
                // TODO:    if touched non-volatile registers, restore them
                // TODO:    restore rsp (stack top pointer) (mov rsp, rbp)
                // TODO:    restore rbp (stack base pointer) (pop rbp) NOT (mov rbp, [rbp]) as that fucks rsp I think
                // TODO:    (ret)? idk, maybe just (jmp [rsp])?
                // TODO: somehow move the pointer to this function code into rax... again, label?

                todo!();
                // TODO: later: figure out closures / enclosed variables
            },
            Expr::Call(exprs, symbol) => todo!(),
        }
    }

    result += &compile_to_asm_rec_helper(entry);

    result += "
    mov rbp, rdi ; restore rbp from the value saved in rdi, again this is onyl usefull if the entire program is being invoked as a function or something idk... felt nice to include
    mov rdi, rax ; exit code is stored in rdi
    mov rax, 60 ; linux system call for exit
    syscall ; invoke operating system to exit
    ";
    result
}

#[allow(dead_code, unused, unused_mut)]
fn main() {
    let mut text = CharProvider {
        position: 0,
        data: "
              {
                  # My giga scuffed standard library LULE #
                  
                  _:;x;{x} # identity function since we can't use parenthesis in expressions #
                  
                  printDigit: ;num; print(48+num)
                  
                  newline: ;; print(10)
                  
                  if: ;bool action; { res:0 bool & res:action() res } # if \"statement\" using short circuit evaluation. Returns 0 if it didn't run, otherwise return the result of the action #
                  ifElse: ;bool ifAction elseAction; { res:elseAction bool & { res:ifAction 1 } res() } 
                  
                  !: ;bool; bool = 0
                  
                  loopWithBase: ;predicate action base; { # Takes a predicate acting on the loop count, and an action acting on the loop count to perform while the predicate evaluates true #
                      
                      recusiveHelper: ;count prev; {
                          
                          ifElse(predicate(count + 1) ;;recusiveHelper(count + 1 action(count prev)) ;;action(count prev))
                          
                      }
                      
                      ifElse(predicate(0) ;;recusiveHelper(0 base) ;;base )
                  }
                  
                  pow: ;base exponent; loopWithBase(;i; {i < exponent + 1} ;i res; {res * base} 1)/base
                  
                  printNum: ;x; { # xd no shot I figure out how to do this with the loop func, garbage lang #
                      
                      recusiveHelper: ;i prev; {
                          if(_(prev/10) > 0 ;;recusiveHelper(i + 1 prev/10))
                          printDigit(prev - _( _(prev/10) * 10))                                           
                      }
                      
                      if(x > 0 ;;recusiveHelper(0 x))
                  }
              
                  # stdlib end #
                  
                  newline()
                  printNum(pow(3 10))
                  newline()
              }
              "
        .to_owned(),
    };

    text.data = "
{
    var_a: 2 + 60 
    7 + 9 + 18+ 6 + 5+ 12
}
    "
    .to_owned();

    let mut lexer = TokenProvider::new(&mut text);
    let mut ast = parse(lexer).expect("Empty ast");
    println!("AST:\n{ast:?}\n");
    let mut asm = compile_to_asm(ast);
    println!("ASM:\n{asm}\n");
}
