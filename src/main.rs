use core::panic;
use std::iter::Peekable;

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

#[derive(Debug, Clone)]
enum Token {
    WhiteSpace,
    Operator(String),
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
    }"
    .to_owned();

    let mut lexer = TokenProvider::new(&mut text);

    let mut ast = parse(lexer);
    println!("{:?}", ast);
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
    Eval(String),
    BiExpr(Box<Expr>, Box<Expr>, String),
    Func(Box<Expr>, Vec<String>),
    Assign(Box<Expr>, String),
    Call(Vec<Expr>, String),
}
