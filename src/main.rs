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

struct TokenProvider<'a, T>
where
    T: Iterator<Item = char>,
{
    char_provider: std::iter::Peekable<&'a mut T>,
}

impl<'a, T> TokenProvider<'a, T>
where
    T: Iterator<Item = char>,
{
    fn new(char_provider: &'a mut T) -> TokenProvider<'a, T> {
        TokenProvider { char_provider: char_provider.peekable() }
    }
}

#[derive(Debug)]
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
}

impl<'a, T> Iterator for TokenProvider<'a, T>
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
                Option::Some(Token::WhiteSpace)
            },

            Option::Some(c) if c.is_ascii_digit() => Option::Some(Token::Number(1)),

            Option::Some(c) if c.is_ascii_alphabetic() => Option::Some(Token::Symbol(c.to_string())),

            Option::None => Option::None,

            _ => Option::Some(Token::SemiColon),
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
        .to_string(),
    };

    text.data = "test this ;; ()".to_owned();
    let mut lexer = TokenProvider::new(&mut text);
    lexer.for_each(|t| println!("{:?}", t))
}
