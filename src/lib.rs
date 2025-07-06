use std::{iter::Peekable, str::Chars};

#[derive(Debug, PartialEq)]
pub enum Token {
    LParen,
    RParen,
    Arrow,  // ->
    Lambda, // \ or λ
    Equal,
    Ident(String),
    Number(f64),
    Bool(bool),
    Let,
    In,
}

pub struct Lexer<'a> {
    chars: Peekable<Chars<'a>>,
}

impl<'a> Lexer<'a> {
    pub fn new(source: &'a str) -> Self {
        Self {
            chars: source.chars().peekable(),
        }
    }
    pub fn tokenize(mut self) -> Vec<Token> {
        let mut tokens = vec![];

        while let Some(ch) = self.chars.peek() {
            match ch {
                ' ' | '\t' | '\n' | '\r' => {}
                '(' => tokens.push(Token::LParen),
                ')' => tokens.push(Token::RParen),
                '\\' | 'λ' => tokens.push(Token::Lambda),
                '-' => {
                    self.chars.next();
                    if !self.next_match('>') {
                        panic!("expected '>' after '-' to form arrow");
                    }
                    tokens.push(Token::Arrow);
                }
                '=' => tokens.push(Token::Equal),
                _ if ch.is_ascii_digit() => {
                    tokens.push(Token::Number(self.read_number()));
                    continue;
                }
                _ if ch.is_alphabetic() || *ch == '_' => {
                    let ident = self.read_ident();
                    match &ident[..] {
                        "in" => tokens.push(Token::In),
                        "let" => tokens.push(Token::Let),
                        "true" => tokens.push(Token::Bool(true)),
                        "false" => tokens.push(Token::Bool(false)),
                        _ => tokens.push(Token::Ident(ident)),
                    }
                    continue;
                }
                _ => panic!("unexpected char {ch}"),
            }
            self.chars.next();
        }

        tokens
    }
    fn next_match(&mut self, ch: char) -> bool {
        matches!(self.chars.peek(), Some(next_ch) if *next_ch == ch)
    }
    fn read_number(&mut self) -> f64 {
        let mut n = String::new();
        n.push(self.chars.next().unwrap());
        while let Some(next_ch) = self.chars.peek() {
            if !next_ch.is_ascii_digit() {
                break;
            }
            n.push(*next_ch);
            self.chars.next();
        }
        n.parse().expect("expected valid number")
    }
    fn read_ident(&mut self) -> String {
        let mut ident = String::new();
        ident.push(self.chars.next().unwrap());
        while let Some(next_ch) = self.chars.peek() {
            if !next_ch.is_alphabetic() && *next_ch != '_' {
                break;
            }
            ident.push(*next_ch);
            self.chars.next();
        }
        ident
    }
}

#[derive(Debug, PartialEq)]
pub enum Expr {
    Lit(Lit),
    Var(String),
    Lambda(Lambda), // "abstraction"
    App(App),
    LetIn(LetIn),
}

#[derive(Debug, PartialEq)]
pub enum Lit {
    Number(f64),
    Bool(bool),
}

#[derive(Debug, PartialEq)]
pub struct Lambda {
    param: String,
    body: Box<Expr>,
}

#[derive(Debug, PartialEq)]
pub struct App {
    lambda: Box<Expr>,
    arg: Box<Expr>,
}

#[derive(Debug, PartialEq)]
pub struct LetIn {
    var: String,
    value: Box<Expr>,
    body: Box<Expr>,
}

pub struct Parser {
    tokens: Peekable<std::vec::IntoIter<Token>>,
}

impl Parser {
    pub fn new(tokens: Vec<Token>) -> Self {
        Self {
            tokens: tokens.into_iter().peekable(),
        }
    }
    pub fn parse(&mut self) -> Expr {
        self.parse_expr()
    }
    fn parse_expr(&mut self) -> Expr {
        match self.tokens.peek() {
            Some(Token::Lambda) => Expr::Lambda(self.parse_lambda()),
            Some(Token::Let) => Expr::LetIn(self.parse_let_in()),
            _ => self.parse_app(),
            // _ => panic!("invalid token {t:?} when parsing 'expression'"),
        }
    }
    fn parse_lambda(&mut self) -> Lambda {
        self.tokens.next(); // Lambda
        let ident = match self.tokens.next() {
            Some(Token::Ident(i)) => i,
            _ => panic!("ident/param not found when parsing 'lambda'"),
        };
        match self.tokens.next() {
            Some(Token::Arrow) => {}
            _ => panic!("'->' not found when parsing 'lambda'"),
        };
        let expr = self.parse_expr();
        Lambda {
            param: ident,
            body: Box::new(expr),
        }
    }
    fn parse_let_in(&mut self) -> LetIn {
        self.tokens.next();
        let ident = match self.tokens.next() {
            Some(Token::Ident(i)) => i,
            _ => panic!("ident/var not found when parsing 'let in'"),
        };
        match self.tokens.next() {
            Some(Token::Equal) => {}
            _ => panic!("'=' not found when parsing 'let in'"),
        };
        let val_expr = self.parse_expr();
        match self.tokens.next() {
            Some(Token::In) => {}
            _ => panic!("'in' keyword not found when parsing 'let in'"),
        };
        let body_expr = self.parse_expr();
        LetIn {
            var: ident,
            value: Box::new(val_expr),
            body: Box::new(body_expr),
        }
    }
    fn parse_app(&mut self) -> Expr {
        let mut expr = self.parse_primary();

        while !matches!(
            self.tokens.peek(),
            None | Some(Token::RParen) | Some(Token::In)
        ) {
            let arg = self.parse_primary();
            expr = Expr::App(App {
                lambda: Box::new(expr),
                arg: Box::new(arg),
            })
        }

        expr
    }
    fn parse_primary(&mut self) -> Expr {
        match self.tokens.next() {
            Some(Token::Number(n)) => Expr::Lit(Lit::Number(n)),
            Some(Token::Bool(b)) => Expr::Lit(Lit::Bool(b)),
            Some(Token::Ident(i)) => Expr::Var(i),
            Some(Token::LParen) => {
                let expr = self.parse_expr();
                match self.tokens.next() {
                    Some(Token::RParen) => {}
                    _ => panic!("')' was not found when parsing expression"),
                };
                expr
            }
            Some(t) => panic!("unexpected token {t:?}"),
            None => panic!("failed to parse primary, no token was found"),
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    fn tokenize(src: &str) -> Vec<Token> {
        Lexer::new(src).tokenize()
    }
    fn parse(src: &str) -> Expr {
        Parser::new(tokenize(src)).parse()
    }

    #[test]
    fn test_tokenize() {
        use Token::*;
        assert_eq!(tokenize("3"), vec![Number(3.0),]);
        assert_eq!(tokenize("true"), vec![Bool(true),]);
        assert_eq!(
            tokenize("(\\x -> x) 3"),
            vec![
                LParen,
                Lambda,
                Ident("x".into()),
                Arrow,
                Ident("x".into()),
                RParen,
                Number(3.0)
            ]
        );
        assert_eq!(
            tokenize("(λx -> (λy -> x)) 1 2"),
            vec![
                LParen,
                Lambda,
                Ident("x".into()),
                Arrow,
                LParen,
                Lambda,
                Ident("y".into()),
                Arrow,
                Ident("x".into()),
                RParen,
                RParen,
                Number(1.0),
                Number(2.0)
            ]
        );
        assert_eq!(
            tokenize(
                r###"(λodd -> odd 3)
                            (λx -> equals (mod x 2) 1)"###
            ),
            vec![
                LParen,
                Lambda,
                Ident("odd".into()),
                Arrow,
                Ident("odd".into()),
                Number(3.0),
                RParen,
                LParen,
                Lambda,
                Ident("x".into()),
                Arrow,
                Ident("equals".into()),
                LParen,
                Ident("mod".into()),
                Ident("x".into()),
                Number(2.0),
                RParen,
                Number(1.0),
                RParen
            ]
        );
        assert_eq!(
            tokenize("(λx -> x y (λy -> y)) y"),
            vec![
                LParen,
                Lambda,
                Ident("x".into()),
                Arrow,
                Ident("x".into()),
                Ident("y".into()),
                LParen,
                Lambda,
                Ident("y".into()),
                Arrow,
                Ident("y".into()),
                RParen,
                RParen,
                Ident("y".into())
            ]
        );
        assert_eq!(
            tokenize("let x = 3 in odd x"),
            vec![
                Let,
                Ident("x".into()),
                Equal,
                Number(3.0),
                In,
                Ident("odd".into()),
                Ident("x".into())
            ]
        );
        assert_eq!(
            tokenize(
                r###"let x = (\x -> x)
                            in x (odd (x 3))"###
            ),
            vec![
                Let,
                Ident("x".into()),
                Equal,
                LParen,
                Lambda,
                Ident("x".into()),
                Arrow,
                Ident("x".into()),
                RParen,
                In,
                Ident("x".into()),
                LParen,
                Ident("odd".into()),
                LParen,
                Ident("x".into()),
                Number(3.0),
                RParen,
                RParen
            ]
        );
    }

    #[test]
    fn test_parse() {
        assert_eq!(parse("3"), Expr::Lit(Lit::Number(3.0)));
        assert_eq!(parse("false"), Expr::Lit(Lit::Bool(false)));
        assert_eq!(
            parse("(\\x -> x) 3"),
            Expr::App(App {
                lambda: Box::new(Expr::Lambda(Lambda {
                    param: "x".into(),
                    body: Box::new(Expr::Var("x".into()))
                })),
                arg: Box::new(Expr::Lit(Lit::Number(3.0)))
            })
        );
        assert_eq!(
            parse("(λx -> (λy -> x)) 1 2"),
            Expr::App(App {
                lambda: Box::new(Expr::App(App {
                    lambda: Box::new(Expr::Lambda(Lambda {
                        param: "x".into(),
                        body: Box::new(Expr::Lambda(Lambda {
                            param: "y".into(),
                            body: Box::new(Expr::Var("x".into()))
                        }))
                    })),
                    arg: Box::new(Expr::Lit(Lit::Number(1.0)))
                })),
                arg: Box::new(Expr::Lit(Lit::Number(2.0)))
            })
        );

        assert_eq!(
            parse(
                r###"(λodd -> odd 3)
                            (λx -> equals (mod x 2) 1)"###
            ),
            Expr::App(App {
                lambda: Box::new(Expr::Lambda(Lambda {
                    param: "odd".into(),
                    body: Box::new(Expr::App(App {
                        lambda: Box::new(Expr::Var("odd".into())),
                        arg: Box::new(Expr::Lit(Lit::Number(3.0)))
                    }))
                })),
                arg: Box::new(Expr::Lambda(Lambda {
                    param: "x".into(),
                    body: Box::new(Expr::App(App {
                        lambda: Box::new(Expr::App(App {
                            lambda: Box::new(Expr::Var("equals".into())),
                            arg: Box::new(Expr::App(App {
                                lambda: Box::new(Expr::App(App {
                                    lambda: Box::new(Expr::Var("mod".into())),
                                    arg: Box::new(Expr::Var("x".into()))
                                })),
                                arg: Box::new(Expr::Lit(Lit::Number(2.0)))
                            }))
                        })),
                        arg: Box::new(Expr::Lit(Lit::Number(1.0)))
                    }))
                }))
            })
        );

        assert_eq!(
            parse("(λx -> x y (λy -> y)) y"),
            Expr::App(App {
                lambda: Box::new(Expr::Lambda(Lambda {
                    param: "x".into(),
                    body: Box::new(Expr::App(App {
                        lambda: Box::new(Expr::App(App {
                            lambda: Box::new(Expr::Var("x".into())),
                            arg: Box::new(Expr::Var("y".into()))
                        })),
                        arg: Box::new(Expr::Lambda(Lambda {
                            param: "y".into(),
                            body: Box::new(Expr::Var("y".into()))
                        }))
                    }))
                })),
                arg: Box::new(Expr::Var("y".into()))
            })
        );

        assert_eq!(
            parse("let x = 3 in odd x"),
            Expr::LetIn(LetIn {
                var: "x".into(),
                value: Box::new(Expr::Lit(Lit::Number(3.0))),
                body: Box::new(Expr::App(App {
                    lambda: Box::new(Expr::Var("odd".into())),
                    arg: Box::new(Expr::Var("x".into()))
                }))
            })
        );

        assert_eq!(
            parse(
                r###"let x = (\x -> x)
                            in x (odd (x 3))"###
            ),
            Expr::LetIn(LetIn {
                var: "x".into(),
                value: Box::new(Expr::Lambda(Lambda {
                    param: "x".into(),
                    body: Box::new(Expr::Var("x".into()))
                })),
                body: Box::new(Expr::App(App {
                    lambda: Box::new(Expr::Var("x".into())),
                    arg: Box::new(Expr::App(App {
                        lambda: Box::new(Expr::Var("odd".into())),
                        arg: Box::new(Expr::App(App {
                            lambda: Box::new(Expr::Var("x".into())),
                            arg: Box::new(Expr::Lit(Lit::Number(3.0)))
                        }))
                    }))
                }))
            })
        );
    }
}
