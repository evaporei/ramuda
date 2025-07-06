use std::{collections::HashMap, fmt, iter::Peekable, str::Chars};

#[derive(Debug, PartialEq)]
pub enum Token {
    LParen,
    RParen,
    Arrow,  // ->
    Lambda, // \ or λ
    Equal,
    Ident(String),
    String(String),
    Number(i64),
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
                    match self.chars.peek() {
                        Some('>') => tokens.push(Token::Arrow),
                        Some(next_ch) if next_ch.is_ascii_digit() => {
                            tokens.push(Token::Number(-self.read_number()));
                            continue;
                        }
                        _ => panic!("expected '>' after '-' to form arrow"),
                    }
                }
                '=' => tokens.push(Token::Equal),
                '"' => {
                    tokens.push(Token::String(self.read_string()));
                    continue;
                }
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
    fn read_string(&mut self) -> String {
        self.chars.next();
        let mut s = String::new();
        while let Some(next_ch) = self.chars.peek() {
            match next_ch {
                '"' => {
                    self.chars.next();
                    break;
                }
                '\\' => {
                    self.chars.next();
                    if let Some(escaped) = self.chars.peek() {
                        match escaped {
                            'n' => s.push('\n'),
                            't' => s.push('\t'),
                            'r' => s.push('\r'),
                            '\\' => s.push('\\'),
                            '"' => s.push('"'),
                            _ => {
                                s.push('\\');
                                s.push(*escaped);
                            }
                        }
                    }
                }
                _ => {
                    s.push(*next_ch);
                }
            }
            self.chars.next();
        }
        s
    }
    fn read_number(&mut self) -> i64 {
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
    String(String),
    Number(i64),
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
            Some(Token::String(s)) => Expr::Lit(Lit::String(s)),
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

// Hindley-Milner
#[derive(Debug, PartialEq, Clone)]
pub enum Type {
    Int,
    String,
    Bool,
    Func(FuncTy),
    TypeVar(String),
}

#[derive(Debug, PartialEq, Clone)]
pub struct FuncTy {
    arg: Box<Type>,
    ret: Box<Type>,
}

impl fmt::Display for Type {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Self::Int => write!(f, "Int"),
            Self::String => write!(f, "String"),
            Self::Bool => write!(f, "Bool"),
            Self::Func(func) => write!(f, "({} -> {})", func.arg, func.ret),
            Self::TypeVar(name) => write!(f, "{}", name),
        }
    }
}

#[derive(Debug, PartialEq)]
pub enum TypeError {
    UnboundVar(UnboundVarError),
    Unification(UnificationError),
    OccursCheck(OccursCheckError),
}

#[derive(Debug, PartialEq)]
pub struct UnboundVarError {
    var: String,
}

#[derive(Debug, PartialEq)]
pub struct UnificationError {
    ty1: Type,
    ty2: Type,
}

#[derive(Debug, PartialEq)]
pub struct OccursCheckError {
    var: String,
    ty: Type,
}

impl std::fmt::Display for TypeError {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        match self {
            Self::UnboundVar(err) => write!(f, "Unbound variable: {}", err.var),
            Self::Unification(err) => write!(f, "Cannot unify {} with {}", err.ty1, err.ty2),
            Self::OccursCheck(err) => write!(f, "Occurs check failed: {} in {}", err.var, err.ty),
        }
    }
}

impl std::error::Error for TypeError {}

#[derive(Default)]
pub struct TypeInferencer {
    var_count: usize,
}

pub type Substitution = HashMap<String, Type>;
pub type Context = HashMap<String, Type>;

impl TypeInferencer {
    pub fn new() -> Self {
        Self::default()
    }
    pub fn infer(&mut self, ctx: &Context, expr: &Expr) -> Result<(Type, Substitution), TypeError> {
        match expr {
            Expr::Lit(lit) => {
                let ty = match lit {
                    Lit::String(_) => Type::String,
                    Lit::Number(_) => Type::Int,
                    Lit::Bool(_) => Type::Bool,
                };
                Ok((ty, HashMap::new()))
            }
            Expr::Var(var) => {
                let ty = match ctx.get(var) {
                    Some(t) => t.clone(),
                    None => {
                        return Err(TypeError::UnboundVar(UnboundVarError { var: var.clone() }));
                    }
                };
                Ok((ty, HashMap::new()))
            }
            Expr::Lambda(Lambda { param, body }) => {
                let param_ty = self.new_type_var();
                let mut new_ctx = ctx.clone();
                new_ctx.insert(param.clone(), param_ty.clone());

                let (body_ty, subst) = self.infer(&new_ctx, body)?;
                let param_ty_subst = self.apply_subst(&subst, &param_ty);
                Ok((
                    Type::Func(FuncTy {
                        arg: Box::new(param_ty_subst),
                        ret: Box::new(body_ty),
                    }),
                    subst,
                ))
            }
            Expr::App(App { lambda, arg }) => {
                let (lambda_ty, subst1) = self.infer(ctx, lambda)?;
                let ctx1 = self.apply_subst_ctx(&subst1, ctx);
                let (arg_ty, subst2) = self.infer(&ctx1, arg)?;

                let result_ty = self.new_type_var();
                let lambda_ty_subst = self.apply_subst(&subst2, &lambda_ty);
                let expected_lambda_ty = Type::Func(FuncTy {
                    arg: Box::new(arg_ty),
                    ret: Box::new(result_ty.clone()),
                });

                let subst3 = self.unify(&lambda_ty_subst, &expected_lambda_ty)?;
                let final_subst =
                    self.compose_subst(&subst3, &self.compose_subst(&subst2, &subst1));
                let final_result_ty = self.apply_subst(&subst3, &result_ty);

                Ok((final_result_ty, final_subst))
            }
            Expr::LetIn(LetIn { var, value, body }) => {
                let (value_ty, subst1) = self.infer(ctx, value)?;
                let mut new_env = self.apply_subst_ctx(&subst1, ctx);
                new_env.insert(var.clone(), value_ty);
                let (body_type, s2) = self.infer(&new_env, body)?;

                Ok((body_type, self.compose_subst(&s2, &subst1)))
            }
        }
    }
    pub fn apply_subst(&self, subst: &Substitution, ty: &Type) -> Type {
        match ty {
            Type::TypeVar(name) => match subst.get(name) {
                Some(replacement) => self.apply_subst(subst, replacement),
                None => ty.clone(),
            },
            Type::Func(FuncTy { arg, ret }) => Type::Func(FuncTy {
                arg: Box::new(self.apply_subst(subst, arg)),
                ret: Box::new(self.apply_subst(subst, ret)),
            }),
            _ => ty.clone(),
        }
    }
    fn apply_subst_ctx(&self, subst: &Substitution, ctx: &Context) -> Context {
        ctx.iter()
            .map(|(k, v)| (k.clone(), self.apply_subst(subst, v)))
            .collect()
    }
    fn new_type_var(&mut self) -> Type {
        let name = format!("t{}", self.var_count);
        self.var_count += 1;
        Type::TypeVar(name)
    }
    fn unify(&self, ty1: &Type, ty2: &Type) -> Result<Substitution, TypeError> {
        use Type::*;
        match (ty1, ty2) {
            (Int, Int) | (String, String) | (Bool, Bool) => Ok(HashMap::new()),
            (TypeVar(var), ty) | (ty, TypeVar(var)) => {
                if TypeVar(var.clone()) == *ty {
                    Ok(HashMap::new())
                } else if self.occurs_check(var, ty) {
                    Err(TypeError::OccursCheck(OccursCheckError {
                        var: var.clone(),
                        ty: ty.clone(),
                    }))
                } else {
                    let mut subst = HashMap::new();
                    subst.insert(var.clone(), ty.clone());
                    Ok(subst)
                }
            }
            (
                Func(FuncTy {
                    arg: arg1,
                    ret: ret1,
                }),
                Func(FuncTy {
                    arg: arg2,
                    ret: ret2,
                }),
            ) => {
                let subst1 = self.unify(arg1, arg2)?;
                let ret1_subst = self.apply_subst(&subst1, ret1);
                let ret2_subst = self.apply_subst(&subst1, ret2);
                let subst2 = self.unify(&ret1_subst, &ret2_subst)?;
                Ok(self.compose_subst(&subst2, &subst1))
            }
            _ => Err(TypeError::Unification(UnificationError {
                ty1: ty1.clone(),
                ty2: ty2.clone(),
            })),
        }
    }
    fn occurs_check(&self, var: &str, ty: &Type) -> bool {
        match ty {
            Type::TypeVar(name) => var == name,
            Type::Func(FuncTy { arg, ret }) => {
                self.occurs_check(var, arg) || self.occurs_check(var, ret)
            }
            _ => false,
        }
    }
    fn compose_subst(&self, subst1: &Substitution, subst2: &Substitution) -> Substitution {
        let mut result = subst1.clone();
        for (k, v) in subst2 {
            result.insert(k.clone(), self.apply_subst(subst1, v));
        }
        result
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
    fn infer_type(expr: &Expr) -> Result<Type, TypeError> {
        let mut inferencer = TypeInferencer::new();
        let env = HashMap::new();
        let (ty, subst) = inferencer.infer(&env, expr)?;
        Ok(inferencer.apply_subst(&subst, &ty))
    }

    #[test]
    fn test_tokenize() {
        use Token::*;
        assert_eq!(tokenize("3"), vec![Number(3),]);
        assert_eq!(tokenize("-3"), vec![Number(-3),]);
        assert_eq!(tokenize("true"), vec![Bool(true),]);
        assert_eq!(tokenize("\"bananas!\""), vec![String("bananas!".into()),]);
        assert_eq!(
            tokenize("(\\x -> x) 3"),
            vec![
                LParen,
                Lambda,
                Ident("x".into()),
                Arrow,
                Ident("x".into()),
                RParen,
                Number(3)
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
                Number(1),
                Number(2)
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
                Number(3),
                RParen,
                LParen,
                Lambda,
                Ident("x".into()),
                Arrow,
                Ident("equals".into()),
                LParen,
                Ident("mod".into()),
                Ident("x".into()),
                Number(2),
                RParen,
                Number(1),
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
                Number(3),
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
                Number(3),
                RParen,
                RParen
            ]
        );
    }

    #[test]
    fn test_parse() {
        assert_eq!(parse("3"), Expr::Lit(Lit::Number(3)));
        assert_eq!(parse("-3"), Expr::Lit(Lit::Number(-3)));
        assert_eq!(parse("false"), Expr::Lit(Lit::Bool(false)));
        assert_eq!(
            parse("\"bananas!\""),
            Expr::Lit(Lit::String("bananas!".into()))
        );
        assert_eq!(
            parse("(\\x -> x) 3"),
            Expr::App(App {
                lambda: Box::new(Expr::Lambda(Lambda {
                    param: "x".into(),
                    body: Box::new(Expr::Var("x".into()))
                })),
                arg: Box::new(Expr::Lit(Lit::Number(3)))
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
                    arg: Box::new(Expr::Lit(Lit::Number(1)))
                })),
                arg: Box::new(Expr::Lit(Lit::Number(2)))
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
                        arg: Box::new(Expr::Lit(Lit::Number(3)))
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
                                arg: Box::new(Expr::Lit(Lit::Number(2)))
                            }))
                        })),
                        arg: Box::new(Expr::Lit(Lit::Number(1)))
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
                value: Box::new(Expr::Lit(Lit::Number(3))),
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
                            arg: Box::new(Expr::Lit(Lit::Number(3)))
                        }))
                    }))
                }))
            })
        );
    }

    #[test]
    fn test_type_inference() {
        // literals
        assert_eq!(infer_type(&Expr::Lit(Lit::Number(42))).unwrap(), Type::Int);
        assert_eq!(infer_type(&Expr::Lit(Lit::Bool(true))).unwrap(), Type::Bool);
        assert_eq!(
            infer_type(&Expr::Lit(Lit::String("hello".into()))).unwrap(),
            Type::String
        );

        // lambda
        let identity = Expr::Lambda(Lambda {
            param: "x".into(),
            body: Box::new(Expr::Var("x".into())),
        });
        let ty = infer_type(&identity).unwrap();

        // λx -> x should have type t0 -> t0
        if let Type::Func(FuncTy { arg, ret }) = ty {
            assert_eq!(arg, ret);
        } else {
            panic!("Expected function type");
        }

        // application
        let identity = Expr::Lambda(Lambda {
            param: "x".into(),
            body: Box::new(Expr::Var("x".into())),
        });
        let app = Expr::App(App {
            lambda: Box::new(identity),
            arg: Box::new(Expr::Lit(Lit::Number(42))),
        });

        // (λx -> x) 42 should have type Int
        assert_eq!(infer_type(&app).unwrap(), Type::Int);

        // let in
        let expr = Expr::LetIn(LetIn {
            var: "x".into(),
            value: Box::new(Expr::Lit(Lit::Number(5))),
            body: Box::new(Expr::Var("x".into())),
        });
        // let x = 5 in x should have type Int
        assert_eq!(infer_type(&expr).unwrap(), Type::Int);

        // complex
        let identity = Expr::Lambda(Lambda {
            param: "x".into(),
            body: Box::new(Expr::Var("x".into())),
        });
        let expr = Expr::LetIn(LetIn {
            var: "f".into(),
            value: Box::new(identity),
            body: Box::new(Expr::App(App {
                lambda: Box::new(Expr::Var("f".into())),
                arg: Box::new(Expr::Lit(Lit::Number(42))),
            })),
        });
        // let f = λx.x in f 42 should have type Int
        assert_eq!(infer_type(&expr).unwrap(), Type::Int);
    }
}
