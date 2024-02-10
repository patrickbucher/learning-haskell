data Expr = Lit Int
  | Add Expr Expr
  | Sub Expr Expr
  | Mul Expr Expr
  | Div Expr Expr

eval :: Expr -> Int
eval expr =
  case expr of
    Lit x -> x
    Add a b -> (eval a) + (eval b)
    Sub a b -> (eval a) - (eval b)
    Mul a b -> (eval a) * (eval b)
    Div a b -> (eval a) `div` (eval b)

prettyPrint :: Expr -> String
prettyPrint expr =
  let
    pp :: String -> Int -> Expr -> Expr -> String
    pp op n l r =
      let
        m = n + 1
        output = prettyPrint' l m <> op <> prettyPrint' r m
      in
        if n > 0
        then "(" <> output  <> ")"
        else output

    prettyPrint' :: Expr -> Int -> String
    prettyPrint' expr n = case expr of
      Lit x -> show x
      Add a b -> pp " + " n a b
      Sub a b -> pp " - " n a b
      Mul a b -> pp " x " n a b
      Div a b -> pp " : " n a b

    result = show $ eval expr
    computation = prettyPrint' expr 0
  in
    computation <> " = " <> result

-- prettyPrint $ Lit 5 `Add` Lit 10
-- 5 + 10 = 15
-- prettyPrint $ Lit 5 `Add` (Lit 10 `Div` Lit 2)
-- 5 + (10 : 2) = 10
-- prettyPrint $ Lit 14 `Mul` (Lit 5 `Add` (Lit 10 `Div` Lit 2))
-- 14 x (5 + (10 : 2)) = 140
