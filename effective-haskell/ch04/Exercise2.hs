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

-- eval $ (Lit 4 `Sub` Lit 2) `Add` (Lit 2 `Mul` Lit 3)
-- 8

saveEval :: Expr -> Either String Int
saveEval expr =
  case expr of
    Lit x -> Right x
    Add a b -> saveEval' (+) a b
    Sub a b -> saveEval' (-) a b
    Mul a b -> saveEval' (*) a b
    Div a b ->
      case b of
        Lit 0 -> Left "Error: division by zero"
        _ -> saveEval' div a b
    where
      saveEval' :: (Int -> Int -> Int) -> Expr -> Expr -> Either String Int
      saveEval' op a b =
        let
          a' = saveEval a
          b' = saveEval b
        in
          case (a', b') of
            (Right x, Right y) -> Right $ op x y
            (Left e, _) -> Left e
            (_, Left e) -> Left e

-- saveEval $ (Lit 4 `Sub` Lit 2) `Add` (Lit 2 `Mul` Lit 3)
-- Right 8
-- saveEval $ Lit 10 `Div` Lit 0
-- Left "Error: division by zero"
