data Expr a = Val a
                | Mult (Expr a) (Expr a)
                | Add (Expr a) (Expr a)

data List a = Nil | Const a (List a)

data AlgExpr a = AlgExpr {
    fVal  ::  a -> a,
    fMult :: a -> a -> a,
    fAdd  ::  a -> a -> a
}



sizeExpr :: Expr a -> Int
sizeExpr (Val x)    = 1
sizeExpr (Mult x y) = 1 + (sizeExpr x) + (sizeExpr y)
sizeExpr (Add x y)  = 1 + (sizeExpr x) + (sizeExpr y)


valueExpr ::(Num a) => Expr a -> a
valueExpr (Val x)    = x
valueExpr (Add x y)  = (valueExpr x) + (valueExpr y)
valueExpr (Mult x y) = (valueExpr x) * (valueExpr y)

-- val :: a -> Expr a

-- mult :: Expr a -> Expr a -> Expr a

-- add :: Expr a -> Expr a -> Expr a

foldExpr :: (a->a) -> (a->a->a) -> (a->a->a) -> Expr a -> a
foldExpr fVal fMult fAdd = f where
                       f (Val x)    = fVal x
                       f (Mult x y) = fMult (f x) (f y)
                       f (Add x y)  = fAdd (f x) (f y)

exp1 = (Mult (Val 3) (Val 4))

expr2 = foldExpr (\x -> x) (*) (+) exp1

-- Les algebres

foldExprAlg :: AlgExpr a -> Expr a -> a
foldExprAlg alg = f where
                  f (Val x)      = fVal alg x
                  f (Mult e1 e2) = fMult alg (f e1) (f e2)
                  f (Add e1 e2)  = fAdd alg (f e1) (f e2)

