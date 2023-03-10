	 
data Expr = Val Int
			|Add Expr Expr
			|Mul Expr Expr
			 deriving Show

data List a = Nill | Const a (List a)

--a) Donner une fonction Tolist::List a qui...
toList :: List b -> [b]
toList Nill = []
toList (Const x xs) = x:(toList xs)

--b)
fmap1 :: (a->b)->List a -> List b
fmap1  f Nill = Nill
fmap1 f (Const a xs) = Const (f a)(fmap1 f xs)

--c) foldlist
foldlist :: (a->b)->c->(b->c->c)->List a ->c

