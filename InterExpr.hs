 data Expr = Val Int
 			|Add Expr Expr
 			|Mul Expr Expr
 			 deriving Show
 			 
 size :: Expr->Int
 size (Val n) = 1
 size (Add x y) = (size x) + (size y) + 1
 size (Mul x y) = (size x) + (size y) + 1
 
 ts = (Mul (Val 2) (Val 3))


 value :: Expr->Int
 value (Val n)   = n
 value (Add x y) = (value x) + (value y) 
 value (Mul x y) = (value x) * (value y)

 ts1 = (Mul (Val 2) (Val 3))


-- FONCTION D'INTERPRETATION de calcul des valeurs

 interpret :: (Int -> b) -> (b -> b -> b) -> (b -> b -> b) -> Expr -> b
 interpret fval fadd fmul       = f where
 					f (Val n)   =  (fval n)
 					f (Add x y) = (fadd l1 l2 ) where

 					    l2= (f y)
 					    l1= (f x)

 					f (Mul s v) = (fmul p1 p2 ) where

 					     p2= (f v)
 					     p1= (f s)

 som e = interpret(\x->x) (+) (*) e

------------------------------------------
--celui ci evalut les operandes ----------
------------------------------------------

 sizeet e = interpret (\x->1) (+) (+) e --
 -----------------------------------------

------------------------------------------

-------------------------------------
 -- size a partir de interprete------
 --taille totale de l'ex[ression-----
 ------------------------------------

 sizee  = interpret val_ add_ mul_ where
 	  	 
 val_ = (\e1  ->  1)
  	  	 
 add_ = (\e1 e2 -> e1 + e2 + 1)
  	  	 
 mul_ = (\e1 e2 -> e1 + e2 + 1)


 


 