
 --------------------------------------
 --------------------------------------
 ------DEFINITION DES ALGEBRE----------
 --------------------------------------
 --------------------------------------
data Expr a  = Val a 
			|Add (Expr a)  (Expr a) 
			|Mult (Expr a)  (Expr a) 
			 deriving Show

data AlgExpr a = AlgExpr {
        	fval :: a->a,
			fadd :: a->a->a,
			fmult :: a->a->a
		}
interExpr :: AlgExpr a -> Expr a  -> a
interExpr alg  = f where
 	f (Val n)      =fval alg n
 	f (Add e1 e2 ) =fadd alg (f e1) (f e2)
 	f (Mult e1 e2) = fmult alg (f e1) (f e2)


 -- fval:: AlgExpr b -> Int -> b
 -- fadd :: AlgExpr b -> b -> b -> b
 -- fmult :: AlgExpr b -> b -> b -> b


data List a=Nil | Const a (List a)
data Alglist a= Alglist{
			
	
}