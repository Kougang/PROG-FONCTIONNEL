


-- produit::(Int,Int)->(Int,Int)->Int
-- produit (a,b) (c,d)=a*c+b*d

-- square :: Integer->Integer
-- square  x=x*x

-- affiche::(Int,Int)->(Int,Int)->Char
-- affiche (a,b) (c,d) = ""

-- smallerc:: Integer->Integer->Integer
-- smallerc x y = if x <= y then x else y

-- twice::(Integer->Integer)->Integer->Integer
-- twice f x = f (f x)

-- fact :: Integer->Integer
-- fact n 
-- 	|n==0 = 1
-- 	|n>0  = n*fact(n-1)


-- f:: Integer->Integer->Integer
-- f x y 
-- 	|x<=10 = x+a
-- 	|x>10 = x-a
-- 	where a=square(y+1)

	--TYPE POLYMORPHE NON FOCTIONNEL--

-- (e):: Num a=>a->a->a
-- (e) x y = x+y

-- sm:: Integer->Integer->Integer
-- smallerc x y = x e y

-------------------------------------------------------------------------------------
	--monade--
-------------------------------------------------------------------------------------
 g :: Int -> Int -> Int
 g x y = x+y

 g1 :: Int -> Int -> Bool
 g1 x y = if x/=y then True else False

 h :: Int -> Int -> Int
 h x y = x*y
	-- f :: Int -> Int -> Int -- par exemple
	-- f x y = (g x (x+y)) + (h (y*x) y) -- où g et h sont des fonctions cohérentes

--la fonction ci dessous est equivalente a la fnction ci dessus

 foct ::   Int -> Int -> Int
 foct x y = let resultatH = h (y*x) y 
		        -- resultatG = g x (x+y) 

		    in
			   g x (x+y) + resultatH



------------------------------

------------------------------
----LES TYPES SIMPLE-----
------------------------------

------------------------------









-------------------------------------------

-------------------------------------------
--2.2 LA DEFINITION PAR CONSTRUCTEUR------
-------------------------------------------

-------------------------------------------



-------------------------------------------

-------------------------------------------
--       DEFINITION DES TYPES        ------
-------------------------------------------

-------------------------------------------

-- type Position = (Integer,Integer)

		

-- 	-- left :: Position
-- 	-- left  =  (0,0)

-- 	-- origin::Position
-- 	-- origin = (0,0)

-- -- Pair ici est un type constructeur ci dessous il permet de dedoubler un element en pair

-- type Pair a = (a,a)

-- mult :: Pair Int->Int
-- mult (m,n) = m*n

-- copy :: a -> Pair a
-- copy x = (x,x)

-- -- LES TYPES PEUVENT ETRE IMBRIQUÉS

-- type Trans = Position -> Position

-- essai :: Trans
-- essai (x,y) = (x+1,y+3)

-------------------------------------------

-------------------------------------------
--       STRUCTURE DE DONNÉES        ------
-------------------------------------------

-------------------------------------------


 -- true ete false ici sont des constructeurs de données d'arité 0
--  module DataType (PLaceholder)
-- where 

	-- main::IO()
	-- main = return()

	-----------------------------
	-----------------------------
	-----------------------------


 data Answer = Yes|No|Unknown deriving Show

 answers :: [Answer]
 answers = [Yes,No,Unknown]
 	

 flip1    :: Answer->Answer
 flip1 Yes     = No
 flip1 No      = Yes
 flip1 Unknown = Unknown


--------------------------------
--------------------------------
--des donnees avec des ---------
--fonctions partielles----------
--------------------------------
--------------------------------

 -- import Data.Maybe
   
 type Radius  = Float
 type Side    = Float

 data Shape    = Circle Radius
				|Rect Side Side
					 deriving Show


 radius :: Shape->Radius
 radius (Circle r) = r

 depth,height      :: Shape->Side
 depth (Rect d h)  = d
 height (Rect d h) = h

	
--------------------------------
--------------------------------
--des donnees avec des ---------
--paramstres          ----------
--------------------------------
--------------------------------

 data MyMaybe a = MyNothing
 			   |MyJust a
 			   deriving Show


 safediv :: Int->Int->MyMaybe Int
 safediv 0 0 = MyNothing
 safediv m n = MyJust(m `div` n)

 safehead :: [a]->MyMaybe a
 safehead [] = MyNothing
 safehead xs = MyJust (head xs)

--------------------------------
--------------------------------
--des donnees pour les ---------
--  entiers naturel   ----------
----reccursifs       -----------
--------------------------------

 data Nat = Zero
		   |Succ Nat
		   deriving Show

 nat2int :: Nat->Int
 nat2int Zero     = 0
 nat2int (Succ n) = 1 + nat2int n


 -- int2nat :: Int->Nat
 -- int2nat 0       = Zero
 -- int2nat (n+1) = Succ (int2nat n)

--------------------------------
--------------------------------
--addition en conversion--------
--  inverse           ----------
----                 -----------
--------------------------------

 -- add :: Nat->Nat->Nat
 -- add m n = int2nat (nat2int m + nat2int n)

--------------------------------
--------------------------------
--en utilisant la reccurssion---
--  ---------         ----------
----                 -----------
--------------------------------

 -- add Zero  n    = n
 -- add (Succ m) n = Succ (add m n)

--------------------------------
--------------------------------
--Expression Arithmetique ------
--  ---------------   ----------
---------------      -----------
--------------------------------

 data Expr = Val Int
 			|Add Expr Expr
 			|Mul Expr Expr
 			 deriving Show

 size :: Expr->Int
 size (Val n) = 1
 size (Add x y) = size x + size y + 1
 size (Mul x y) = size x + size y + 1

 -- value :: Expr->Int
 -- value (Val n)   = n
 -- value (Add x y) = (value x) + (value y) 
 -- value (Mul x y) = (value x) * (value y)

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



--------------------------------
--------------------------------
--ALGEBRE A PARAMETre     ------
--  ---------------   ----------
--reprise des exos precc    ----
--------------------------------

 data AlgExpr b  = AlgExpr  {
 	val :: Int->b, 
 	add :: b->b->b, 
 	mul :: b->b->b
  } --deriving Show


 
 interExpr :: AlgExpr b  -> Expr -> b
 interExpr alg     = ff where
 	ff (Val n)     = (val alg) n
 	ff (Add e1 e2) = ((add alg)  s1 s2) where
			 	s1 = (ff e1)
			 	s2 = (ff e2)
 	ff (Mul e1 e2) = ((mul alg)  v1 v2) where
			 	v1 = (ff e1)
			 	v2 = (ff e2)

-- sm e =  interExpr
 -- sm e = interExpr (\x->x)(+)(*)e
---------------------------------------------	
---------------------------------------------
--UTILISONS CETTE FONCTION D'INTERPRETATION--
-- POUR ECRIRE LA FONCTION SIZE--------------
---------------------------------------------
---------------------------------------------


      -- size = interExpr alg where
 	    -- alg = AlgExpr val_ add_ mul_ where
 	  	 -- val_ :: Int->Int
 	  	 -- add_ :: Int->Int->Int
 	  	 -- add_ = (\e1 e2 -> e1 + e2 + 1)
 	  	 -- mul_ :: Int->Int->Int
 	  	 -- mul_ = (\e1 e2 -> e1 + e2 + 1)








 -- taille :: (Int -> b) -> (b -> b -> b) -> (b -> b -> b) -> Expr -> b
 -- taille = interval interadd intermul  where
 -- 	interval = 1
 -- 	intermul = (\se1 se2-> se1 + se2)
 -- 	interadd = (\ se1 se2 -> se1 + se2)

--------------------------------
--------------------------------
--Arbre Binair          e ------
--  ---------------   ----------
---------------      -----------
--------------------------------


 data Tree  = Leaf Int
 			  | Node Tree  Int Tree 
 			   deriving Show


-- test d'occurrence

 occurs :: Int->Tree->Bool
 occurs m (Leaf n) = m==n
 occurs m (Node l n r) = m==n
 						|| occurs m l
 						|| occurs m r


 --Test d'arbre reccurssif--

 -- fold :: (Int->b)->(b->Int->b->b)->Tree->b
 -- fold feuille noeud = f where
 -- 	f (Leaf n)      = (feuille n)
 -- 	f(Node l v r)   = noeud( l1 v r1) where
 -- 		r1 = f r
 -- 		l1 = f l
 		

---------------------------------------------	
---------------------------------------------
--programmation des automates----------------
-- ------------------------------------------
---------------------------------------------
---------------------------------------------
 
--structure de donnees de notre automate

 data Automate a b = Automate {
 				initiale   :: b  ,
 				finale     :: [b],
 				alphabet   :: [a],
 				etat       :: [b],
 				transition :: b-> MyMaybe a ->[b]  
 }-- deriving Show

--code haskell de l'automate

 -- auto :: Automate Char Int
 -- auto = Automate initiale finale alphabet etat transition  where
 -- 	  initiale                 = 0 
 -- 	  finale                   = [1]
 --  	  alphabet                 = ['a','b']
 --  	  etat                     = [0,1]
 --      transition  (MyMaybe b) 0 = [1]

 ---------------------------------------------

 -- printElems :: [a]->String
 -- printElems []     = ''
 -- printElems x:xs   =  (Show x) ++ "->" ++ printElems xs

