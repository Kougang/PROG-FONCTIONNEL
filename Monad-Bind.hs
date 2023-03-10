
---------------------------------
---------------------------------
--Opeateur Bind------------------
---------------------------------
---------------------------------

-- (>>=) :: Int -> Int -> a -> a

-- (>>=) :: a -> (a->b) -> b
-- x >>= f = f x

-- a = 3 Main.>>= \x ->  if (x == 20) then True else False



(>>=) :: (Int, [String]) -> (Int -> (Int, [String])) -> (Int, [String])

(x, str) >>= f =
	let (y, str1) = f x in 
	(y, str ++ str1)

a1 = (3, []) Main.>>= \x ->(x + 3, ["j ai ajoute 3"]) Main.>>=
 \y ->(y + x, ["j ai ajoute par x et y"])
 
 ----------------------------------------------
 --recrivons le bind preccedent par le where---
 ---------------------------------------------- 

-- (x,strx) >>= f = (z,zs) where
-- 	zs = strx++stry
-- 	(z,stry)= f x

--Autres exemples

-- (>>=)::(Int,[String])->(Int->(Int,[String]))->(Int,String)

-- action :: Int -> (Int, [String])
-- action x = (x+1, ["Une action va avoir lieu"])  

--  (3, [""])                          Main.>>=
--  action                             Main.>>= \x ->
--  (x+3, ["j ai ajoute 3"])           Main.>>=
 -- action                             Main.>>= \y ->
 -- (x , ["j ai multiplie x par y"])

----------------------------
----------------------------	

 -- [1..10] >>= \x ->
 -- [1..10] >>= \y ->
 --  return (x,y)

transform :: (Monad m)=>Int -> m String
transform x = return (show x)


