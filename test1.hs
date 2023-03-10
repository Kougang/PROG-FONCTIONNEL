-- f :: Int -> Int -> Int
-- f x y =
-- 	let resultatG = g x (x+y)
-- 		resultatH = h (y*x) y
-- 		in
-- 		resultatG + resultatH

-- f :: Int -> Int -> Int -- par exemple
-- f x y = (g x (x+y)) + (h (y*x) y) -- où g et h sont des fonctions cohérentes


(>>=) :: (Int,[String]) -> (Int ->(Int,[String])) -> (Int,[String])
(x ,str1) >>= f = (z,str) where
	(y,str2)= f x
	z=y
	str=str1++str2