-- (>>=) :: t1 -> (t1 -> t2) -> t2
-- x >>= f = f x 




-- calc = 3 Main.>>= \x -> if (x == 20) then True else False
-- 3 >>= \x -> if (x == 20) then True else False
-- (>>=) :: a -> (a -> b) -> b

-- (>>=) :: (Int, [String]) -> (Int -> (Int, [String])) -> (Int, [String])
-- (x, str) >>= f =
--             let (y, str1) = f x
--             in (y, str ++ str1)

--  version avec la clause where

(>>=) :: (Int, [String]) -> (Int -> (Int, [String])) -> (Int, [String])
(x, strX) >>= f = (z, zs) where
                    (z, strY) = f x
                    zs = strX ++ strY
           

calcLog = (3, ["Le premier calcul"]) Main.>>= \x -> ((x + 1), ["Le resultat du second calcul !"])
calcLog1 =(3, []) Main.>>= \x -> (x + 3, ["j'ai ajoute 3"]) Main.>>= \y -> (y + x, ["j'ai ajoute par x et y"])

-- utilisons let pour plus de clarté
