data Automate a b = Automate{
alphabet :: [a],
etats    :: [b],
ini      :: b,
term     :: [b],
trans    :: b -> a -> [b]}

tau :: Int -> Char -> [Int]
tau 0 'a' = [1]
tau 0 'b' = [2]
tau 1 'a' = [4]
tau 1 'b' = [3]
tau 2 'a' = [3]
tau 2 'b' = [5]
tau 3 'a' = [6]
tau 3 'b' = [6]
tau 4 'a' = [6]
tau 4 'b' = [6]
tau 5 'a' = [6]
tau 5 'b' = [5]
tau 6 'a' = [6]
tau 6 'b' = [6]



auto :: Automate Char Int
auto = Automate{
alphabet = ['a','b'],
etats    = [0,1,2,3,4,5,6],
ini      = 0,
term     = [1,3,4,6],
trans    = tau}

estDeterministe :: Automate a b -> Bool
estDeterministe auto = and (map (\xs -> length xs <= 1) ltrans)
                 where
                    ltrans = [((trans auto) q a) | q <- (etats auto), a <- (alphabet auto)]
                    


appartient :: (Eq b) => Automate a b -> [a] -> [b] -> Bool
appartient auto [] q = if elem (head q) (term auto) then True else False
appartient auto (x:xs) q = appartient auto xs ((trans auto) (head q) x)


appartientT :: Automate a b -> [a] -> [b] -> [b]
appartientT auto [] q = []
appartientT auto (x:xs) q = q ++ (appartientT auto xs ((trans auto) (head q) x))