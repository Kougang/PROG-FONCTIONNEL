series f a = a: series f (f a)


pair :: [a] -> [(a,a)]
pair [] = []
pair [_] = []
pair (x:y:resi) = (x,y) : pair (y:resi)



sorted :: (Ord a) => [a] -> Bool
sorted []  = True
sorted [_] = True
sorted xs  = foldr (&&) True (map f (pair xs)) where
    f (x,y) = y > x



applyPred :: (a -> b -> Bool) -> [a] -> [b]  -> Bool
applyPred f [] [] = True
applyPred f (x:xs) (y:ys) = (f x y) && applyPred f xs ys



listIndent :: (Eq a) => [a] -> [a] -> Bool
listIndent xs ys = applyPred (\x y -> x == y) xs ys

-- 1) definition du type auttomate

data Automate q s = Automate {
    initial :: q,
    final :: [q],
    alphabet :: [s],
    transition :: q -> Maybe s -> q,
    etats :: [q]
}


--  show instance


instance (Show q, Show s) => Show (Automate q s) where
    show Automate {
    initial = q,
    final = finaux,
    alphabet = alphabet,
    transition = transi,
    etats = states
} = "Automate {\n" ++ "  initial = " ++ show q ++ "\n  final = [" ++ customShowList finaux ++"]," ++ "\n  alphabet = ["++ customShowList alphabet ++"]," ++ "\n  transition = ["++ customShowTransi Automate {
    initial = q,
    final = finaux,
    alphabet = alphabet,
    transition = transi,
    etats = states
} states alphabet  ++"]," ++"\n  etats = [" ++ customShowList states++"]\n}"



customShowList :: (Show a) => [a] -> String
customShowList (x:[]) = show x 
customShowList (x:xs) = show x ++ ", " ++ customShowList xs



customShowTransi ::(Show q, Show s) => Automate q s -> [q] -> [s] -> String
customShowTransi _ [] _ = ""
customShowTransi automate (s:states) alphabet = f s alphabet ++ ",  " ++ customShowTransi automate states alphabet where
                                                f _ [] = ""
                                                f currentState (x:xs) = show currentState ++ "-"++ show x ++"->" ++ show (transition automate currentState (Just x)) 
                                                    -- resi 

transi :: q -> Maybe s -> q
transi x (Just sy) = x

-- 2) Representation de l'automate

f '0' (Just 'a') = '1'
f '0' (Just 'b') = '2'
f '1' (Just 'a') = '4'
f '1' (Just 'b') = '3'
f '2' (Just 'a') = '3'
f '2' (Just 'b') = '5'
f '3' (Just 'a') = '6'
f '3' (Just 'b') = '6'
f '4' (Just 'a') = '6'
f '4' (Just 'b') = '6'
f '5' (Just 'a') = '6'
f '5' (Just 'b') = '5'
f '6' (Just 'a') = '6'
f '6' (Just 'b') = '6'


myAutomate :: Automate Char Char
myAutomate = Automate {
    initial = '0',
    final = ['1', '3', '4', '6'],
    alphabet = ['a', 'b', 'c'],
    transition = f,
    etats = ['0', '1', '2', '3', '4', '5', '6']
}
