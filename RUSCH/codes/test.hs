{- Ici, je dois faire les tests sur les monades  -}

(>>=)::(Int-> [String]) -> (Int, [Int, [String]]) -> (Int, [String])

(x, str) >>= f =  
    let (y, str') = f x in 
    (y, str ++ str') 