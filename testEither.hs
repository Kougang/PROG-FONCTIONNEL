
 plus :: (Num b) => Either a b -> Either a b -> Either a b
 
 plus (Left erra) _       = Left erra
 plus _ (Left errb)       = Left errb
 plus (Right a) (Right b) = Right (a + b)


 divise:: (Integral b) =>Either [Char] b -> Either [Char] b -> Either [Char] b

 divise (Left erra) _       = Left erra
 divise _ (Left errb)       = Left errb
 divise (Right _) (Right 0) = Left "Division par 0"
 divise (Right a) (Right b) = Right (a `div` b)
