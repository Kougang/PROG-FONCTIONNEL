module MyFonctor where

instance Functor MyMaybe where
    fmap f (MyJust a )=MyJust (f a)
    fmap f MyNothing = MyNothing

data MyMaybe a = MyJust a | MyNothing 
    deriving Show
increment2::(Num a )=> a -> a
increment2 x = 1
