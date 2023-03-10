-- import MyFonctor 
-- import Control.Monad()
instance Functor MyMaybe where
    fmap f (MyJust a )=MyJust (f a)
    fmap _ MyNothing = MyNothing

data MyMaybe a = MyJust a | MyNothing 
    deriving Show

instance Applicative MyMaybe where
    pure = MyJust
    MyNothing <*> _ = MyNothing
    (MyJust f) <*> something = fmap f something
instance Monad MyMaybe where
    return x = MyJust x
    (>>=) (MyNothing) _ = MyNothing
    (>>=) (MyJust x) f = f x
    
--t::MyMaybe Integer
t = do
    x <- (MyJust 4)
    y<- (MyJust 12)

    return  (y + x)


    