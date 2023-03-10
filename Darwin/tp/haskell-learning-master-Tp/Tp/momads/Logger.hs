-- {-# LANGUAGE InstanceSigs #-}

newtype Logger a = Logger ([String],a) deriving Show


instance Functor Logger  where
    -- fmap::(Show a)=>(a->b)->(Logger a)->(Logger b)
    fmap f (Logger (xs,a) ) = Logger (xs ++ ["application on the previous value a"],f a)
        -- where  Logger (ys, b) = f (xs ,a)

        
instance Applicative Logger  where
    pure x = Logger ([],x)
    (Logger (_,f)) <*> someting = fmap f someting

instance Monad Logger where
    --return::a->(Logger s a)
    return x = Logger ([], x)
   -- (>>=)::(Logger s a)->(a->Logger s b)->Logger s b
    (>>=) (Logger (xs, a)) f = Logger (xs ++ ys,b)
        where Logger (ys,b) = f a
program1 = do
    x <- Logger ([],3)
    y <- Logger (["I added 3"], x + 3)
    z <- Logger (["I add x and y"], x + y)
    return z
out::[String]-> Logger ()
out xs = Logger (xs,())
snoc::String-> Logger ()
snoc s = out [s]

factLog::Integer->Logger Integer
factLog 0 = do snoc "(0,1)";return 1
factLog n = do 
    x <- factLog (n -1)
    snoc ("(" ++ (show n) ++ "," ++ (show (x*n)) ++ ")")
    return (x*n)