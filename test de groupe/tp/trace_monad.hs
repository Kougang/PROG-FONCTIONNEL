{-# LANGUAGE InstanceSigs #-}

newtype Trace s a = Trace ([s], a)

instance Monad (Trace s) where
    return :: a -> Trace s a
    return x = Trace ([], x)

    (>>=) :: Trace s a -> (a -> Trace s b) -> Trace s b
    (Trace (xs, e)) >>= k = Trace (zs, b) where
        zs  = xs ++ ys
        Trace (ys, b) = k e
