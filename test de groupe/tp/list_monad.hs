-- {-# LANGUAGE InstanceSigs #-}



data List a = Nil | Cons a (List a)

fmap_ :: (a -> b) -> List a -> List b
fmap_ _ Nil = Nil
fmap_ f (Cons x xs) = Cons (f x) xs

instance (Show a) => Show (List a) where
    show Nil = ""
    show (Cons x xs) = show x ++ ", " ++ show xs

instance Functor List where
    fmap f Nil = Nil
    fmap f (Cons x xs) = Cons (f x) (fmap f xs)

instance Applicative List where
    -- pure :: a -> List a
    pure a = Cons a Nil
    -- 
    -- (<*>) :: State s (a -> b) -> State s a -> State s b
    -- (<*>) :: List (a -> b) -> List a -> List b
    -- (<*>) :: f (a -> b) -> f a -> f b
    -- (<*>) :: f (a -> b) -> f a -> f b
    Cons f <*> xs = join (fmap_ f xs)


instance Monad List where
    return x = Cons x Nil
    xs >>= k = join $ fmap k xs

join :: List (List a) -> List a
join Nil = Nil
join (Cons xs xss) =  cat xs (join xss)

cat :: List a -> List a -> List a
cat Nil ys = ys
cat (Cons x xs) ys = Cons x (cat xs ys)

-- fmap :: (a -> b) -> List a -> List b
-- fmap _ Nil = Nil
-- fmap f (Const x xs) = Const (f x) xs

-- instance Monad List where
--     -- return :: a -> List a
--     return x = Const x Nil


--     xs  >>= k = fmap

--     -- (>>=) :: List a -> (a -> List b) -> List b
--     -- xs  >>= k = f xs where 
--     --     f (Const p Nil) = k p
--     --     f (Const p ps) = Const (extract (k p)) (f ps) where 
--     --         extract (Const x Nil) = x