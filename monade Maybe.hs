data MyMaybe a = MyJust a | MyNothig
--monad maybe

instance Monad MyMaybe  where
	return:: a -> MyMaybe a
	return x = MyJust x

	(>>=) :: MyMaybe a -> (a-> MyMaybe b)->MyMaybe b
	MyJust b >>= f = f b

