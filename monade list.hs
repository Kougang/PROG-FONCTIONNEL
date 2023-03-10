--monad list
-- flattern :: [[a]]->[a]
-- flattern [[]] = []
-- flattern [[a]] = concat [[a]]
 
flattern :: [[a]]->[a]
flattern [[]]  = []
flattern (xs:xss) = xs ++ flattern xss 

-- instance Monad [] where
-- 	return :: a -> [a]
-- 	return c = [c]

-- 	(>>=) :: [a]-> (a->[b])->[b]
-- 	 xs >>= g = flattern (map g xs)