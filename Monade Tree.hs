data Tree a = Leaf a | Tree a (Tree a) (Tree a)  deriving Show

 instance Monad Tree where
 	return::a->Tree a
 	return x = Leaf x

 	(>>=) Tree a-> (a->Tree b)->Tree b
 	Leaf x >>= f = f x
 	(Tree m Tree y Tree y) >>= 

  