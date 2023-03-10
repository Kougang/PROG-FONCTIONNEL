data Tree a = MyNothing |Leaf a | Tree a (Tree a) (Tree a)  deriving Show

aTree  = Tree 5 (Tree 3 (Leaf 3) (Leaf 4)) (Leaf 8)

--2
prune :: Int->Tree a -> Tree a
prune _ (Leaf a)      = (Leaf a) 
prune n (Tree a1 left right) |n == 0 = (Leaf a1)
                             |otherwise = Tree a1 (prune (n-1) left) (prune (n-1) right )

-- prune 0 (Tree 4 (Leaf 3) (Leaf 4))
-- prune aTree
--3
data AlgTree a = AlgTree{
	         leaf:: a -> Tree a,
	         tree :: a->Tree a -> Tree a -> Tree a
}
--4
evalTree :: AlgTree a -> Tree a -> Tree a
evalTree alg = f where
	      f(Leaf x) = (leaf alg) x
	      f(Tree x xs ys) = (tree alg) x (f xs) (f ys)
-------------------------------
-------------------------------
--OPERATION SUPLEMENTAIRE------
-------------------------------
-------------------------------

------------------------------------------------------------------------------
------------------------------------------------------------------------------
--insertion dans un arbre de tel sorte a avoir un arbre binaire de recherche--
------------------------------------------------------------------------------
--fonction permettant de prendre un element et l'inserer dans un arbre--------
------------------------------------------------------------------------------
------------------------------------------------------------------------------

treeInsert ::  (Ord a)=> a -> Tree a -> Tree a
treeInsert x MyNothing = Leaf x 
treeInsert x (Leaf a) | x == a = (Leaf a)
					  | x<a = Tree a  (Leaf x) MyNothing
					  | otherwise = Tree a MyNothing (Leaf x)
treeInsert x (Tree a left right)
			| x == a = Tree a left right
			| x<a    = Tree a (treeInsert x left) right
			| x> a   = Tree a left (treeInsert x right)


----------------------------------------------------------------------
--fonction permettant de prendre une liste et la transfomer en arbre--
----------------------------------------------------------------------

insertion :: (Ord a)=> Tree a ->[a]->Tree a
insertion MyNothing (x:xs)  = insertion (Leaf x) xs
insertion (Leaf x) []            = Leaf x
insertion (Tree a left right) [] = (Tree a left right)
insertion (Leaf a) (x:xs)        |x == a = (Leaf a)
					             |x<a = insertion (Tree a  MyNothing  (Leaf x)) xs
					             |otherwise = insertion (Tree a (Leaf x) MyNothing ) xs
insertion (Tree a left right) (x:xs) | a>x =  Tree a left (insertion right (x:xs))
									 |otherwise = Tree a  (insertion left (x:xs)) right

listArbre :: (Ord a)=>[a] -> Tree a
listArbre [] = MyNothing
listArbre (x:xs) = insertion (Leaf x) xs

--------------------------------------------------------------
--------------------------------------------------
--transformer un arbre en liste en ifixe----------
--------------------------------------------------
--------------------------------------------------------------

treeToList :: Tree a -> [a]
treeToList MyNothing = []
treeToList (Leaf a) = [a]
treeToList (Tree a left  right ) =  treeToList left ++ [a] ++ treeToList right 



