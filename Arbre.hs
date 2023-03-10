 -- data Arbre a = Leaf | Node a (Arbre a)  (Arbre a) deriving Show



 -- singleton :: a -> Arbre a
 -- sigleton x = Node x Leaf Leaf

 -- treeInsert :: (Ord a)=> a-> Arbre a -> Arbre a
 -- treeInsert x Leaf = sigleton x
 -- treeInsert x (Node a left right)
 -- 			| x == a = Node a left right
 -- 			| x<a    = Node a (treeInsert x left) right
 -- 			|x> a    = Node a left (treeInsert x right)

 data Tree a = Leaf a | Tree a (Tree a) (Tree a) deriving Show

 tTree = Tree 5 (Tree 3 (Leaf 2) (Leaf 4)) (Leaf 8)


 data  AlgTree a b = AlgTree{
    leaf::a->b,
    tree::a->b->b->b

 } 

 foldTreeA::AlgTree a b->Tree a->b
 foldTreeA alg = f
    where f (Leaf a) =  leaf alg a
          f (Tree a l r) = (tree alg) a (f l) (f r)

----------------------------------------------
----------------------------------------------
--autre arbre---------------------------------
----------------------------------------------
----------------------------------------------
 data Arbre a = Branche a (Arbre a) (Arbre a)
               |Feuille deriving Show


-- arbreExemple = Branche 1
-- 					(Branche 2
-- 					   (Branche 4 Feuille Feuille)
-- 					   (Branche 5
-- 					     (Branche 7 Feuille Feuille)
-- 					     (Branche 8 Feuille Feuille)))
-- 					(Branche 3
-- 					  Feuille
-- 					  (Branche 6
-- 					   (Branche 9 Feuille Feuille)
-- 					  Feuille))
