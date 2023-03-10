data Tree a = Leaf a| Tree a (Tree a) (Tree a) deriving Show



-- foldlTree leaf tree = f
--     where f (Leaf x) = leaf x
--           f (Tree x l r) = tree x (f l) (f r)



aTree = Tree 5 (Tree 3 (Leaf 2) (Leaf 4)) (Leaf 8)

leafInter = const 1
treeInter x y z = 1 + y + z

-- foldlTree2 inter leaf tree = f
--     where f (Leaf x) = leaf (inter x)
--           f (Tree x l r) = tree (inter x) (f l) (f r)
data AlgTree a b = AlgTree { leaf:: a -> b, tree::a->b->b->b }

foldlTree3 alg = f
    where f (Leaf x) = (leaf alg) x
          f (Tree x l r) = (tree alg) x (f l) (f r)

theAlgTree = AlgTree { leaf = leafInter, tree= treeInter}