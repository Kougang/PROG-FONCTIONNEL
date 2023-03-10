module BinaryTree where
data BTree  a = EmptyTree
            | Leaf{leaf::   a}
            | Node {left::BTree  a  , value::  a, right::BTree  a }
            deriving (Show, Eq)

flatternBTree:: BTree a -> [a]
flatternBTree EmptyTree = []
flatternBTree (Leaf a) = [a]
flatternBTree ( Node left value right) = flatternBTree left ++ [value] ++ flatternBTree right
ar = Node (Leaf 5) 6 (Leaf 8 )
ar2 = Node ar 7 ar

-- toNode:: Maybe (BTree a) -> BTree a
-- toNode ( (Leaf a) )= Leaf a
-- toNode (Just (Node left val right )) = Node left val right
-- toNode Nothing = 

-- toBRTree:: (Ord a)=>[a] ->   BTree a
-- toBRTree [] = EmptyTree
-- toBRTree [x] =  Leaf x
-- toBRTree (x:(y:xs) )
--     | y < x =  Node (Leaf y) x (toBRTree xs)
--     | otherwise =  Node  (toBRTree xs) x (Leaf y) 

    --insertNode is a function which take an element and insert it in existing  tree
insertNode:: Ord a => BTree a ->a ->BTree a 
insertNode EmptyTree x= Leaf x
insertNode (Leaf x) y  
    | x < y = Node EmptyTree x (Leaf y)
    | otherwise = Node (Leaf y) x EmptyTree
insertNode (Node left x right) y  
    | y <= x = Node (insertNode left y) x right
    | otherwise = Node left x (insertNode right y)

makeTree::Ord a => [a] -> BTree a
makeTree [] = EmptyTree
makeTree [x] = Leaf x
makeTree xs = insertNode (makeTree (tail (reverse xs))) (head (reverse xs))

show' :: (Show a) => BTree a -> Int -> Int -> String
show' EmptyTree _ _  = " "
show' (Leaf a) _ _ =  show a
show' (Node  left a right) depth width =
    leftside ++ "\n" ++ center ++ rightside
    where center    = replicate depth ' ' ++ show a
          leftside  = show' left (depth + width) width
          rightside = show' right (depth + width) width
-- showBTree:: Show a => BTree a => String
-- showBtree EmptyTree = " "
-- showBTree (Leaf a ) = show a
-- showBTree (Node left a right ) = showBTree