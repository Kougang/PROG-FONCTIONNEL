data Tree a = Leaf{leaf::a}
            | Node {left::Tree a  , value::a, right::Tree a }
            deriving Show

flatternTree:: Tree a -> [a]
flatternTree (Leaf a) = [a]
flatternTree ( Node left value right) = flatternTree left ++ [value] ++ flatternTree right
ar = Node (Leaf 5) 6 (Leaf 8 )
ar2 = Node ar 7 ar

