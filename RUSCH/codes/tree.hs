{- data Tree = Leaf Int | Node Tree Int Tree

inordered :: Tree -> Bool 
inordered (Leaf n) = True
inordered (Node l n r) = n> 1

 -}

data Tree = Leaf Int | Node Tree Int Tree | Empty deriving Show 

flattern:: Tree -> [Int]
flattern (Leaf a) = [a]
flattern Empty = []
flattern (Node g i d) = flattern g ++ [i] ++ flattern d

{-
sorted::[Int] -> Bool
sorted [] = True
sorted [x] = True
sorted (x:y:xs) = x <= y && sorted(y:xs)

inordered::Tree -> Bool
inordered (Leaf a) = True 
inordered (Node g m d) = sorted(flattern(Node g m d)) -}

{- Fonction d'insertion -}

insert:: Int -> Tree ->Tree
insert x Empty = Leaf x
insert x (Leaf n)
    | x <= n = Node (Leaf x) n Empty
    | x > n = Node Empty n (Leaf x)
insert x (Node g m d)
    | x <= m = Node (insert x g) m d
    | x > m = Node g m (insert x d)  
makeTree:: [Int] -> Tree
makeTree [] = Empty
makeTree [a] = Leaf a
makeTree (x:y:xs) = insert x (makeTree (y:xs))