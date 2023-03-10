applyPred::(a->b->Bool)->[a]->[b]->Bool
applyPred f (x:xs) (y:ys) = (f x y) && applyPred f xs ys

listIdent::(Eq a)=>[a]->[a]->Bool
listIdent xs ys = applyPred (==) xs ys

pairs::[a] -> [(a,a)]
pairs [] = []
pairs [x] = []
pairs (x:y:xs) = [(x,y)] ++ (pairs (y:xs))

sorted::(Ord a)=>[a]->Bool
sorted [] = True
sorted [x] = True
sorted xs = and ( map fonc (pairs xs))
    where fonc x = fst x <= snd x
type Side =  Float
newtype T = Ta Int
t::Side->Int
t x =1
y::T->Int
y x = 1

data Tree a = Leaf a | Tree a (Tree a) (Tree a) deriving Show
data List b = Nil | Cons b (List b) deriving Show
toList::List a -> [a]
toList Nil = []
toList (Cons a xs) = [a] ++ toList xs

listT = Cons 1 (Cons 3 Nil)

fmapTree::(a->b)->Tree a ->Tree b
fmapTree f (Leaf a) = Leaf (f a)
fmapTree f (Tree a l r) = Tree (f a) (fmapTree f l) (fmapTree f r)

theTree = Tree 4 (Tree 2 (Leaf 3) (Leaf 5)) (Leaf 6)

foldTree::(a->b->b->b)->b->Tree a -> b
foldTree f e (Leaf a) = f a e e
foldTree f e (Tree a l r) = f a (foldTree f e l) (foldTree f e r)

countTreeElem::Tree Integer ->Integer
countTreeElem =foldTree (\x y z ->x+ y + z) 0

foldList::(a->b->b)->b->List a->b
foldList _ e Nil = e
foldList f e (Cons a xs) = f  a (foldList f e xs)

foldList2::(a->b->b)->b->List a->b
foldList2 f e  xs= foldr f e (toList xs)

data  AlgTree a b = AlgTree{
    leaf::a->b,
    tree::a->b->b->b

} 

foldTreeA::AlgTree a b->Tree a->b
foldTreeA alg = f
    where f (Leaf a) =  leaf alg a
          f (Tree a l r) = (tree alg) a (f l) (f r)

countTreeElem2::Tree Integer-> Integer
countTreeElem2  = foldTreeA alg 
    where alg = AlgTree{
        leaf = const 1,
        tree = (\x y z ->  1 + y+z)
    }

-- exo 2
f1 xs = [take n xs | n <- [0..length xs]]
f2 xs = [(take n xs, drop n xs) | n <- [0..length xs]]
f3 xs = case xs of 
                [] -> [[]]
                (x:xss)->[y:ys| y<- xs,ys<- f3 xss]
--index::[a]->a->Int
--index [] _ = -1
-- index xs y 
--     | y `elem` xs = fst (head [couple | couple <- zip [1..length xs] xs,(snd couple) == y ])
--     |otherwise = -1 

perm [] = []
perm [x] = [[x]]
perm xs = [ (snd couple):ys | couple <- xpos, ys <- perm (giveListWithout xs (fst couple))]
    where xpos =   zip [1..length xs] xs
          giveListWithout t ind = (take (ind-1) t )++ (drop ind t)

shuffle [] = [[]]
shuffle xs = [(x:ys) | x <- xs , ys <- shuffle (delete x xs)]
    where delete x [] = []
          delete x (y:ys) 
                | x==y = ys
                |otherwise = y:(delete x ys)
pallindrome::Eq a => [a] -> Bool
pallindrome xs = foldr (\ couple a -> ((fst couple) == (snd couple)) && a) True (zip xs (reverse xs))

-- Definiton of function which returns the set of differents parts of a initial set

setPart::[a]->[[a]]
setPart []= []
setPart [x]= [[x]]
setPart [x,y] = [[x,y],[x],[y],[]]
setPart (x:xs) = [[x],x:xs] ++ [[ x, xNext] | xNext <-  xs ] ++ setPart xs
-- setPart xs = [xs, [head xs] ] ++ setPart (tail xs)
-- setPart xs = [[ snd xCouple] ++ drop (fst xCouple) xs | xCouple<- xsPos ]
    -- where xsPos = zip [1..length xs] xs

         