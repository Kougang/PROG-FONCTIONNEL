--exo 1
deleteAll :: (Eq a)=> a->[a]->[a]
deleteAll x xs = filter (\ a -> a/=x)xs --filter (/=x)xs

--exo 2

--1
paire :: [a]->[(a,a)]
paire xs = zip xs (tail xs)

--2 fonction sorted permettant de deduire si une liste est trié
sorted :: (Ord a)=> [a]->Bool
sorted xs = and [x<=y |(x,y)<-paire xs]
--3
applypred:: (a->b->Bool)->[a]->[a]->Bool
applypred p [] [] = True
applypred p (x:xs) (y:ys)  = p x y && applypred p xs  ys

