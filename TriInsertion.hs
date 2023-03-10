insert:: (Ord a)=> a->[a]->[a]
insert b [] = [b]
insert b (x:xs) | b<=x = b:x:xs
                | otherwise = x:(insert b xs)

triInsert :: (Ord a)=> [a]->[a]
triInsert [] = []
triInsert (x:xs) = insert x (triInsert xs)

--  triInsert [1,5,47,2,9,0,2,-1,5,-1,4,-3,65]