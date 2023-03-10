import Data.Foldable 

theMax:: (Foldable t, Num b, Ord b) => t b -> b
theMax = foldr f 0

    where f x y =  if x > y then x else y    

theSum :: (Foldable t, Num b) => t b -> b
theSum  = foldr f 0 
    where f x y = x+ y

theReverse:: (Foldable t) => t b -> [b]
theReverse = foldr f []
    where  f x xs = xs ++ [x]
 
theAnd:: (Foldable t )=> t Bool-> Bool
theAnd = foldr f True
    where f x y = x == y
-- theMap:: (Foldable t)=> t b -> [b]
-- theMap :: Foldable t1 => (t2 -> a) -> t1 t2 -> [a]
theMap:: (a->b)->[a] -> [b]
theMap g  = foldr f [] 
    where f x y = g x : y
increment x = x + 1

-- theUnZip:: Foldable t => ((a,b) -> ([a],[b]))
theUnzip :: [(a1, a2)] -> ([a1], [a2])
theUnzip = foldr f ([],[])
    where f ( x, y) (lx,ly) = (x:lx, y:ly) 