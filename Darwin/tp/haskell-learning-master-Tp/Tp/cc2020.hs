import Data.Char;
-- t = [1,2,3]
-- for x in t:
--     print x

tt f x = do y <- f x;         return (x==y)
te =  foldr (\f c -> f c) 16 [(-) 9,(*) 9,(-) 3]
t1 = \(x,y)-> g (x,y)  where g (x,y)  | x == 0 = y otherwise 0.5
t2 = \x-> \y-> g (x,y)  where g (x,y)  | x == 0 = y otherwise 0.5
t3 = \x -> \y -> x y
t4 = \x -> \y -> \z -> x (y,z) 
t5 = \x -> \(y,z) -> x y z 

series::(b->b)->b-> [b]
series f a = a:series f (f a)

evenNumbers = series (\x -> x*2) 1

notNullintegers = series (\x-> x+1)  1

oneSerie = series (const 1) 1

string2int xs= sum [ (digitToInt (fst couple)) * (snd couple)| couple <- zip (reverse xs) tenMultiple]
    where tenMultiple = series (\x -> x*10) 1

y x = do 
    if z==1 then    2
    else 
        if x < 2 then 9 else 10
    where z = x+1
j = do 
    [1..10]
    [2..10]