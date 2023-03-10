
 --normale 2015
 --1
 second xs = head( tail xs)
--2
 mult x y = x*y
--3
 double = mult 2
--4
 palindrome xs = reverse xs==xs
--5
 twice f x = f(f x)
--6
 compose f g x = f(g x)
--7
 data T a = C a | R [T a]

 te=R [C(C True),C(C False)]

 --8 cc :: (t1 -> t2) -> (t3 -> t4 -> t1) -> (t3, t4) -> t2
 cc f g = \(x,y)->f(g x y)
--9
 qq (x:q) = (x,q)
 
--10 rr :: (Foldable t, Num a) => t a -> (a, a)
 rr q = (a,b) where
  a = foldr f1 0 q 
  b= foldr f1 0 q
  f1 x y = 10*x + y

  
