--EXERCICE2
   --donnons les valeurs de sorties
   --1
 a1  = [(x,y)| x<-[1..2] , y<-[2..5], (x+y)/=4] 
   -- type :  :: (Num b, Enum b, Eq b) => [(b, b)]
   --2
 a2 = [x |x<- [1..10],(x`mod`2)==0] 
   -- :: Integral a => [a]



--2
 a3 = map fst [(1,2),(3,8)]
 --type a3 :: [Integer]
--3
 a4=(foldr f 0 q , foldl f 0 q ) where
 								q =[6,9,8,3,10]
 								f x y = (x+y)`div`2
 								--type a4 :: (Integer, Integer)
--5
 a5 = foldr(++) [] [[1,2,3],[4,5,6],[7]]
  --type a6 :: [Integer]

  

