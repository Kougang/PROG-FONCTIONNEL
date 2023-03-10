
test x = if x>6 
    then 0 
    else 9
nul :: (Eq a, Num a) => a -> Bool
nul x = if x == 0 
    then True
    else False
sign :: (Ord a, Num a) => a -> [Char]
sign x = if x > 0
    then "Positif"
    else "Negatif"
totalSign :: (Ord a, Num a) => a -> [Char]
totalSign x 
    | x < 0 = "Negatif"
    | x > 0 = "Positif"
 | otherwise = "Null"

zeroNumber' a b c = let delta = b^2 - 4*a*c in
                if delta < 0
                    then
                        (-1,-1)
                    else
                        if delta == 0 
                            then 
                                (-b/2*a,-1)
                            else
                                ((-b - sqrt(delta))/2*a,(-b + sqrt(delta))/2*a)   

zeroNumber a b c
  | a == 0 = error "a can not be null"
  | delta < 0 = error "Nothing"
  | delta == 0 = (- b / 2 * a, - 1)
  | otherwise
  = ((- b - sqrt (delta)) / 2 * a, (- b + sqrt (delta)) / 2 * a)
  where
      delta = b ^ 2 - 4 * a * c



weekDay day = case day of 
    'M'  ->  "Monday"
    'T'  ->  "Tuesday ot THursday"
    'W'  ->  "Wednesday"
    'S'  ->  "Saturday or Sunday"
    _    ->  "Not exist"
weekDay' day = case day of 
    'M'  ->  Just "Monday"
    'T'  ->  Just  "Tuesday ot THursday"
    'W'  ->  Just  "Wednesday"
    'S'  ->    Just "Saturday or Sunday"
    _    ->  Nothing

sumPair t = case t of 
    (x,y) -> x + y
    -- _     -> 0
premierNonNul x y = case (x,y) of
    (0,y) -> y
    (x,_) -> x

diffProdSum a b = product - summ
    where summ  = a + b
          product = a*b
myMin x  y 
    | x < y = x
    |otherwise = y
myMax x y 
    | x < y = y
    |otherwise = x

minMax a b c d  = (myMin(myMin a b) (myMin c d),myMax (myMax a b) (myMax c d ))

isIn a b c = if a < c && b > c  
    then c
    else
        if abs (a - c) < abs (b-c)
            then a
            else b
addition [] = 0
addition [x] = x
addition (x:y:_) = x + y

