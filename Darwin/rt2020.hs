lengthx::[a]-> Int
lengthx = foldr t 0
    where t x y = 1 + y 
reversex::[a]->[a]    
reversex = foldr f []
    where f x  y = y ++ [x]
mapx g =foldr f []
    where f x y = [g x] ++ y
data Auto a b = MakeAuto {
    intialStates:: b,
    alphabet:: [a],
    states::[b],
    finalStates::[b],
    trans1::b->a->[b]
} deriving  Show

auto = MakAuto {
    initialStates = 0,
    alphabet = ['a','b'],
    states = ["6"],
    finalStates = ["6"],
    trans1 "0" 'a'  = ["1"],

}