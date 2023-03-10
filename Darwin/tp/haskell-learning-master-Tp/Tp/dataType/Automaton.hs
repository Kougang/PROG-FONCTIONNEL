module Automaton (isDeterminist, appart) where
-- data MyType a = Mytype a a deriving Show 
data State a = MakeState{ label::a} deriving Show
data Transition a b = MakeTransition{ start::State a,symbol::b, arrival::State a} deriving Show
data Automaton a b = EmptyAutomaton | MakeAutomaton{
                                                    initialStates::[State a],
                                                     finalStates::[State a],
                                                     q::[State a],
                                                      transitions::[Transition a b]

                                                      } deriving Show
state0 =  MakeState 0
state1 = MakeState 1
state2 = MakeState 2
state3 = MakeState 3

t1 = MakeTransition state0 'a' state0
t2 = MakeTransition state0 'b' state0
t3 = MakeTransition state0 'a' state1
t4 = MakeTransition state1 'b' state2
--t5 = MakeTransition state2 '€' state3
t5 = MakeTransition state2 'b' state3

theIntialStates = [state0]
theFinalStates = [state3,state0]
theStates = [state0,state1,state2,state3]
theTransitions = [t1,t2,t3,t4,t5]

theAutomaton = MakeAutomaton theIntialStates theFinalStates theStates theTransitions
theAutomaton2 = MakeAutomaton{
                                                    initialStates=theIntialStates,
                                                     finalStates=theFinalStates,
                                                     q=theStates,
                                                      transitions=theTransitions

                                                      }
q0 =MakeState "q0" 
q1 = MakeState "q1"
q2 = MakeState "q2"
q0q1 = MakeTransition q0 'b' q1                                                    
q1q2 = MakeTransition q1 'a' q2                                                   
q2q1 = MakeTransition q2 'a' q1
q2q0 = MakeTransition q2 'b' q0
qAutomaton = MakeAutomaton{
                                                    initialStates=[q0],
                                                     finalStates=[q1, q2],
                                                     q=[q0,q1,q2],
                                                      transitions=[q0q1,q1q2,q2q1,q2q0]

                                                      }
s1 = MakeState 1
s2 = MakeState 2
s3 = MakeState 3
s1s1 = MakeTransition s1 'b' s1
s1s2 = MakeTransition s1 'a' s2                                               
s2s1 = MakeTransition s2 'b' s1                                               
s2s3 = MakeTransition s2 'a' s3                                               
s3s2 = MakeTransition s3 'a' s2  

sAutomaton = MakeAutomaton{
                                                    initialStates=[s1],
                                                     finalStates=[s3],
                                                     q=[s1,s2,s3],
                                                      transitions=[s1s2,s1s1,s2s1,s2s3,s3s2]

                                                      }
interSectionA =intersect    sAutomaton qAutomaton                                                   

-- isDeterminist::(Eq b)  => Automaton a b -> Bool
-- isDeterminist EmptyAutomaton = False
-- isDeterminist (MakeAutomaton intialStates finalStates states transitions) =
--     (( (length ([a | a <- transitions, (symbol a ) == 'E' ])) == 0  ) ||  0 ==(length ([s | s <- states,( isRedudant s transitions)])))
--     where isRedudant theState transitions = 
--         (([]))

contentsEpsisonTransition []  = False
contentsEpsisonTransition (transition:xs )
        | symbol transition == '€' = True
        |otherwise = contentsEpsisonTransition xs

isDeterminist:: (Eq a )  => Automaton a Char -> Bool
isDeterminist EmptyAutomaton = False
isDeterminist (MakeAutomaton intialStates finalStates states transitions) =
      not (contentsEpsisonTransition transitions) &&    not  (oneStateIsAmbigious states transitions)
    where contentsEpsisonTransition []  = False
          contentsEpsisonTransition (transition:xs )
             | symbol transition == '€' = True
             |otherwise = contentsEpsisonTransition xs
          oneStateIsAmbigious [] theTransitions =  False
          oneStateIsAmbigious (s:states ) theTransitions
                    | redundantSymbol symbolsOfSTransitions   = True
                    |otherwise = oneStateIsAmbigious states theTransitions
            where redundantSymbol [] = False
                  redundantSymbol (currentSymbol:symbolsOfSTransitions)
                        |null t = redundantSymbol symbolsOfSTransitions
                        |otherwise = True
                        where t = [theSymbol | theSymbol <- symbolsOfSTransitions, currentSymbol == theSymbol ]
                  symbolsOfSTransitions =  [symbol a | a <- theTransitions, label (start a ) == label s]
--Definition of function which determine if a word apparts to and automaton
isIn ::Eq a =>  State a->[State a] -> Bool
isIn _ [] = False
isIn theState (currentState:restOfSetOfStates)
     | label theState == label currentState = True
     |otherwise = isIn  theState restOfSetOfStates


appart::(Eq a)=> Automaton a Char-> String -> Bool
appart EmptyAutomaton _ = False
appart (MakeAutomaton intialStates finalStates states transitions) word
      | length word > 0 = not (null [state | state<- (getFinalStates word intialStates transitions), isIn state finalStates])
      | otherwise  = not (null theFinalsStates) || not (null [state | state<- getFinalStates "€" intialStates transitions, isIn state finalStates])
 where  theFinalsStates =  [theState |theState<- intialStates , isIn theState finalStates]

nextStates::(Eq a)=> Char->[State a]->[Transition a Char ]->[State a]
nextStates theSymbol [] _ = []
nextStates theSymbol _ [] = []
nextStates theSymbol (currentState:xs) transitions = [ arrival transition| transition <- recoveryTransitions ] ++ nextStates theSymbol xs transitions
            where recoveryTransitions = [theTransition | theTransition<- transitions ,
                                                            label (start theTransition) == label currentState,
                                                             (symbol theTransition )== theSymbol]

getFinalStates::Eq a => String
  -> [State a]
  -> [Transition a Char]
  ->[State a]
getFinalStates word [] _ = []
getFinalStates [currentSymbol] states transitions = nextStates currentSymbol states transitions
getFinalStates (currentSymbol:rest) states transitions = getFinalStates rest (nextStates currentSymbol states transitions) transitions
getFinalStates [] states transitions = getFinalStates "€" states transitions

--Here we want to define the fucntions wch give the demonstration that a certain word appart to an automaton
--here we have to give all wises whch dertermine the thqt the word is one of the automaton

-- appartWithProof::(Eq a)=> Automaton a Char-> String -> [State a]
-- appartWithProof (MakeAutomaton intialStates finalStates states transitions) word 
--       | not appart (MakeAutomaton intialStates finalStates states transitions) word = []
--       |otherwise = 

-- previousStates::Eq a => [[State a]] ->String->[[State a]] 
-- previousStates [[]] _ _ = [[]]
-- previousStates arrivalStatesSet [symbol] = [statesSet | statesSet <- ]
-- --The function retrace road of a given word by the final state
-- giveRoads::Eq a => String->[Transition a Char]-> [State a]->[[State a]]
-- giveRoads [] transitions initialStates = [t ++ find | t<- initialStates , ]
--       where 

timesTransitions :: (Eq a, Eq b1, Eq b2) => [State (a, b1)] -- ^ 
  -> [Transition a b2] -- ^ 
  -> [Transition b1 b2] -- ^ 
  -> [Transition (a, b1) b2]
timesTransitions theStates theTransitions1 theTransitions2 = [MakeTransition sState (snd astateSymbol)  (MakeState  (label ( fst astateSymbol), aState))  |
                                                                                          sState<- theStates, astateSymbol<- symbols sState theTransitions1, aState<- arStates sState (snd astateSymbol) theTransitions2]
                                          where symbols s t = [(arrival trans,symbol trans)| trans <- t, label (start trans) == fst (label s)]
                                                arStates s sym t = [ label (arrival trans )| trans <- t, snd (label s) == label (start trans),symbol trans == sym]

--Fubction intersection

-- intersect:: Eq a => Automaton a b -> Automaton a b -> Automaton (a,a) b
intersect EmptyAutomaton EmptyAutomaton = EmptyAutomaton
intersect _ EmptyAutomaton = EmptyAutomaton
intersect EmptyAutomaton _ = EmptyAutomaton
intersect (MakeAutomaton intialStates1 finalStates1 states1 transitions1) (MakeAutomaton intialStates2 finalStates2 states2 transitions2) =
                  MakeAutomaton intialStates finalStates statesProduct transitions
            where intialStates =  [state | state <- statesProduct , isIn (MakeState (fst (label state))) intialStates1 ,isIn (MakeState (snd (label state))) intialStates2]
                  finalStates  = [state | state <- statesProduct, isIn (MakeState (fst (label state))) finalStates1 ,isIn (MakeState (snd (label state))) finalStates2 ]
                  transitions = timesTransitions statesProduct transitions1 transitions2
                  statesProduct = [MakeState (label a,label b) | a<- states1, b <- states2]
                 