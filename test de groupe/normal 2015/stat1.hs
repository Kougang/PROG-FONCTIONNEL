-- count42 :: [Int] -> (Bool, [Int])
-- count42 s0 = 
--     case s0 of
--         [] -> (False, [])
--         (n:ns) -> (n==42, ns)

-- rep3Count42 :: [Int] -> ((Bool, Bool, Bool), [Int])
-- rep3Count42 s0 = 
--     let (v1, s1) = count42 s0
--         (v2, s2) = count42 s1
--         (v3, s3) = count42 s2
--     in ((v1,v2,v3), s3)

--La monade State permet de simuler un état mutable simplement. 
--Concrètement, on peut l’implémenter par un type polymorphe State s a 
--où s est le type de l’état et a le type de la valeur produite. 
--Le type State s a encapsule simplement une fonction runState qui
-- permet de passer d’un état à l’état suivant, en produisant une 
--valeur. On définit également les fonctions get et put qui permettent
-- respectivement de récupérer et de définir l’état d’un State.

newtype State s a = State { runState :: s -> (a, s) }

---------------------------------
--type definit dans le cour------
--type State s a = s -> (a, s)---
---------------------------------

--celle ci permet de reccuperer un etat

get :: State s s
get = State $ \s -> (s, s)

--celle ci permet de definir un state

put :: s -> State s ()
put s = State $ \_ -> ((), s)

--L’intérêt de ce type State est qu’il correspond à des structures 
--plus évoluées, notamment Functor, Applicative et Monad. En Haskell,
--on peut implémenter cela facilement en instanciant les classes de 
--types correspondantes.

instance Functor (State s) where
    -- fmap :: (a -> b) -> State s a -> State s b
    fmap f (State act) = State $ \s ->
        let (a1, s1) = act s
        in (f a1, s1)

instance Applicative (State s) where
    -- pure :: a -> State s a
    pure a = State $ \s -> (a, s)
    -- (<*>) :: State s (a -> b) -> State s a -> State s b
    State act1 <*> State act2 = State $ \s ->
        let (a1, s1) = act1 s
            (a2, s2) = act2 s1
        in (a1 a2, s2)

instance Monad (State s) where
    -- (>>=) :: State s a -> (a -> State s b) -> State s b
    State act >>= k = State $ \s ->
        let (a1, s1) = act s
        in runState (k a1) s1
--definition du support--
m >>= k = \s -> let (a, s2) = m s
               in (k a) s2
-------------------------
   -- return :: a -> State s a
    --return x = \s -> (x, s)


--On peut ensuite profiter des fonctionnalités que fournit Haskell 
--pour ces structures. Par exemple, on peut réécrire, plus simplement, 
--la fonction coun42 en “notation do” et la fonction rep3Count42 en 
--“style applicatif”.

count42 :: State [Int] Bool
count42 = do
s0 <- get
case s0 of
    [] -> return False
    (n:ns) -> put ns >> return (n==42)  -- runState count42 []

-- rep3Count42 :: State [Int] (Bool, Bool, Bool)
-- rep3Count42 = (,,) <$> count42 <*> count42 <*> count42