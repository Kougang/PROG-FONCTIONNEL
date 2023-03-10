-- newtype State s a = State s -> (a, s)

-- instance Monade (State s) where
--     return :: a -> State s a
--     return x = State (\s -> (x, s))

