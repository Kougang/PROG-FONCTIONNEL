--{-# LANGUAGE BlockArguments #-}
x = do
putStrLn "test"
y<-getLine
putStrLn $ reverse y
return ()
