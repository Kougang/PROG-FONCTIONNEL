dividers x = [y | y <- [1..x], (x `mod` y) == 0 ]

prime x =dividers x == [1,x]

primesList n = [x | x <- [1..n], prime x]