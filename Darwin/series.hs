series::(b->b)->b-> [b]
series f a = a:series f (f a)

evenNumbers = series (\x -> x*2) 1

notNullintegers = series (\x-> x+1)  1

oneSerie = series (const 1) 1