-- tri insertion
insert:: Int->[Int]->[Int]
insert a [] = [a]
insert  a x:xs |x>=a = a:xs
			   |otherwise x:insert a xs


triInsert ::[Int]->[Int]
triInsert []    = []
triInsert x:xs = insert x xs