deleteAll :: (Eq a)=> a->[a]->[a]
deleteAll x xs = filter (\ a -> a/=x)xs --filter (/=x)xs