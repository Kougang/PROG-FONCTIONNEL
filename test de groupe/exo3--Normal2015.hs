--que fait les operations suivantes?
 --1
 f1 xs = [take n xs | n<- [0..(length xs)]]

 f2 xs = [(take n xs,drop n xs)]