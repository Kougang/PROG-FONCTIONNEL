 data MyMaybe a = MyJust a|MYNothing

 
--structure de donnee de l'automate

 data Automate q s = Automate {
 				initiale   :: q,
 				finale     :: [q],
 				alphabet   :: [s],
 				etat       :: [q],
 				transition :: q->Maybe s->[q]  
                              } 

--code haskell de l'automate
 

 -- tr :: Int->MyMaybe Char->[Int]

 -- tr 0 (MyJust 'a')   = [0]
 -- tr 0 (MyJust 'b')   = [1]
 -- tr 1 (MyJust 'a')   = [1]
 -- tr 1 (MyJust 'b')   = [1]
 -- tr _   _            = []

 -- auto :: Automate Int Char 
 
 -- auto = Automate {
	--  initiale   = 0, 
	--  finale     = [1],
	--  alphabet   = ['a','b'],
	--  etat       = [0,1],
	--  transition = tr
	-- }





 flattern :: [[a]]->[a]
 flattern [[]]  = []
 flattern (xs:xss) = xs ++ flattern xss

 auto :: Automate Int Char 
 auto = Automate initiale_ finale_ alphabet_ etat_ transition_ where
	 initiale_   = 0 
	 finale_     = [1]
	 alphabet_   = ['a','b']
	 etat_       = [0,1]

	 transition_ 0 (Just 'a')   = [0]
	 transition_ 0 (Just 'b')   = [1]
	 transition_ 1 (Just 'a')   = [1]
	 transition_ 1 (Just 'b')   = [1]
	 transition_ _   _          = []


-----------------------------------------------
--fonction pour notre automate particuliere----
-----------------------------------------------

 acceptA :: [s]->Bool
 acceptA word = (accept q0 word) where q0=(initiale auto)

 accept :: q->[s]->Bool
 accept q [] = q `elem` (finale auto) 
 accept q (x:xs) = (accept q1 xs) where q1=(transition auto) q (Just x) 




--  -- code permettant d'accepter un mot quelconque
 
 -- accept :: (Eq q)=> Automate q s -> [s]-> Bool
 -- accept auto mot = or (acceptFrom auto mot q) where q = (initiale auto)

 -- acceptFrom:: (Eq q)=> Automate q s -> [s]-> q -> [Bool]
 -- acceptFrom auto [] z = [z`elem`(finale auto)]
 -- acceptFrom auto (x:xs) z = flattern ((map(acceptFrom xs)ys)) where ys = ((transition auto) z (MyJust x))

-- --accepteur de language regulier

--  acceptation :: (Eq q)=> Automate q s -> [s]-> Bool
--  acceptation auto (x:xs) z =flattern (map(acceptFrom xs)ys) ++ 
--  flattern (map(acceptFrom x:xs)zs) where ys = trans z MYNothing 
 	  
    


