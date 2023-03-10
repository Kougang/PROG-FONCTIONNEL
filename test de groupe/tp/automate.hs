--  import qualified Data.List as L
----------------------- Exercice Automate ------------------------------------
 ------------------------------------------------------------------------------
--------------------------- question a ----------------------------------------
-------------------------------------------------------------------------------
 data Automate q s = Automate{
    initial :: q,
    final :: [q],
    etat :: [q],
    alphabet :: [s],
    transition :: q->(Maybe s)->[q]
 }
 -------------------------------------------------------------------------------
--------------------------- question b ----------------------------------------
-------------------------------------------------------------------------------
 dfa1 :: Automate Int Char
 dfa1 = Automate intit finale etats alphabets transitions where
            intit                     = 0
            finale                        = [1]
            etats                         = [0,1]
            alphabets                     = ['a','b']
            transitions 0 (Just 'a')      =[0]
            transitions 0 (Just 'b')      =[1]
            transitions 1 (Just 'a')      =[1]
            transitions 1 (Just 'b')      =[1]
--  dfa2 :: Automate Char Char
--  dfa2 = Automate intit finale etats alphabets transis where
--             intit                          = 'A'
--             finale                         = ['B']
--             etats                          = ['A','C','D','E','F','G']
--             alphabets                      = ['a','b','c']
--             transis 'A' (Nothing)      =['C']
--             transis 'A' (Nothing)      =['E']
--             transis 'D' (Nothing)      =['G']
--             transis 'F' (Nothing)      =['G']
--             transis 'G' (Nothing)      =['A']
--             transis 'E' (Just 'b')     =['F']
--             transis 'C' (Just 'a')     =['D']
--             transis 'G' (Just 'c')     =['B']
-------------------------------------------------------------------------------
--------------------------- fonction flattern ---------------------------------
------------------------------------------------------------------------------- 
 flattern :: [[a]] ->[a]
 flattern [] = []
 flattern (xs:xss) = xs ++ flattern xss
-------------------------------------------------------------------------------
--------------------------- question c ----------------------------------------
-------------------------------------------------------------------------------
 accept :: (Eq q)=>Automate q s ->[s]->Bool
 accept auto mot = or (acceptFrom auto mot q) where q = initial auto


 acceptEtat :: (Eq q)=>Automate q s ->[s]->q->[Bool]
 acceptEtat auto [] z = [z `elem` (final auto)]
 acceptEtat auto (x:xs) z = flattern (map (acceptEtat auto xs) ys) where ys = transition auto z (Just x)
-------------------------------------------------------------------------------
--------------------------- question d----------------------------------------
-------------------------------------------------------------------------------
 acceptFrom :: (Eq q)=>Automate q s ->[s]->q->[Bool]
 acceptFrom auto [] z = [z `elem` (final auto)]
 acceptFrom auto (x:xs) z = flattern(map(acceptFrom auto xs) ys) ++ flattern(map (acceptFrom auto (x:xs)) zs) where 
                           zs = transition auto z Nothing
                           ys = transition auto z (Just x)

-------------------------------------------------------------------------------
--------------------------- affiche automate ----------------------------------
-------------------------------------------------------------------------------
--  affiche :: Automate q s -> [Char]
--  affiche  = []
-------------------------------------------------------------------------------
--------------------------- parcours automate ---------------------------------
-------------------------------------------------------------------------------
--  parcours :: Automate q s -> [s]->[[q]]
--  parcours auto chaine = let parcoursintermediaire tau liste chaine
--                                              | chaine == [] = [L.nub liste]
--                                              | otherwise = (L.nub liste) : (parcoursintermediaire tau (L.concat $ L.map (\ x -> tau x (head chaine))liste)(tail chaine))
--                         in parcoursintermediaire (transition auto) (intitial auto) chaine

------------ accept mot de dfa1 ------------------
 
--  acceptmot :: [s]->Bool
--  acceptmot mot = etataccept q mot where q = [(initial dfa1)]

--  etataccept ::[Int]->[s]->Bool
--  etataccept etat [] = etat `elem` [(final dfa1)]
--  etataccept (z:zs) (x:xs) = etataccept zs xs where q1 = (transition dfa1) z  (Just x)

---------------------------- Ajouter une action sur dfa1  --------------------------------------