------------------------------

------------------------------
--2.1.3 Autres enumerations--
------------------------------

------------------------------

 data Jour = Dim|Lund|Mard|Mer|Jeu|Ven|Sam deriving Show

 class Enumm a where
         toEnumm   :: a->Int
         fromEnumm :: Int->a

 -- instance Enumm Jour where
 instance Enumm Jour where
 	
    toEnumm  Dim  = 0
	toEnumm  Lund = 1
	toEnumm  Mard = 2
	toEnumm  Mer  = 3
	toEnumm  Jeu  = 4
	toEnumm  Ven  = 5
	toEnumm  Sam  = 6

 -- 	fromEnumm   0 = Dim
	-- fromEnumm   1 = Lund
	-- fromEnumm   2 = Mard
	-- fromEnumm   3 = Mer
	-- fromEnumm   4 = Ven
	-- fromEnumm   5 = Sam

	

	
	



 -- instance Enumm Char where
	-- toEnumm   = Ord   
	-- fromEnumm = Char

-- 	--pensons a definir la class EQ et ORD

 -- instance Eq Jour where
	-- (x==y) = (toEnumm x == toEnumm y)

 -- instance Ord Jour where
	-- (x<y) = (toEnumm x < toEnummS y)


-- data Jour = Dim|Lund|Mard|Mer|Jeu|Ven|Sam
-- 	deriving (Eq, Ord, Enumm)
