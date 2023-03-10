 

 --Vous savez maintenant exécuter des actions simples, au moins dans ghci, mais seulement une seule à la fois. Comment faire pour
--exécuter plusieurs actions à la suite, ou réutiliser le résultat d'une action dans une autre action ?
--Pour cela il faut utiliser la notation do. On va commencer avec un exemple simple : un programme qui affiche deuxlignes de texte.
--Ouvrez un nouveau fichier, et entrez-y le code suivant :

 conversation = do
	putStrLn "Bonjour"
	putStrLn "Au revoir"
--
--La notation do permet de combiner plusieurs actions pour en faire une seule action. Elle garantit que les actions sont réalisées
--dans l'ordre spécifié : plus de problèmes avec l'évaluation paresseuse. Il est bien sûr possible de faire des fonctions qui prennent
--des arguments :

--
 conversation1 nom = do
	putStrLn $ "Bonjour " ++ nom
	putStrLn "Au revoir" 

--La notation do permet aussi de récupérer le résultat des actions effectuées. Pour cela on utilise la flèche pour récupérer le résultat
 echo = do
	putStrLn "Entrez un mot"
	mot <- getLine
	putStrLn $ "Vous avez dit " ++ mot

--Vous pouvez définir des valeurs intermédiaires avec let. Cependant, la syntaxe est différente d'un let normal : il n'y a pas besoin
--d'indiquer "in" à la fin :

 retourner = do
	putStrLn "Entrez un mot"
	mot <- getLine
	let envers = reverse mot
	putStrLn $ "Dites plutôt " ++ envers
--Enfin, vous devez savoir que le résultat de la dernière action est renvoyé. Il est donc possible de factoriser le code de cette façon
 
 lireMot1 = do
	 putStrLn "Entrez un mot"
	 getLine

 -- echo1 = do
 -- mot <- lireMot1
 -- putStrLn $ "Vous avez dit " ++ mot

 -- retourner = do
 -- mot <- lireMot1
 -- let envers = reverse mot
 -- putStrLn $ "Dites plutôt " ++ envers


--Mais imaginons que vous ne voulez pas renvoyer la dernière valeur. Dans ce cas, vous ne pouvez pas écrire un code comme :

 -- lireMot2 = do
	-- putStrLn "Entrez un mot"
	-- x <- getLine
	-- putStrLn  "Merci !" 

 lireMot3 = do
	putStrLn "Entrez un mot"
	x <- getLine
	putStrLn "Merci !"
	return x

--do, structures conditionnelles et récursion

 motSecret x = do
	putStrLn "Entrez le mot secret"
	m <- getLine
	if x == m
	then return True
	else return False
---------------------------
---------------------------
---------------------------
 motSecret1 x = do
	putStrLn "Entrez le mot secret"
	m <- getLine
	if x == m
	  then do
		putStrLn "Vous avez trouvé !"
		return True
	else do
		putStrLn "Non, ce n'est pas le mot secret"
		return False
--Maintenant, on veut faire une dernière modification :si le mot entré n'est pas le bon, on veut redemander le mot. Vous pensez
--peut-être tout de suite à utiliser une boucle : tant que le mot entré n'est pas le bon, on demande un mot à l'utilisateur. En Haskell,
--on utilise la récursivité à la place : ilsuffit de rappeler la fonction motSecret si le mot entré n'est pas le bon :

 motSecret2 x = do
	 putStrLn "Entrez le mot secret"
	 m <- getLine
	 if x == m
		 then putStrLn "Vous avez trouvé !"
	 else do
		 putStrLn "Non, ce n'est pas le mot secret, veillez recommencer."
		 motSecret2 x

--Cette idée peut servir pour des choses plus intéressantes. Par exemple, on peut coder facilement un plus ou moins de cette façon

 -- plusOuMoins x xmin xmax ncoups = do
	--  putStrLn $ "Entrez un nombre entre" ++ show xmin ++ "et" 
 --     ++show xmax 
 --     y <- readLn
	--  case compare x y of
	--  LT -> do
	--    putStrLn "Plus petit !"
	--    plusOuMoins x xmin (y-1) (ncoups + 1)
	--  GT -> do
	-- 	putStrLn "Plus grand !"
	-- 	plusOuMoins x (y+1) xmax (ncoups + 1)
	--  EQ -> do
	-- 	putStrLn $ "Bravo, vous avez trouvé le nombre en " ++ show
	-- 	ncoups ++ " essais"
----------------------------
----------------------------

	


