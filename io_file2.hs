



ouiNon :: String -> Maybe Bool
ouiNon s = if s' `elem` oui
					then Just True
			   else if s' `elem` non
					then Just False
					else Nothing
		where oui = ["y","yes","oui","o"]
			  non = ["n","no","non"]
			  s' = map toLower s

	-- lireValide lire = do
	-- s <- getLine
	-- case lire s of
	-- Nothing -> do
	-- putStrLn "Réponse invalide"
	-- lireValide lire
	-- Just r -> return r
	-- lireOuiNon = lireValide ouiNon