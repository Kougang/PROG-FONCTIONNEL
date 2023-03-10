


 data MyMaybe a = MyJust a|MYNothing

 data Annuaire = Annuaire{
    nom    ::String, 
    adresse::String, 
    email  ::String
 } deriving Show

 base_de_donnee = [
    Annuaire "SOPJIO ROCHNEL" "DOUL" "kougangsopjio@gmail.com", 
    Annuaire "MAJESTE SOCRATE" "YAOUNDE" "fsopjio@gmail.com",
    Annuaire "SANKARA L'INTEGRE" "BURKINA" "sankara@gmail.com"]

 rechercher::[Annuaire]->String->Either String Annuaire
 rechercher base_de_donnee nm = case filter (\a -> nom a == nm ) base_de_donnee of 
    []-> Left "Cette personne n'existe pas dans la base de donnee"
    x :_ -> Right x 

