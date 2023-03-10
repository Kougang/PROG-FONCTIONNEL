{- Ici, on defini un type annuaire pour la representation des annuaires -}

data Annuaire = Annuaire{
    nom::String, 
    adresse::String, 
    email::String
} deriving Show

base_de_donnee = [
    Annuaire "Takam Rushclin" "Bafoussam" "takamrushclin@gmail.com", 
    Annuaire "Roslyn Le Grand" "Dschang" "legrand@gmail.com",
    Annuaire "Ouandji Armel" "Yaounde" "armel@gmail.com"]


rechercher::[Annuaire]->String->Either String Annuaire
rechercher base_de_donnee nm = case filter (\a -> nom a == nm ) base_de_donnee of 
    []-> Left "Cette personne n'existe pas dans la base de donnee"
    x :_ -> Right x 