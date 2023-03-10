{- Ici, on doit construire un ABR  -}

data Arbre a = Feuille | Branche a (Arbre a) (Arbre a)

inserer:: Int -> Arbre a -> Arbre a
inserer e Feuille = Branche e Feuille Feuille
inserer e (Branche f g d) 
    | e == f = Branche f g d
    | e < f = Branche f (inserer e g) d 
    | e > f = Branche f g (inserer e d)