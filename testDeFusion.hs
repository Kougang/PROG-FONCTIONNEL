fusion::[String]->String
fusion [] = ""
fusion (x:xs) = x ++ " " ++ fusion xs