firstElement :: [a] -> Maybe a
firstElement [] = Nothing
firstElement (x:xs) = Just x
