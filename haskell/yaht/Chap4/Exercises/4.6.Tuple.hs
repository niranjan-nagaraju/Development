import Tuple

{- 
 - 'Maybe' Not required for tuple1, I suppose :), 
 - There'll always be one element
 -}
tuple1 :: (Tuple a b c d) -> a
tuple1 (One a		) = a
tuple1 (Two a b		) = a
tuple1 (Three a b c	) = a
tuple1 (Four a b c d) = a

tuple2 :: (Tuple a b c d) -> Maybe b
tuple2 (One a		) = Nothing
tuple2 (Two a b		) = Just b
tuple2 (Three a b c	) = Just b
tuple2 (Four a b c d) = Just b

tuple3 :: (Tuple a b c d) -> Maybe c
tuple3 (One a		) = Nothing
tuple3 (Two a b		) = Nothing
tuple3 (Three a b c	) = Just c
tuple3 (Four a b c d) = Just c

tuple4 :: (Tuple a b c d) -> Maybe d
tuple4 (One a		) = Nothing 
tuple4 (Two a b		) = Nothing
tuple4 (Three a b c	) = Nothing
tuple4 (Four a b c d) = Just d


