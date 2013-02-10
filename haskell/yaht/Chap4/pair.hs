data Pair a b = Pair a b

pairFst (Pair x _) = x

pairSnd (Pair _ y) = y
