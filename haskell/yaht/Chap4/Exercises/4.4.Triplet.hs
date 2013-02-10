module Triplet
	where

data Triplet a b c = Triplet a b c

tripletFst (Triplet x _ _) = x
tripletSnd (Triplet _ y _) = y
tripletTrd (Triplet _ _ z) = z
