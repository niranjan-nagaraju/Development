{- Extract a given number of randomly selected elements from a list. -}

import System.Random
import Control.Monad

-- Return a random number between x to y
random_num :: Int -> Int -> IO Int
random_num x y = getStdRandom $ randomR (x, y)

rnd_select list n = do
	let random_gen = (random_num 0 (length list - 1))
	indexes <- replicateM n random_gen
	return [list !! p | p <- indexes]
