import Control.Monad

makeList i = i: makeList i

main = do
	print list5
	where 
		list5 = take 5 $ makeList 5
