import Control.Monad

main = do
	a <- getLine
	let b = (read a)::Int
	let c = b:[3,2]
	print c

