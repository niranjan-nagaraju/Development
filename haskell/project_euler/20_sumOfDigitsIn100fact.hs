import Data.Char

main = do
	print $ sum . map digitToInt.show $ product [1..100] {- 648 -}
