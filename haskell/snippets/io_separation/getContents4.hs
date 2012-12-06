import Control.Monad

main = do_io (map processit)

-- Separate IO altogether
do_io f = interact (unlines. f. lines)

processit s = show (length s)
