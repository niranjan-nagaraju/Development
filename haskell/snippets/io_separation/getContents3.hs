import Control.Monad

main = interact (unlines. map processit. lines)

processit s = show (length s)
