import Control.Monad


main = interact_ (unlines. map processit. lines)

-- func passed to interact is unlines. map processit. lines
interact_ func = do
   s <- getContents
   putStr (func s)

-- Processit is called for every line (Lazy evaluation)
processit s = show (length s)
