import Control.Monad

main = do
    s <- getContents
    let r = map processit (lines s)
    putStr (unlines r)


-- Processit is called for every line (Lazy evaluation)
processit s = show (length s)
