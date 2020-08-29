import Control.Monad 

factorial :: Integer -> Integer
factorial x = product [1..x]

main = do    
    n <- readLn
   
    -- Read n numbers and store them in a list
    inputList <- mapM (\a -> readLn) [1..n]
           
    let outputList = map factorial inputList
    mapM_ print outputList