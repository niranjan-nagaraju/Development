import Control.Monad

isPrime :: Integer -> Bool
isPrime number = do
    let numbersList = takeWhile ( < (floor $ sqrt number)) (2: [3,5..])
    let divisorsList = (filter ( (\x -> ((mod number x) == 0)) ) numbersList)
    null divisorsList
