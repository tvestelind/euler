main :: IO ()
main = do
        let sumOfSquares = sum [ x^2 | x <- [1..100]]
        let squareOfSums = (sum [1..100] )^2
        let diff = squareOfSums - sumOfSquares
        putStrLn $ "The smallest number that is evenly divesible by all the numbers from 1 to 20 is: " ++ show diff
