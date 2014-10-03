leastCommonMultiple :: Integral a => a -> a -> a
leastCommonMultiple 0 0 = 0
leastCommonMultiple x y = div (x * y) (greatestCommonDivisor x y)

greatestCommonDivisor :: Integral a => a -> a -> a
greatestCommonDivisor a 0 = a
greatestCommonDivisor a b = greatestCommonDivisor b (mod a b)

main :: IO ()
main = do
        let s = foldl leastCommonMultiple 1 [1..20]
        putStrLn $ "The smallest number that is evenly divesible by all the numbers from 1 to 20 is: " ++ show s
