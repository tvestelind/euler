primeFactors :: Integral a => a -> [a]
primeFactors n = primeFactors' $ fermatFactor n

primeFactors' :: Integral a => (a,a) -> [a]
primeFactors' (1,1) = []
primeFactors' (a,1) = a : []
primeFactors' (1,b) = b : []
primeFactors' (a,b) = (primeFactors' $ fermatFactor a) ++
                      (primeFactors' $ fermatFactor b)

fermatFactor :: Integral a => a -> (a,a)
fermatFactor n = do
                let a = ceiling $ sqrt $ fromIntegral n
                fermatFactor' a n

fermatFactor' :: (Integral a) => a -> a -> (a,a)
fermatFactor' a n | even n = (2, n `div` 2)
                  | otherwise = do
                    let b = sqrt $ fromIntegral $ a^2 - n
                    case isInt b of
                        True -> do
                            let bInt = round b
                            (a-bInt, a+bInt)
                        False -> fermatFactor' (a+1) n

isInt :: (RealFrac a) => a -> Bool
isInt x = x == fromInteger (round x)

main :: IO ()
main = do
         let s = maximum (primeFactors 600851475143)
         putStrLn $ "The largest prime factor of the number 600851475143 is: " ++ show s
