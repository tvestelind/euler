factors :: Integral a => a -> [a]
factors n = factors' $ fermatFactor n

factors' :: Integral a => (a,a) -> [a]
factors' (1,1) = []
factors' (a,1) = a : []
factors' (1,b) = b : []
factors' (a,b) = (factors' $ fermatFactor a) ++
                 (factors' $ fermatFactor b)

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
         let s = maximum (factors 600851475143)
         putStrLn $ "The largest prime factor of the number 600851475143 is: " ++ show s
