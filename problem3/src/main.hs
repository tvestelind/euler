factors :: Integral a => a -> [a]
factors 1 = []
factors n | isPrime n = n : []
          | otherwise = do
              let (a,b) = fermatFactor n
              factors a ++ factors b

fermatFactor :: Integral a => a -> (a,a)
fermatFactor n = do
              let a = ceiling $ sqrt $ fromIntegral n
              let b = sqrt $ fromIntegral $ a^2 - n
              fermatFactor' a n b

fermatFactor' :: (Integral a, RealFrac b) => a -> a -> b -> (a,a)
fermatFactor' a n b | even n = (2, n `div` 2)
                    | isInt b = do
                        let bInt = truncate b
                        (a - bInt, a + bInt)
                    | otherwise = do
                        let newA = a + 1
                        let newB = sqrt $ fromIntegral $ newA^2 - n
                        fermatFactor' newA n newB 

isPrime :: Integral a => a -> Bool
isPrime n = do
            case fermatFactor n of
                (1,_) -> True
                (_,1) -> True
                _ -> False

isInt :: (RealFrac a) => a -> Bool
isInt x = x == fromInteger (round x)

main :: IO ()
main = do 
         let s = maximum (factors 600851475143)         
         putStrLn $ "The largest prime factor of the number 600851475143 is: " ++ show s
