main :: IO ()
main = do 
         let s = sum [ x | x <- [0..999], x `mod` 3 == 0 || x `mod` 5 == 0]
         putStrLn $ "The sum of all multiples of 3 or 5 below 1000 is: " ++ show s
