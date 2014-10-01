fib = 1 : 2 : [ a + b | (a, b) <- zip fib (tail fib)]

main :: IO ()
main = do 
         let s = sum [ x | x <- takeWhile (<=4000000) fib, even x ]
         putStrLn $ "The sum of all even Fibonacci numbers below 4,000,000 is: " ++ show s
