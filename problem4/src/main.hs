makeList :: Integral a => a -> [a]
makeList 0 = []
makeList a = makeList (a `div` 10) ++ (a `mod` 10) : []

isPalindrome :: (Integral a, Eq a) => a -> Bool
isPalindrome a = (makeList a) == (reverse $ makeList a)

main :: IO ()
main = do
        let threeDigits = [100..999]
        let allCombos = [ x * y | x <- threeDigits, y <- threeDigits]
        let s = maximum [ x | x <- allCombos, isPalindrome x] 
        putStrLn $ "The largest palindrome made from the product of two 3-digit numbers: " ++ show s
