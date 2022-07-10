import Prelude
import System.Environment

gen :: [Int] -> Int -> [Int]  
gen ls 0 = [1]
gen ls x = (sum $ map (\l -> (later !! (l-1))) $ filter (<= x) ls):later
    where later = gen ls (x-1)

main :: IO()
main = do
    size:lengths <- (read <$>) <$> getArgs
    putStrLn . show . head $ gen lengths size