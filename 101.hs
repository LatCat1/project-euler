import Prelude
import System.Environment


type GenFunc = Int -> Int

interpret :: [Int] -> GenFunc
interpret [] = \_ -> 0
interpret (s:ss) = \x -> s + x*(interpret ss $ x)

points :: GenFunc -> [Int]
points = flip (<$>) [1..]

diffs :: [Int] -> [Int]
diffs xs = zipWith (\a b -> a - b) xs $ tail xs

comb :: [Int] -> Int
comb ([]) = 0
comb (x:[]) = x
comb xs = head xs + comb later
    where later = diffs xs

bop :: Int -> [Int] -> Int
bop n ls = comb . reverse $ take n ls


sumBop :: [Int] -> Int
sumBop as = sum $ (flip bop (points $ interpret as)) <$> [1..(length as - 1)]


main :: IO()
main = getArgs >>= (putStrLn . show . sumBop . map read)