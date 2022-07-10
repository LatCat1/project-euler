import Prelude
import System.Environment

gen :: Int -> Int -> [Int]  -- this seems to always overcount by 1
gen l 0 = [0]
gen l x = ((head later) + (if x >= l then 1 + later !! (l-1) else 0)):later
    where later = gen l (x-1)

main :: IO()
main = do
    size:lengths <- (read <$>) <$> getArgs
    putStrLn . show . sum $ map (head . flip gen size) lengths