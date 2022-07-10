import Prelude
import System.Environment

type Point = (Int, Int)
data Triangle = Triangle Point Point Point

twiceArea :: Triangle -> Int
twiceArea (Triangle (x1,y1) (x2,y2) (x3,y3)) = abs $ x1*(y2-y3)+x2*(y3-y1)+x3*(y1-y2)

uncurry3 :: (a -> b -> c -> d) -> (a, b, c) -> d
uncurry3 f (a, b, c) = f a b c

-- Checks if a point is in a triangle by going by the area around it
inTriangle :: Point -> Triangle -> Bool
inTriangle p (Triangle a b c) = (==)
    (twiceArea $ Triangle a b c) 
    (sum $ map (twiceArea . uncurry3 Triangle) [(a, b, p), (a, p, c), (p, b, c)])


split :: (a -> Bool) -> [a] -> [[a]]
split _ [] = [[]]
split f (a:as) = if f a then []:later else (a:(head later)):(tail later)
    where later = split f as

parseTriangle :: String -> Triangle
parseTriangle = 
    (\(x1:x2:y1:y2:z1:z2:_) -> Triangle (x1,x2) (y1,y2) (z1,z2)) . (<$>) read . split (== ',')

main :: IO()
main = (show . length . filter (inTriangle (0,0)) . map parseTriangle . lines <$> getContents) >>= putStrLn