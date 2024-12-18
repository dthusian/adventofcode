import Data.Vector((!), Vector, fromList)
import qualified Data.Vector as Vector
import Data.Ix (Ix(inRange))
import Data.List
import Data.Monoid

type Map = Vector (Vector Char)

w :: Map -> Int
w s = length (s ! 0) - 1

h :: Map -> Int
h s = length s - 1

c2i :: Char -> Int
c2i '0' = 0
c2i '1' = 1
c2i '2' = 2
c2i '3' = 3
c2i '4' = 4
c2i '5' = 5
c2i '6' = 6
c2i '7' = 7
c2i '8' = 8
c2i '9' = 9
c2i _ = -1

dfsrecur :: (Monoid a) => (Int -> Int -> a) -> Int -> Int -> Int -> Map -> a
dfsrecur xy2a x y 9 s = xy2a x y
dfsrecur xy2a x y l s =
  let check xp yp = if inRange (0, w s) xp && inRange (0, h s) yp && c2i (s ! yp ! xp) == l + 1 then dfsrecur xy2a xp yp (l + 1) s else mempty
  in check (x + 1) y <> check (x - 1) y <> check x (y + 1) <> check x (y - 1)

dfs :: (Monoid a) => (Int -> Int -> a) -> (a -> Int) -> Int -> Int -> Map -> Int
dfs xy2a a2i x y s = if c2i (s ! y ! x) == 0 then a2i (dfsrecur xy2a x y 0 s) else 0

common :: (Monoid a) => (Int -> Int -> a) -> (a -> Int) -> String -> Int
common xy2a a2i inp = 
  let smap = fromList (map fromList (lines inp))
  in sum [dfs xy2a a2i x y smap | x <- [0 .. w smap], y <- [0 .. h smap]]

part1 :: String -> Int
part1 = common (fmap fmap fmap (:[]) (,)) (length . nub)

part2 :: String -> Int
part2 = common ((const . const . Sum) 1) getSum

main :: IO ()
main = do
  contents <- readFile "input.txt"
  print (part1 contents)
  print (part2 contents)