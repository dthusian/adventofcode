import Data.List.Split
import Debug.Trace

type Vec2 = (Int, Int)
type Machine = (Vec2, Vec2, Vec2)

parseMachine :: String -> Machine
parseMachine inp =
  let spl = splitOn "\n" inp
      f1 = read . head . splitOn "," . (!! 1)
      f2 = read . (!! 2)
      f12 x = (f1 x, f2 x)
      v0 = splitOn "+" (spl !! 0)
      v1 = splitOn "+" (spl !! 1)
      v2 = splitOn "=" (spl !! 2)
  in (f12 v0, f12 v1, f12 v2)

parse :: String -> [Machine]
parse = map parseMachine . splitOn "\n\n"

matInv2x2 :: (Vec2, Vec2) -> ((Vec2, Vec2), Int)
matInv2x2 ((a, c), (b, d)) = (((d, -c), (-b, a)), a * d - b * c)

matMul2x2 :: (Vec2, Vec2) -> Vec2 -> Vec2
matMul2x2 ((a, c), (b, d)) (x, y) = (a * x + b * y, c * x + d * y)

solve :: Machine -> Int
solve (va, vb, vc) = 
  let (inv, divisor) = matInv2x2 (va, vb)
      btns = matMul2x2 inv vc
  in if fst btns `mod` divisor == 0 && snd btns `mod` divisor == 0
    then (3 * fst btns + snd btns) `div` divisor
    else 0

part1 :: String -> Int
part1 = sum . map solve . parse

adaptPart2 :: Machine -> Machine
adaptPart2 (va, vb, vc) = (va, vb, (fst vc + 10000000000000, snd vc + 10000000000000))

part2 :: String -> Int
part2 = sum . map solve . map adaptPart2 . parse

main :: IO ()
main = do
  contents <- readFile "input.txt"
  print (part1 contents)
  print (part2 contents)