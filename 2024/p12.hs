import Data.Vector((!), Vector, fromList)
import Data.Set(empty, member, notMember, union, Set, size, insert, elemAt, singleton, difference)
import qualified Data.Set
import Data.Ix (Ix(inRange))
import Debug.Trace
import Data.List(nub, sort)

type Pos = (Int, Int)
type Map = Vector (Vector Char)
type Fence = (Dir, Pos)

data Dir = DUp | DDown | DLeft | DRight
  deriving (Show, Eq, Ord)
data DFSState = DFSState Map Char (Set Pos) (Set Fence) -- map, cpos, visited, perimeter
  deriving (Show, Eq)
data StepState = StepState Map Pos (Set Pos) Int -- map, cpos, visited, cost
  deriving (Show, Eq)

posAdd :: Pos -> Pos -> Pos
posAdd (x1, y1) (x2, y2) = (x1 + x2, y1 + y2)

dsAddPerim :: DFSState -> Fence -> DFSState
dsAddPerim (DFSState m c visit perim) fence = DFSState m c visit (insert fence perim)

dsAddVisited :: DFSState -> Pos -> DFSState
dsAddVisited (DFSState m c visit perim) pos = DFSState m c (insert pos visit) perim

dsInBounds :: DFSState -> Pos -> Bool
dsInBounds (DFSState m c _ _) pos = gm m pos == c

dsUnvisited :: DFSState -> Pos -> Bool
dsUnvisited (DFSState _ _ visit _) pos = notMember pos visit

ssPos :: StepState -> Pos
ssPos (StepState _ pos _ _) = pos

ssCost :: StepState -> Int
ssCost (StepState _ _ _ cost) = cost

turnLeft :: Fence -> Fence
turnLeft (DUp, p) = (DLeft, p)
turnLeft (DRight, p) = (DUp, p)
turnLeft (DDown, p) = (DRight, p)
turnLeft (DLeft, p) = (DDown, p)

turnRight :: Fence -> Fence
turnRight (DUp, p) = (DRight, posAdd p (-1, -1))
turnRight (DRight, p) = (DDown, posAdd p (1, -1))
turnRight (DDown, p) = (DLeft, posAdd p (1, 1))
turnRight (DLeft, p) = (DUp, posAdd p (-1, 1))

fwd :: Fence -> Fence
fwd (DUp, p) = (DUp, posAdd p (-1, 0))
fwd (DRight, p) = (DRight, posAdd p (0, -1))
fwd (DDown, p) = (DDown, posAdd p (1, 0))
fwd (DLeft, p) = (DLeft, posAdd p (0, 1))

-- get char from map, or space if OOB
gm :: Map -> Pos -> Char
gm m (x, y) =
  let w = length (m ! 0)
      h = length m
  in if inRange (0, w - 1) x && inRange (0, h - 1) y then m ! y ! x else ' '

dfsrecur :: DFSState -> Pos -> DFSState
dfsrecur st pos =
  let tryDfs npos nst fence = if dsInBounds nst npos then if dsUnvisited nst npos then dfsrecur nst npos else nst else dsAddPerim nst fence
      st0 = dsAddVisited st pos
      st1 = tryDfs (posAdd pos (0, -1)) st0 (DUp, pos)
      st2 = tryDfs (posAdd pos (0, 1)) st1 (DDown, pos)
      st3 = tryDfs (posAdd pos (-1, 0)) st2 (DLeft, pos)
      st4 = tryDfs (posAdd pos (1, 0)) st3 (DRight, pos)
  in st4

dfsBase :: (Set Fence -> Int) -> Map -> Pos -> (Set Pos, Int)
dfsBase perimEval m pos =
  let (DFSState _ _ visited perim) = dfsrecur (DFSState m (gm m pos) empty empty) pos
  in (visited, size visited * perimEval perim)

dfs1 :: Map -> Pos -> (Set Pos, Int) -- map, position -> cost
dfs1 = dfsBase size

traceOne :: Set Fence -> Fence -> (Fence, Int)
traceOne a x | member (turnLeft x) a = (turnLeft x, 1)
             | member (turnRight x) a = (turnRight x, 1)
             | member (fwd x) a = (fwd x, 0)
             | otherwise = error "path is incomplete"

tracePath :: Set Fence -> Fence -> Fence -> (Set Fence, Int)
tracePath a start end =
  let (next, seg) = traceOne a start
      (na, np) = tracePath a next end
  in if next == end then (singleton next, seg) else (insert next na, np + seg)

countSegs :: Set Fence -> Int
countSegs a =
  let end = elemAt 0 a
      (start, firstSeg) = traceOne a end
      (path, segs) = tracePath a start end
      remain = difference a (insert start path)
  in if Data.Set.null remain then firstSeg + segs else firstSeg + segs + countSegs remain

dfs2 :: Map -> Pos -> (Set Pos, Int)
dfs2 = dfsBase countSegs

step :: (Map -> Pos -> (Set Pos, Int)) -> StepState -> StepState
step dfs (StepState m pos visited cost)
  | notMember pos visited = let (visited2, cost2) = dfs m pos in StepState m pos (visited `union` visited2) (cost + cost2)
  | fst pos == length (m ! 0) - 1 = StepState m (0, snd pos + 1) visited cost
  | otherwise = StepState m (fst pos + 1, snd pos) visited cost

part1 :: String -> Int
part1 inp =
  let m = (fromList . map fromList . lines) inp
      finalSt = until (\ss -> ssPos ss == (length (m ! 0) - 1, length m - 1)) (step dfs1) (StepState m (0, 0) empty 0)
  in ssCost finalSt

part2 :: String -> Int
part2 inp =
  let m = (fromList . map fromList . lines) inp
      finalSt = until (\ss -> ssPos ss == (length (m ! 0) - 1, length m - 1)) (step dfs2) (StepState m (0, 0) empty 0)
  in ssCost finalSt

main :: IO ()
main = do
  contents <- readFile "input.txt"
  print (part1 contents)
  print (part2 contents)