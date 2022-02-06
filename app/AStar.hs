module AStar (startAStar, AsGridCell (..), GridCell (..)) where
  import Data.List
  import Data.Maybe
  import Data.Map (Map)
  import qualified Data.Map as Map

  import Debug.Trace

  import qualified Layout

  data GridCell = Free | Used deriving (Show, Eq);

  type Grid = Layout.Grid GridCell;

  newtype ScoreMap i = ScoreMap (Map i Int);
  type FScores i = ScoreMap i;
  type GScores i = ScoreMap i;
  newtype CameFrom i = CameFrom (Map i i);
  type DistanceFunc i = (i -> i -> Int);
  type NeighbourFunc c i = c -> ((i, Maybe i) -> [(i, Int)]);
  newtype OpenSet i = OpenSet ([(i, Int)]) deriving Show;

  class AsGridCell bc where
    asCell :: bc -> GridCell

  instance AsGridCell GridCell where
    asCell cell = cell

  emptyCameFrom :: (Ord ind) => CameFrom ind
  emptyCameFrom = CameFrom (Map.empty)

  emptyScoreMap :: (Ord i) => ScoreMap i
  emptyScoreMap = ScoreMap (Map.empty);

  startAStar :: (Layout.Container cont ind c, Eq ind, Ord ind, Show ind, AsGridCell c) => DistanceFunc ind -> NeighbourFunc cont ind -> cont -> ind -> ind -> [ind]
  startAStar dist neighbours grid start dest
    = aStar dist neighbours grid start dest openSet cameFrom gScore fScore
    where openSet = insertOpenSet emptyOpenSet (start, 0)
          cameFrom = emptyCameFrom
          gScore = setScore emptyScoreMap start 0
          fScore = setScore emptyScoreMap start (dist start dest)

  aStar :: (Layout.Container cont ind c, Eq ind, Ord ind, Show ind, AsGridCell c) => DistanceFunc ind -> NeighbourFunc cont ind -> cont -> ind -> ind -> OpenSet ind -> CameFrom ind -> GScores ind -> FScores ind -> [ind]
  aStar h n_f grid start dest rawOpenSet cameFrom gScores fScores
    | (current == dest) = aStarReconstructPath cameFrom current
    | otherwise = aStar h n_f grid start dest n_openSet n_cameFrom n_gScores n_fScores
      where (current, openSet) = getLowestOpen rawOpenSet
            pred = cameFromPredecessor cameFrom current
            neighbours = n_f grid (current, pred)
            (raw_openSet_Entries, n_cameFrom, n_gScores, n_fScores) = aStarNeighbours h current neighbours dest cameFrom gScores fScores []
            (update_openSet_Entries, new_openSet_Entries) = partition (openSetContains openSet) raw_openSet_Entries
            u_openSet_Entries = map (\op -> (op, getScore n_fScores op)) update_openSet_Entries
            u_openSEt = foldl (\op e -> updateOpenSet op e) openSet u_openSet_Entries
            n_openSet_Entries = map (\op -> (op, getScore n_fScores op)) new_openSet_Entries
            n_openSet = foldl (\op e -> insertOpenSet op e) openSet n_openSet_Entries

  aStarNeighbours :: (Eq ind, Ord ind) => DistanceFunc ind -> ind -> [(ind, Int)] -> ind -> CameFrom ind -> GScores ind -> FScores ind -> [ind] -> ([ind], CameFrom ind, GScores ind, FScores ind)
  aStarNeighbours _ _ [] _ cameFrom gScores fScores openNodes = (openNodes, cameFrom, gScores, fScores)
  aStarNeighbours h current ((neighbour, n_cost):neighbours) dest cameFrom gScores fScores nOpenNodes
    | tmp_gscore < old_neighbor_score = aStarNeighbours h current neighbours dest n_cameFrom n_gScores n_fScores (neighbour:nOpenNodes)
    | otherwise = aStarNeighbours h current neighbours dest cameFrom gScores fScores nOpenNodes
    where tmp_gscore = (getScore gScores current) + n_cost
          old_neighbor_score = getScore gScores neighbour
          n_cameFrom = updateCameFrom cameFrom neighbour current
          n_gScores = setScore gScores neighbour tmp_gscore
          n_fScores = setScore fScores neighbour (tmp_gscore + (h neighbour dest))

  getScore :: (Eq ind, Ord ind) => ScoreMap ind -> ind -> Int
  getScore (ScoreMap map) pos = Map.findWithDefault (maxBound - 1) pos map
  
  setScore :: (Eq ind, Ord ind) => ScoreMap ind -> ind -> Int -> ScoreMap ind
  setScore (ScoreMap map) pos value = ScoreMap (Map.insert pos value map)

  updateCameFrom :: (Eq (ind), Ord ind) => CameFrom ind -> ind -> ind -> CameFrom ind
  updateCameFrom (CameFrom map) pos value = CameFrom (Map.insert pos value map)

  cameFromPredecessor :: (Eq (ind), Ord ind) => CameFrom ind -> ind -> Maybe ind
  cameFromPredecessor (CameFrom map) pos = Map.lookup pos map

  aStarReconstructPath :: (Eq (ind), Ord ind) => CameFrom ind -> ind -> [ind]
  aStarReconstructPath cameFrom current = case (cameFromPredecessor cameFrom current) of
    Just pred -> (aStarReconstructPath cameFrom pred) ++ [current]
    Nothing -> [current]

  emptyOpenSet :: OpenSet i
  emptyOpenSet = OpenSet ([])

  getLowestOpen :: Show i => OpenSet i -> (i, OpenSet i)
  getLowestOpen (OpenSet ([])) = error "Empty OpenSet"
  getLowestOpen (OpenSet (((c, _):rest))) = (c, OpenSet (rest))

  openSetContains :: Eq i => OpenSet i -> i -> Bool
  openSetContains (OpenSet []) _ = False
  openSetContains (OpenSet ((c, _):rest)) n
    | (n == c) = True
    | otherwise = openSetContains (OpenSet rest) n

  insertOpenSet :: Eq i => OpenSet i -> (i, Int) -> OpenSet i
  insertOpenSet (OpenSet ([])) val = OpenSet ([val])
  insertOpenSet (OpenSet ((n, val) : rest)) (n_node, n_val)
    | n == n_node = OpenSet ((n, n_val) : rest)
    | val > n_val = OpenSet ((n_node, n_val) : (n, val) : rest)
    | otherwise = OpenSet ((n, val) : n_entries)
      where OpenSet (n_entries) = insertOpenSet (OpenSet rest) (n_node, n_val)

  updateOpenSet :: Eq i => OpenSet i -> (i, Int) -> OpenSet i
  updateOpenSet (OpenSet ([])) val = error ""
  updateOpenSet (OpenSet ((n, val) : rest)) (n_node, n_val)
    | n == n_node = OpenSet ((n, n_val) : rest)
    | otherwise = OpenSet ((n, val) : n_entries)
      where OpenSet (n_entries) = updateOpenSet (OpenSet rest) (n_node, n_val)