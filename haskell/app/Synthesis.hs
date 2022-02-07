{-# LANGUAGE OverloadedStrings #-}

module Synthesis where
  import qualified Semantics
  import qualified AStar
  import qualified EntityGraph
  import qualified BuiltinGraph
  import qualified NormalizedGraph
  import qualified Layout

  import Data.Maybe
  import Data.List
  import qualified Data.Text
  import Data.Map (Map)
  import qualified Data.Map as Map

  import Debug.Trace

  import Graphics.Svg

  data SynthError = Other String deriving (Show);
  data SynthResult a = Ok a | Error SynthError deriving (Show);

  instance Functor SynthResult where
    fmap f (Ok x) = Ok (f x)

  instance Applicative SynthResult where
    pure = Ok
    Ok f <*> Ok x = Ok (f x)

  instance Monad SynthResult where
    (Error err) >>= f = Error err
    (Ok x) >>= f = f x
    return = pure
  
  unwrap :: SynthResult a -> a
  unwrap (Ok v) = v
  unwrap (Error err) = error (show err)


  data Orientation = North | East | South | West deriving (Show, Eq);
  data BlockColor = Red | Green | Blue | Pink deriving (Show, Eq);
  data CellBlock
    = SolidBlock
    | ColoredSolid BlockColor
    | RedStone
    | TorchOnBlock Orientation
    | Repeater Orientation
    | Comparitor Orientation Bool
    deriving (Show, Eq);
  data GridCell = Empty | Reserved | Full CellBlock deriving (Show, Eq);
  type GridPosition = Layout.GridPosition;
  type Grid = Layout.Grid GridCell;
  type SpacePosition = Layout.SpacePosition;
  type Space = Layout.Space GridCell;

  data PlaceNode
    = Input String GridPosition
    | Output String GridPosition
    | Variable (String, Int) GridPosition
    | Entity Int [GridPosition] [GridPosition]
    deriving (Show);

  instance AStar.AsGridCell GridCell where
    asCell Empty = AStar.Free
    asCell _ = AStar.Used

  synthesis :: [Semantics.Entity] -> Semantics.Entity -> SynthResult Space
  synthesis other_entities target_e = synthEntity other_entities target_e

  synthEntity :: [Semantics.Entity] -> Semantics.Entity -> SynthResult Space
  synthEntity others e =
    do
      let entity_graph = EntityGraph.build e;
      let builtin_graph = BuiltinGraph.fromEntityGraph others entity_graph;
      let normalized_graph = NormalizedGraph.fromBuiltin builtin_graph;
      
      return (trace (NormalizedGraph.toString normalized_graph) (generatePlacement normalized_graph));

  rmdups :: Eq a => [a] -> [a]
  rmdups [] = []
  rmdups (x:xs) = x : filter (/= x) (rmdups xs)

  -- https://en.wikipedia.org/wiki/Taxicab_geometry
  manhattenDistance :: GridPosition -> GridPosition -> Int
  manhattenDistance (x1, y1) (x2, y2) = (abs (x1 - x2)) + (abs (y1 - y2))

  manhattenDistance3D :: SpacePosition -> SpacePosition -> Int
  manhattenDistance3D (x1, y1, z1) (x2, y2, z2) = ((x2 - x1)) + ((y2 - y1)) + ((z2 - z1))

  aStarNeighbours3D :: Space -> (SpacePosition, Maybe SpacePosition) -> [(SpacePosition, Int)]
  aStarNeighbours3D space (pos, _)
    = placeable_neighbours
    where base_neighbours = baseNeighbours space pos
          in_grid_neighbours = (filter (\((x, y, z), _) -> x >= 0 && y >= 0 && z >= 0) base_neighbours)
          free_neighbours = filter (\(pos, _) -> isSpaceCellFree space pos) in_grid_neighbours
          placeable_neighbours = filter (\((x, y, z), _) -> case (Layout.getCell space (x, y, z + 1)) of
            Just Empty -> True
            Just (Full SolidBlock) -> True
            Just (Full (ColoredSolid _)) -> True
            _ -> False) free_neighbours

  baseNeighbours :: Space -> SpacePosition -> [(SpacePosition, Int)]
  baseNeighbours space (x, y, z)
    = (map (\pos -> (pos, 1)) base_cords)
        ++ (map (\(nx, ny, nz) -> ((nx, ny, nz  + 1), 2)) base_cords)
        ++ (map (\(nx, ny, nz) -> ((nx, ny, nz  - 1), 2)) (filter (\_ -> top_free) base_cords))
    where base_cords = [(x + 1, y, z), (x - 1, y, z), (x, y + 1, z), (x, y - 1, z)]
          is_free = (\pos -> case (Layout.getCell space pos) of
            Just Empty -> True
            Just Reserved -> True
            _ -> False)
          top_free = is_free (x, y, z-1)

  isSpaceCellFree :: Space -> SpacePosition -> Bool
  isSpaceCellFree space pos = case (Layout.getCell space pos) of
    (Just raw_cell) -> (raw_cell == Empty)
    Nothing -> True

  isSpaceCellUseable :: Space -> SpacePosition -> Bool
  isSpaceCellUseable space (x, y, z)
    = (isSpaceCellFree space pos) && bottom_free && (not other_used)
    where pos = (x, y, z)
          bottom_free = case (Layout.getCell space (x, y, z + 1)) of
            Just Empty -> True
            Just (Full SolidBlock) -> True
            Just (Full (ColoredSolid _)) -> True
            _ -> False
          --top_ring = [(x + 1, y, z + 1), (x - 1, y, z + 1), (x, y + 1, z + 1), (x, y - 1, z + 1)]
          --bottom_ring = [(x + 1, y, z - 1), (x - 1, y, z - 1), (x, y + 1, z - 1), (x, y - 1, z - 1)]
          surrounding = [] ++ [] :: [SpacePosition]
          other_used = any (\tmp_p -> case (Layout.getCell space tmp_p) of
            Just (Full RedStone) -> True
            _ -> False) surrounding
  
  shortestSpacePath :: Space -> SpacePosition -> SpacePosition -> [SpacePosition]
  shortestSpacePath space start dest
    = AStar.startAStar manhattenDistance3D aStarNeighbours3D space start dest

  setGridCell :: Grid -> GridPosition -> (GridCell -> GridCell) -> Grid
  setGridCell grid pos f = Layout.setCell grid f pos

  setGridCellsValue :: Grid -> [(GridPosition, GridCell)] -> Grid
  setGridCellsValue grid [] = grid
  setGridCellsValue grid ((pos, value):others) = setGridCellsValue (setGridCell grid pos (\_ -> value)) others

  setGridCells :: Grid -> [GridPosition] -> (GridCell -> GridCell) -> Grid
  setGridCells grid [] _ = grid
  setGridCells grid (pos:others) value_f = setGridCells (setGridCell grid pos value_f) others value_f

  setSpaceCells :: Space -> [SpacePosition] -> (GridCell -> GridCell) -> Space
  setSpaceCells world [] _ = world
  setSpaceCells world (pos:others) f = setSpaceCells (Layout.setCell world f pos) others f

  paddingLayers :: Int
  paddingLayers = 8

  generatePlacement :: NormalizedGraph.Graph -> Space
  generatePlacement (NormalizedGraph.Graph (nodes, edges))
    = result_space
    where (result, placed_nodes) = placeNodes (Layout.infiniteGrid Empty, []) (NormalizedGraph.Graph (nodes, edges)) 1 nodes
          underlaying_grid = Layout.mapGrid (\x -> case x of
            (Full RedStone) -> Full SolidBlock
            (Full (Repeater _)) -> Full SolidBlock
            (Full (Comparitor _ _)) -> Full SolidBlock
            other -> Empty) result
          top_grid = Layout.infiniteGrid Empty
          top_padding_layers = take paddingLayers (repeat (Layout.infiniteGrid Empty))
          bottom_padding_layers = take (250 - paddingLayers - 3) (repeat (Layout.infiniteGrid Empty))
          initial_space = Layout.Space (top_padding_layers ++ [top_grid, result, underlaying_grid] ++ bottom_padding_layers)
          result_space = connectNodes initial_space edges placed_nodes
  
  connectNodes :: Space -> [NormalizedGraph.GraphEdge] -> [PlaceNode] -> Space
  connectNodes world [] _ = world
  connectNodes world ((NormalizedGraph.InputToNode (NormalizedGraph.InputID input_name) dest_id dest_port):edges) placed
    = connectNodes connected_world edges placed
    where Input _ in_pos = fromJust (find (\p -> case p of
            Input name _ -> name == input_name
            _ -> False) placed)
          port_pos = getDestNodePosition dest_id dest_port placed
          connected_world = connectNodePorts world in_pos port_pos
  connectNodes world ((NormalizedGraph.NodeToNode src_id src_port dest_id dest_port):edges) placed
    = connectNodes connected_world edges placed
    where src_port_pos = getSrcNodePosition src_id src_port placed
          dest_port_pos = getDestNodePosition dest_id dest_port placed
          connected_world = connectNodePorts world src_port_pos dest_port_pos
  connectNodes world ((NormalizedGraph.NodeToOutput src_id src_port (NormalizedGraph.OutputID dest_name)):edges) placed
    = connectNodes connected_world edges placed
    where src_port_pos = getSrcNodePosition src_id src_port placed
          Output _ dest_pos = fromJust (find (\p -> case p of
            Output name _ -> name == dest_name
            _ -> False) placed)
          connected_world = connectNodePorts world src_port_pos dest_pos
  connectNodes world (edge:edges) placed = error (show edge)

  getSrcNodePosition :: NormalizedGraph.GraphNodeID -> Int -> [PlaceNode] -> GridPosition
  getSrcNodePosition (NormalizedGraph.EntityID id) port placed
    = head (drop port src_ports)
    where Entity _ _ src_ports = fromJust (find (\p -> case p of
            Entity eid _ _ -> eid == id
            _ -> False) placed)
  getSrcNodePosition (NormalizedGraph.SplitterID id) port placed
    = head (drop port src_ports)
    where Entity _ _ src_ports = fromJust (find (\p -> case p of
            Entity eid _ _ -> eid == id
            _ -> False) placed)
  getSrcNodePosition (NormalizedGraph.VariableID src_name src_id) _ placed
    = src_pos
    where Variable _ src_pos = fromJust (find (\p -> case p of
            Variable (name, id) _ -> name == src_name && id == src_id
            _ -> False) placed)

  getDestNodePosition :: NormalizedGraph.GraphNodeID -> Int -> [PlaceNode] -> GridPosition
  getDestNodePosition (NormalizedGraph.EntityID id) port placed
    = head (drop port dest_ports)
    where Entity _ dest_ports _ = fromJust (find (\p -> case p of
            Entity eid _ _ -> eid == id
            _ -> False) placed)
  getDestNodePosition (NormalizedGraph.SplitterID dest_id) dest_port placed
    = head (drop dest_port dest_ports)
    where Entity _ dest_ports _ = fromJust (find (\p -> case p of
            Entity id _ _ -> id == dest_id
            _ -> False) placed)
  getDestNodePosition (NormalizedGraph.VariableID dest_name dest_id) _ placed
    = dest_pos
    where Variable _ dest_pos = fromJust (find (\p -> case p of
            Variable (name, id) _ -> name == dest_name && id == dest_id
            _ -> False) placed)

  connectNodePorts :: Space -> GridPosition -> GridPosition -> Space
  connectNodePorts world (ix, iy) (px, py)
    = connected_world
    where base_z = paddingLayers + 1
          port_x_offsets = take (portReserveSize) [1..]
          src_reserved_area = (map (\x_off -> (ix + x_off, iy, base_z)) port_x_offsets)
          --  ++ (map (\x_off -> (ix + x_off, iy + 1, base_z)) port_x_offsets)
          --  ++ (map (\x_off -> (ix + x_off, iy - 1, base_z)) port_x_offsets)
          src_freed = setSpaceCells world src_reserved_area (\_ -> Empty)
          dest_reserved_area = map (\x_off -> (px - x_off, py, base_z)) port_x_offsets
          dest_freed = setSpaceCells src_freed dest_reserved_area (\_ -> Empty)
          connection_path = shortestSpacePath dest_freed (ix+1, iy, base_z) (px-1, py, base_z)
          connected_world = placePath dest_freed connection_path

  placePath :: Space -> [SpacePosition] -> Space
  placePath world [] = world
  placePath world ((x, y, z):rest_pos)
    = placePath final_world rest_pos
    where place_f = (\c -> case c of
            Empty -> Reserved
            other -> other)
          to_reserve = [
            (x, y, z - 1),
            (x + 1, y, z), (x - 1, y, z), (x, y + 1, z), (x, y - 1, z)
            ,(x + 1, y, z + 1), (x - 1, y, z + 1), (x, y + 1, z + 1), (x, y - 1, z + 1)
            ,(x + 1, y, z - 1), (x - 1, y, z - 1), (x, y + 1, z - 1), (x, y - 1, z - 1)
            ]
          solid_pos = (x, y, z + 1)
          redstone_pos = (x, y, z)
          reserverd_world = setSpaceCells world to_reserve place_f
          redstone_world = Layout.setCell reserverd_world (\cell -> Full RedStone) redstone_pos
          final_world = Layout.setCell redstone_world (\_ -> Full (ColoredSolid Red)) solid_pos

  columnSpacing :: Int
  columnSpacing = 8

  placeNodes :: (Grid, [PlaceNode]) -> NormalizedGraph.Graph -> Int -> [NormalizedGraph.GraphNode] -> (Grid, [PlaceNode])
  placeNodes x _ _ [] = x
  placeNodes (grid, placed) (NormalizedGraph.Graph (nodes, edges)) x_offset to_place
    = placeNodes (n_grid, n_placed) graph (x_offset + max_width + columnSpacing) new_to_place
    where graph = (NormalizedGraph.Graph (nodes, edges))
          node_preds = map (\n -> (n, NormalizedGraph.nodePredecessors graph n)) to_place
          filtered_node_preds = map (\(n, preds) -> (n, filter (\p -> isJust (find (\tp -> p == tp) to_place)) preds)) node_preds
          placeable = map (\(n, _) -> n) (filter (\(n, preds) -> null preds) filtered_node_preds)
          (n_grid, n_placed, _) = foldl
            (placeNode)
            (grid, placed, (x_offset, 1))
            placeable
          max_width = (maximum (map nodeWidth placeable)) + 2*portReserveSize
          new_to_place = filter (\tp -> isNothing (find (\p -> tp == p) placeable)) to_place

  nodeWidth :: NormalizedGraph.GraphNode -> Int
  nodeWidth (NormalizedGraph.GraphNode (NormalizedGraph.InputID id, _)) = 1
  nodeWidth (NormalizedGraph.GraphNode (NormalizedGraph.OutputID id, _)) = 1
  nodeWidth (NormalizedGraph.GraphNode (NormalizedGraph.VariableID name id, _)) = 1
  nodeWidth (NormalizedGraph.GraphNode (NormalizedGraph.SplitterID id, NormalizedGraph.SplitterNode port_count)) = 3
  nodeWidth (NormalizedGraph.GraphNode (NormalizedGraph.EntityID id, NormalizedGraph.OperationNode "not")) = 5
  nodeWidth (NormalizedGraph.GraphNode (NormalizedGraph.EntityID id, NormalizedGraph.OperationNode "and")) = 4
  nodeWidth (NormalizedGraph.GraphNode (NormalizedGraph.EntityID id, NormalizedGraph.OperationNode "or")) = 4
  nodeWidth (NormalizedGraph.GraphNode (NormalizedGraph.EntityID id, NormalizedGraph.OperationNode "xor")) = 7
  nodeWidth (NormalizedGraph.GraphNode (NormalizedGraph.EntityID id, NormalizedGraph.VariableNode _)) = 1
  nodeWidth other = error (show other)

  portReserveSize :: Int
  portReserveSize = 4

  reserveInnerBlock :: Grid -> GridPosition -> (Int, Int) -> Grid
  reserveInnerBlock grid (x, y) (width, height)
    = setGridCells grid overall_cords (\cell -> case cell of
        Empty -> Reserved
        Reserved -> Reserved
        --Full x -> Reserved
        Full unknown -> error ((show x) ++ " - " ++ (show y) ++ ": " ++ show unknown)
        )
    where length_extension = portReserveSize - 1
          length_wise_offsets = take (width + 2 * length_extension) [(- length_extension)..]
          top_cords = map (\x_offset -> (x + x_offset, y - 1)) length_wise_offsets
          bottom_cords = map (\x_offset -> (x + x_offset, y + height)) length_wise_offsets
          height_wise_offsets = take (height) [0..]
          left_end_cords = map (\y_offset -> (x - portReserveSize, y + y_offset)) height_wise_offsets
          right_end_cords = map (\y_offset -> (x + width + portReserveSize, y + y_offset)) height_wise_offsets
          overall_cords = top_cords ++ bottom_cords ++ left_end_cords ++ right_end_cords

  placeNode :: (Grid, [PlaceNode], GridPosition) -> NormalizedGraph.GraphNode -> (Grid, [PlaceNode], GridPosition)
  placeNode (grid, placed, (x, y)) (NormalizedGraph.GraphNode (NormalizedGraph.InputID id, _))
    = (reserved_grid, (Input id (x, y)) : placed, (x, y + 10))
    where fulled_grid = setGridCell grid (x, y) (\_ -> Full RedStone)
          reserved_grid = reserveInnerBlock fulled_grid (x, y) (1, 1)
  placeNode (grid, placed, (x, y)) (NormalizedGraph.GraphNode (NormalizedGraph.OutputID id, _))
    = (reserved_grid, (Output id (x, y)) : placed, (x, y + 10))
    where fulled_grid = setGridCell grid (x, y) (\_ -> Full RedStone)
          reserved_grid = reserveInnerBlock fulled_grid (x, y) (1, 1)
  placeNode (grid, placed, (x, y)) (NormalizedGraph.GraphNode (NormalizedGraph.VariableID name id, _))
    = (reserved_grid, (Variable (name, id) (x, y)) : placed, (x, y + 10))
    where fulled_grid = setGridCell grid (x, y) (\_ -> Full RedStone)
          reserved_grid = reserveInnerBlock fulled_grid (x, y) (1, 1)
  placeNode (grid, placed, (x, y)) (NormalizedGraph.GraphNode (NormalizedGraph.EntityID id, NormalizedGraph.VariableNode _))
    = (reserved_grid, (Entity id [(x, y)] [(x, y)]) : placed, (x, y + 10))
    where fulled_grid = setGridCell grid (x, y) (\_ -> Full RedStone)
          reserved_grid = reserveInnerBlock fulled_grid (x, y) (1, 1)
  placeNode (grid, placed, (x, y)) (NormalizedGraph.GraphNode (NormalizedGraph.EntityID id, NormalizedGraph.OperationNode "not"))
    = (reserved_grid, (Entity id [(x, y)] [(x+4, y)]) : placed, (x, y + 10))
    where input_grid = setGridCell grid (x, y) (\_ -> Full RedStone)
          repeater_grid = setGridCell input_grid (x + 1, y) (\_ -> Full (Repeater East))
          block_grid = setGridCell repeater_grid (x + 2, y) (\_ -> Full SolidBlock)
          torch_grid = setGridCell block_grid (x + 3, y) (\_ -> Full (TorchOnBlock West))
          dust_grid = setGridCell torch_grid (x + 4, y) (\_ -> Full RedStone)
          reserved_grid = reserveInnerBlock dust_grid (x, y) (5, 1)
  placeNode (grid, placed, (x, y)) (NormalizedGraph.GraphNode (NormalizedGraph.EntityID id, NormalizedGraph.OperationNode "and"))
    = (reserved_grid, (Entity id [(x, y), (x, y + 2)] [(x+3, y + 1)]) : placed, (x, y + 10))
    where input_grid = setGridCellsValue grid [((x, y), Full RedStone), ((x, y + 2), Full RedStone), ((x + 1, y), Full (Repeater East)), ((x + 1, y + 2), Full (Repeater East))]
          block_grid = setGridCellsValue input_grid [((x + 2, y), Full SolidBlock), ((x + 2, y + 2), Full SolidBlock)]
          torches_grid = setGridCellsValue block_grid [((x + 3, y), Full (TorchOnBlock West)), ((x + 3, y + 2), Full (TorchOnBlock West))]
          output_grid = setGridCellsValue torches_grid [((x + 3, y + 1), Full RedStone), ((x + 4, y + 1), Full RedStone)]
          reserved_grid = reserveInnerBlock output_grid (x, y) (4, 3)
  placeNode (grid, placed, (x, y)) (NormalizedGraph.GraphNode (NormalizedGraph.EntityID id, NormalizedGraph.OperationNode "or"))
    = (reserved_grid, (Entity id [(x, y), (x, y + 2)] [(x+3, y + 1)]) : placed, (x, y + 10))
    where input_grid = setGridCellsValue grid [((x, y), Full RedStone), ((x, y + 2), Full RedStone)]
          logic_grid = setGridCellsValue input_grid [((x + 1, y), Full RedStone), ((x + 1, y + 1), Full RedStone), ((x + 1, y + 2), Full RedStone), ((x + 2, y + 1), Full RedStone)]
          output_grid = setGridCellsValue logic_grid [((x + 3, y + 1), Full RedStone)]
          reserved_grid = reserveInnerBlock output_grid (x, y) (4, 3)
  placeNode (grid, placed, (x, y)) (NormalizedGraph.GraphNode (NormalizedGraph.EntityID id, NormalizedGraph.OperationNode "xor"))
    = (reserved_grid, (Entity id [(x, y), (x, y + 3)] [(x+6, y + 1)]) : placed, (x, y + 10))
    where input_grid = setGridCellsValue grid [((x, y), Full RedStone), ((x, y + 3), Full RedStone), ((x + 1, y), Full (Repeater East)), ((x + 1, y + 3), Full (Repeater East))]
          block_grid = setGridCells input_grid [(x + 2, y), (x + 2, y + 1), (x + 2, y + 2), (x + 2, y + 3)] (\_ -> Full SolidBlock)
          logic_dust_grid = setGridCells block_grid [(x + 3, y), (x + 3, y + 1), (x + 3, y + 2), (x + 3, y + 3), (x + 4, y), (x + 4, y + 3), (x + 5, y + 1), (x + 5, y + 2)] (\_ -> Full RedStone)
          logic_comparitor_grid = setGridCells logic_dust_grid [(x + 4, y + 1), (x + 4, y + 2)] (\_ -> Full (Comparitor East True))
          output_grid = setGridCells logic_comparitor_grid [(x + 6, y + 1)] (\_ -> Full RedStone)
          reserved_grid = reserveInnerBlock output_grid (x, y) (7, 4)
  placeNode (grid, placed, (x, y)) (NormalizedGraph.GraphNode (NormalizedGraph.SplitterID id, NormalizedGraph.SplitterNode port_count))
    = (reserved_grid, (Entity id [(x, y + input_y)] port_cords) : placed, (x, y + height + 10))
    where port_y_offsets = map (* 2) (take port_count [0..])
          height = 1 + (port_count - 1) * 2
          input_y = div (height - 1) 2
          input_grid = setGridCellsValue grid [((x, y + input_y), Full RedStone)]
          connecting_grid = setGridCellsValue input_grid (map (\y_off -> ((x + 1, y + y_off), Full RedStone)) (take height [0..]))
          output_grid = setGridCellsValue connecting_grid (map (\y_off -> ((x + 2, y + y_off), Full RedStone)) port_y_offsets)
          reserved_grid = reserveInnerBlock output_grid (x, y) (3, height)
          port_cords = map (\(y_off) -> (x + 2, y + y_off)) port_y_offsets
  
  placeNode (_, _, pos) node = error ((show pos) ++ " - Place: " ++ (show node))

  generateSpaceImage :: Space -> Element
  generateSpaceImage (Layout.Space []) = error "Empty Space"
  generateSpaceImage (Layout.Space grids)
    = doctype
      <> with (svg11_ content) [Version_ <<- "1.1", Width_ <<- Data.Text.pack (show (width * scale)), Height_ <<- Data.Text.pack (show (height * scale * depth + 50 * (depth-1)))]
    where (width, height, depth) = Layout.spaceSize (Layout.Space grids) (\x -> x /= Empty) (10000, 10000, 10000)
          content = foldl1 (<>) (generateSpaceImageLayer 0 scale (width, height, depth) grids)
          scale = 20

  generateSpaceImageLayer :: Int -> Int -> (Int, Int, Int) -> [Grid] -> [Element]
  generateSpaceImageLayer _ _ _ [] = []
  generateSpaceImageLayer y_offset scale (width, height, depth) (layer:layers)
    = (generateGridContent (0, y_offset) scale (width, height) layer) : (generateSpaceImageLayer (y_offset + (height * scale) + 50) scale (width, height, depth) layers)

  generateGridImage :: Grid -> Element
  generateGridImage grid
    = doctype
      <> with (svg11_ content) [Version_ <<- "1.1", Width_ <<- Data.Text.pack (show (width * scale)), Height_ <<- Data.Text.pack (show (height * scale))]
    where (width, height) = Layout.gridSize grid (\x -> x /= Empty) (10000, 10000)
          content = generateGridContent (0, 0) scale (width, height) grid
          scale = 20

  emptyElement :: Element
  emptyElement = path_ [ Fill_ <<- "transparent", Stroke_ <<- "#000000", D_ <<- mA 0 0]

  generateGridContent :: (Int, Int) -> Int -> (Int, Int) -> Grid -> Element
  generateGridContent (ix, iy) scale (width, height) grid
    = component_content <> grid_struct
    where grid_content = gridContent (ix, iy) scale grid
          component_content = foldl (\left right -> left <> right) emptyElement (grid_content)
          grid_struct = generateGrid (ix, iy) scale (width, height)
  
  generateGrid :: (Int, Int) -> Int -> (Int, Int) -> Element
  generateGrid (ix, iy) scale (max_x, max_y)
    = path_ [ Fill_ <<- "transparent", Stroke_ <<- "#000000", D_ <<- content]
    where vertical_lines = map
                    (\y -> (mA 0 (fromIntegral y :: Float) <> lA (fromIntegral (max_x * scale) :: Float) (fromIntegral y :: Float)))
                    (map (+ iy) (map (* scale) (take (max_y - 1) [1..])))
          horizontal_lines = map
                    (\x -> (mA (fromIntegral x :: Float) (fromIntegral iy :: Float) <> lA (fromIntegral x :: Float) (fromIntegral (iy + max_y * scale) :: Float)))
                    (map (+ ix) (map (* scale) (take (max_x - 1) [1..])))
          fused_vertical = foldl1 (<>) vertical_lines
          fused_horizontal = foldl1 (<>) horizontal_lines
          content = fused_vertical <> fused_horizontal

  gridContent :: (Int, Int) -> Int -> Grid -> [Element]
  gridContent (ix, iy) scale grid
    = map fromJust (filter isJust (map map_cell_f numbered_cells))
    where (width, height) = Layout.gridSize grid (\x -> x /= Empty) (10000, 10000)
          numbered_cells = Layout.positionGrid (width, height) grid
          map_cell_f
            = (\(cell, (x, y)) -> case cell of
                Empty -> Nothing
                Reserved -> Nothing
                Full b -> Just (cellContent scale b ((x * scale) + ix, (y * scale) + iy)))

  numbText numb = Data.Text.pack (show numb)

  cellContent :: Int -> CellBlock -> (Int, Int) -> Element
  cellContent scale SolidBlock (x, y)
    = rect_   [ X_ <<- (numbText x), Y_ <<- (numbText y), Width_ <<- (numbText scale), Height_ <<- (numbText scale), Fill_ <<- "gray"]
  cellContent scale (ColoredSolid Red) (x, y)
    = rect_   [ X_ <<- (numbText x), Y_ <<- (numbText y), Width_ <<- (numbText scale), Height_ <<- (numbText scale), Fill_ <<- "#FF0000"]
  cellContent scale RedStone (x_pos, y_pos)
    = rect_   [ X_ <<- (numbText x), Y_ <<- (numbText y), Width_ <<- (numbText size), Height_ <<- (numbText size), Fill_ <<- "#DD0000"]
    where x = x_pos + (div size 2)
          y = y_pos + (div size 2)
          size = div scale 2
  cellContent scale (Repeater East) (x_pos, y_pos)
    = rect_   [ X_ <<- (numbText x_base), Y_ <<- (numbText y_base), Width_ <<- (numbText base_size), Height_ <<- (numbText base_size), Fill_ <<- "gray"]
      <> rect_   [ X_ <<- (numbText torch1_x), Y_ <<- (numbText torch_y), Width_ <<- (numbText torch_size), Height_ <<- (numbText torch_size), Fill_ <<- "#DD0000"]
      <> rect_   [ X_ <<- (numbText torch2_x), Y_ <<- (numbText torch_y), Width_ <<- (numbText torch_size), Height_ <<- (numbText torch_size), Fill_ <<- "#DD0000"]
    where base_size = scale
          torch_size = div scale 4
          x_base = x_pos
          y_base = y_pos
          torch1_x = x_base + base_size - torch_size
          torch2_x = x_base + base_size - (3 * torch_size)
          torch_y = y_base + (div base_size 2) - (div torch_size 2)
  cellContent scale (TorchOnBlock West) (x_pos, y_pos)
    = rect_   [ X_ <<- x_str, Y_ <<- y_str, Width_ <<- (numbText size), Height_ <<- (numbText size), Fill_ <<- "#DD0000"]
    --  <> text_   [ X_ <<- x_str, Y_ <<- (numbText (y + scale)), Font_size_ <<- "9", Text_anchor_ <<- "left", Fill_ <<- "#000000"] "T"
    where size = div scale 2
          x = x_pos
          y = y_pos + (div size 2)
          x_str = Data.Text.pack (show x)
          y_str = Data.Text.pack (show y)
  cellContent scale (Comparitor East True) (x_pos, y_pos)
    = rect_   [ X_ <<- (numbText x_base), Y_ <<- (numbText y_base), Width_ <<- (numbText base_size), Height_ <<- (numbText base_size), Fill_ <<- "#123123"]
    where base_size = scale
          x_base = x_pos
          y_base = y_pos
  cellContent _ other pos = error ((show pos) ++ ": " ++ (show other))

  data MinecraftBlockType
    = Stone
    | RedstoneDust
    | RedstoneTorch (Maybe Orientation)
    | RedstoneRepeater Orientation
    | RedstoneComparitor Orientation Bool
    deriving Show;
  newtype MinecraftBlock = MinecraftBlock (SpacePosition, MinecraftBlockType) deriving Show;

  blockPlacement :: Space -> [MinecraftBlock]
  blockPlacement space
    = map (\(pos, c) -> MinecraftBlock (pos, blockFromCell c)) ordered_blocks
    where raw_cells = Layout.positionSpace (10000, 10000, 10000) space
          filtered_cells = filter (\(cell, _) -> case cell of
            Full _ -> True
            _ -> False) raw_cells
          cellBlocks = map (\(c, pos) -> (pos, case c of
            Full x -> x
            _ -> error "Unexpected")) filtered_cells
          solid_blocks = filter (\(_, c) -> case c of
            SolidBlock -> True
            ColoredSolid _ -> True
            _ -> False) cellBlocks
          rest_blocks = filter (\(_, c) -> case c of
            SolidBlock -> False
            ColoredSolid _ -> False
            _ -> True) cellBlocks
          ordered_blocks = solid_blocks ++ rest_blocks
  
  blockFromCell :: CellBlock -> MinecraftBlockType
  blockFromCell SolidBlock = Stone
  blockFromCell (ColoredSolid _) = Stone
  blockFromCell RedStone = RedstoneDust
  blockFromCell (TorchOnBlock or) = RedstoneTorch (Just or)
  blockFromCell (Repeater or) = RedstoneRepeater or
  blockFromCell (Comparitor or active) = RedstoneComparitor or active