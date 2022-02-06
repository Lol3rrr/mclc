{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}

module Layout where
  import Data.Maybe
  import Data.List
  import Data.Proxy

  newtype Row a = Row [a] deriving Show;
  newtype Grid a = Grid [Row a] deriving Show;
  newtype Space a = Space [Grid a] deriving Show;

  type GridPosition = (Int, Int);
  type SpacePosition = (Int, Int, Int);

  class Container a i c | a -> c where
    isEmpty :: a -> (c -> Bool) -> i -> Bool
    getCell :: a -> i -> Maybe c
    setCell :: a -> (c -> c) -> i -> a

  instance Container (Row a) Int a where
    isEmpty (Row elems) f max = isJust (find f (take max elems))

    getCell (Row []) _ = Nothing
    getCell (Row (elem:elems)) 0 = Just elem
    getCell (Row (elem:elems)) x = getCell (Row elems) (x - 1)

    setCell (Row []) _ _ = error "Out of Bounds Row access"
    setCell (Row (elem:elems)) f 0 = Row ((f elem) : elems)
    setCell (Row (elem:elems)) f x = Row (elem : rest)
      where Row rest = setCell (Row elems) f (x - 1)

  instance Container (Grid a) GridPosition a where
    isEmpty (Grid rows) f (max_x, max_y) = isJust (find (\row -> isEmpty row f max_x) (take max_y rows))

    getCell (Grid []) _ = Nothing
    getCell (Grid (row:rows)) (x, 0) = getCell row x
    getCell (Grid (row:rows)) (x, y) = getCell (Grid rows) (x, y - 1)

    setCell (Grid []) _ _ = error "Out of Bounds Grid access"
    setCell (Grid (row:rows)) f (x, 0) = Grid ((setCell row f x) : rows)
    setCell (Grid (row:rows)) f (x, y) = Grid (row : rest)
      where Grid rest = setCell (Grid rows) f (x, y - 1)

  instance Container (Space a) SpacePosition a where
    isEmpty (Space layers) f (max_x, max_y, max_z) = isJust (find (\layer -> isEmpty layer f (max_x, max_y)) (take max_z layers))

    getCell (Space []) _ = Nothing
    getCell (Space (layer:layers)) (x, y, 0) = getCell layer (x, y)
    getCell (Space (layer:layers)) (x, y, z) = getCell (Space layers) (x, y, z - 1)

    setCell (Space []) _ _ = error "Out of Bounds Space access"
    setCell (Space (layer:layers)) f (x, y, 0) = Space ((setCell layer f (x, y)) : layers)
    setCell (Space (layer:layers)) f (x, y, z) = Space (layer : rest)
      where Space rest = setCell (Space layers) f (x, y, z - 1)

  infiniteRow :: a -> Row a
  infiniteRow f
    = Row (f : rest)
    where Row rest = infiniteRow f

  infiniteGrid :: a -> Grid a
  infiniteGrid f
    = Grid ((infiniteRow f) : rows)
    where Grid rows = infiniteGrid f

  mapRow :: (a -> b) -> Row a -> Row b
  mapRow f (Row elems) = Row (map f elems)

  mapGrid :: (a -> b) -> Grid a -> Grid b
  mapGrid f (Grid rows) = Grid (map (mapRow f) rows)

  takeRow :: Int -> Row a -> [a]
  takeRow n (Row elems) = take n elems

  takeGrid :: Int -> Grid a -> [Row a]
  takeGrid n (Grid rows) = take n rows

  positionRow :: Int -> Row a -> [(a, Int)]
  positionRow max (Row elems) = zip (take max elems) [0..]

  positionGrid :: GridPosition -> Grid a -> [(a, GridPosition)]
  positionGrid (max_x, max_y) (Grid rows)
    = concat nested_cells
    where numbered_rows = zip (take max_y rows) [0..] -- [(Row a, Int)]
          nested_rows = map (\(row, y) -> (positionRow max_x row, y)) numbered_rows -- [([a, Int], Int)]
          nested_cells = map (\(tmp, y) -> map (\(v, x) -> (v, (x, y))) tmp) nested_rows
        
  positionSpace :: SpacePosition -> Space a -> [(a, SpacePosition)]
  positionSpace (max_x, max_y, max_z) (Space layers)
    = concat nested_cells
    where numbered_rows = zip (take max_z layers) [0..] -- [(Row a, Int)]
          nested_rows = map (\(row, z) -> (positionGrid (max_x, max_y) row, z)) numbered_rows -- [([a, Int], Int)]
          nested_cells = map (\(tmp, z) -> map (\(v, (x, y)) -> (v, (x, y, z))) tmp) nested_rows

  rowSize :: Row a -> (a -> Bool) -> Int -> Int
  rowSize (Row elems) f max
    = case length_res of
      Just (_, l) -> l
      Nothing -> 0
    where length_res = (find (\(v, _) -> f v) (reverse (zip (take max elems) [1..])))

  gridSize :: Grid a -> (a -> Bool) -> GridPosition -> GridPosition
  gridSize (Grid rows) f (max_x, max_y)
    = (width, height)
    where height_res = (find (\(v, _) -> (isEmpty v f max_x)) (reverse (zip (take max_y rows) [1..])))
          height = case height_res of
            Just (_, h) -> h
            Nothing -> 1
          width = maximum (map (\row -> rowSize row f max_x) (take height rows))

  spaceSize :: Space a -> (a -> Bool) -> SpacePosition -> SpacePosition
  spaceSize (Space layers) f (max_x, max_y, max_z)
    = (width, height, depth)
    where (_, depth) = fromJust (find (\(v, _) -> (isEmpty v f (max_x, max_y))) (reverse (zip (take max_z layers) [1..])))
          width = maximum (map (\(width, height) -> width) width_height_list)
          height = maximum (map (\(width, height) -> height) width_height_list)
          width_height_list = map (\layer -> gridSize layer f (max_x, max_y)) (take depth layers)