{-# LANGUAGE TemplateHaskell, FlexibleContexts, LambdaCase #-}

module Lib where

import qualified Data.Array as Array
import Data.Array (Array, (//), (!))

import qualified Data.Map as Map
import Data.Map (Map)

import Data.List

import Safe

import Control.Lens

import Control.Monad
import Control.Monad.State
import Control.Monad.Trans.Maybe

import Control.Monad.Random

data Orientation = Vertical | Horizontal

newtype ShipKey = ShipKey (Int, Int) deriving (Show, Eq, Ord)
newtype ShipData = ShipData { _health :: Int
                            } deriving (Show, Eq, Ord)
makeLenses ''ShipData
type Ship = (ShipKey, ShipData)

data BeenHit = WasHit | NotHit deriving (Show, Eq, Ord)

data GridCell = GridCell { _beenHit :: BeenHit
                         , _ship :: Maybe ShipKey
                         } deriving (Eq, Ord)
makeLenses ''GridCell
instance Show GridCell where
    show (GridCell NotHit Nothing) = "~"
    show (GridCell NotHit (Just _)) = "+"
    show (GridCell WasHit Nothing) = "X"
    show (GridCell WasHit (Just _)) = "*"
    
emptyGridCell = GridCell NotHit Nothing

type Grid = Array (Int,Int) GridCell

showGrid :: Grid -> String
showGrid grid = intercalate "\n"
              $ map (foldMap (\ (_, cell) -> show cell))
              $ groupBy (\ ((y1,_),_) ((y2,_),_) -> y1 == y2)
              $ Array.assocs grid

data GameState = GameState { _grid :: Grid
                           , _ships :: Map ShipKey ShipData
                           } deriving (Eq, Ord)
makeLenses ''GameState

instance Show GameState where
    show (GameState grid _) = showGrid grid
    
emptyState (h,w) = GameState (Array.listArray ((0,0),(h-1,w-1)) (repeat emptyGridCell)) Map.empty

arrayGet :: (Array.Ix i) => i -> Array i a -> Maybe a
arrayGet i arr
  | Array.inRange (Array.bounds arr) i = Just $ arr ! i
  | otherwise = Nothing

arraySet :: (Array.Ix i) => i -> a -> Array i a -> Maybe (Array i a)
arraySet i a arr = arr // [(i, a)] <$ arrayGet i arr

addShipToCell :: ShipKey -> Grid -> (Int, Int) -> Maybe Grid
addShipToCell key grid (y,x) = do
  --Pattern match failure gives a nothing and protects from ships already there
  GridCell beenHit Nothing <- arrayGet (y,x) grid
  arraySet (y,x) (GridCell beenHit $ Just key) grid

insertShip :: ShipKey -> Orientation -> Int -> GameState -> Maybe GameState
insertShip key@(ShipKey (y,x)) orientation length (GameState grid ships) = let
      newShips = Map.insert key (ShipData length) ships
      shipCells = case orientation of
                      Vertical   -> [(y',x) | y' <- [y..y+length-1]]
                      Horizontal -> [(y,x') | x' <- [x..x+length-1]]
      newGrid = foldM (addShipToCell key) grid shipCells
   in (\ g -> GameState g newShips) <$> newGrid

data ShotOutcome = Hit
                 | Miss
                 | AlreadyTaken
                 | OffGrid
                 | Sunk
                 | Win
                 deriving (Show, Eq, Ord)

isLost :: Map ShipKey ShipData -> Bool
isLost = all (\ ship -> ship^.health == 0) 

fire :: (Int, Int) -> GameState -> (ShotOutcome, GameState)
fire pos gameState@(GameState grid ships) =
  case arrayGet pos grid of
    Nothing -> (OffGrid, gameState)
    Just (GridCell WasHit _) -> (AlreadyTaken, gameState)
    Just (GridCell NotHit Nothing) -> let
      newGrid = fromJustNote "pos stopped being valid halfway through" $
        arraySet pos (GridCell WasHit Nothing) grid
     in (Miss, GameState newGrid ships)
    Just (GridCell NotHit (Just shipKey)) -> let
      newGrid = fromJustNote "pos stopped being valid halfway through" $
        arraySet pos (GridCell WasHit $ Just shipKey) grid
      newShips = Map.adjust (\ (ShipData health) -> ShipData $ health - 1) shipKey ships
      outcome = case Map.lookup shipKey newShips of
                  Nothing -> error "Hit ship not in ships"
                  Just (ShipData 0) -> if isLost newShips
                               then Win
                               else Sunk
                  _ -> Hit
      in (outcome, GameState newGrid newShips)

oldFire :: (Int, Int) -> State GameState ShotOutcome
oldFire pos =
    preuse (grid.ix pos) >>= \case
        Nothing -> return OffGrid
        Just (GridCell WasHit _) -> return AlreadyTaken
        Just (GridCell NotHit Nothing) -> do
            grid.ix pos.beenHit.= WasHit
            return Miss
        Just (GridCell NotHit (Just shipKey)) -> do
            grid.ix pos.beenHit.= WasHit
            ships.ix shipKey.health -= 1
            preuse (ships.ix shipKey.health) >>= \case
                Nothing -> error "Hit ship not in ships"
                Just 0 -> isLost <$> use ships >>= \case
                    True -> return Win
                    False -> return Sunk
                _ -> return Hit

--Not guarenteed to terminate.
--Given a way to lazily generate a permutation we could fix that, but 
--for now attempting to add a ship if it won't fit is an error.
addRandomShip :: MonadRandom m => Int -> GameState -> m GameState
addRandomShip l initialState@(GameState grid ships) = let
  --Not height and width (!): haskell array bounds are inclusive.
  ((0,0),(gridMaxY,gridMaxX)) = Array.bounds grid
  (h,w) = (gridMaxY + 1, gridMaxX + 1)
  randOrientation =  case (h >= l, w >= l) of
                     (True, True) -> (\ b -> if b then Horizontal else Vertical) <$> getRandom
                     (True, False) -> return Vertical
                     (False, True) -> return Horizontal
                     (False, False) -> error "Ship doesn't fit"
  go = do
    orientation <- randOrientation
    pos <- case orientation of
               Horizontal -> (,) <$> getRandomR (0, h-1) <*> getRandomR (0, w-l)
               Vertical   -> (,) <$> getRandomR (0, h-l) <*> getRandomR (0, w-1)
    let newState = insertShip (ShipKey pos) orientation l  initialState
    maybe go return newState
  in go

stateWithShips ::MonadRandom m => (Int, Int) -> m GameState
stateWithShips dims = foldM (flip addRandomShip) (emptyState dims) [4,3,3]


