{-# LANGUAGE TemplateHaskell, FlexibleContexts, LambdaCase #-}

module Lib where

import qualified Data.Array as Array
import Data.Array (Array, (//))

import qualified Data.Map as Map
import Data.Map (Map)

import Data.List

import Control.Lens

import Control.Monad.State

data Orientation = Vertical | Horizontal

newtype ShipKey = ShipKey (Int, Int) deriving (Show, Eq, Ord)
newtype ShipData = ShipData { _health :: Int
                         }
makeLenses ''ShipData
type Ship = (ShipKey, ShipData)

data BeenHit = WasHit | NotHit deriving (Show, Eq, Ord)

data GridCell = GridCell { _beenHit :: BeenHit
                         , _ship :: Maybe ShipKey
                         }
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
                           }
makeLenses ''GameState

instance Show GameState where
    show (GameState grid _) = showGrid grid
    
emptyState (h,w) = GameState (Array.listArray ((0,0),(h-1,w-1)) (repeat emptyGridCell)) Map.empty

--Does no checking that the target is valid or clear!
insertShip :: ShipKey -> Orientation -> Int -> GameState -> GameState
insertShip key@(ShipKey (y,x)) orientation length = execState $ do
    ships.at key.= Just (ShipData length)
    let gridUpdate = case orientation of
                    Vertical   -> [((y',x), GridCell NotHit $ Just key) | y' <- [y..y+length-1]]
                    Horizontal -> [((y,x'), GridCell NotHit $ Just key) | x' <- [x..x+length-1]]
    grid %= (// gridUpdate)

data ShotOutcome = Hit
                 | Miss
                 | AlreadyTaken
                 | Sunk
                 | Win
                 deriving (Show, Eq, Ord)

isLost :: GameState -> Bool
isLost (GameState _ ships) = all (\ ship -> ship^.health == 0) ships

fire :: (Int, Int) -> State GameState ShotOutcome
fire pos =
    preuse (grid.ix pos) >>= \case
        Nothing -> error "Fired off grid"
        Just (GridCell WasHit _) -> return AlreadyTaken
        Just (GridCell NotHit Nothing) -> do
            grid.ix pos.beenHit.= WasHit
            return Miss
        Just (GridCell NotHit (Just shipKey)) -> do
            grid.ix pos.beenHit.= WasHit
            ships.ix shipKey.health -= 1
            preuse (ships.ix shipKey.health) >>= \case
                Nothing -> error "Hit ship not in ships"
                Just 0 -> isLost <$> get >>= \case
                    True -> return Win
                    False -> return Sunk
                _ -> return Hit

