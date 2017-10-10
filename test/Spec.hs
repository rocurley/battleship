{-# LANGUAGE LambdaCase #-}

import Lib

import Control.Monad.State

import qualified Data.Map as Map
import Data.Map (Map)

import Safe

import Data.Maybe
import Data.Foldable
import Data.List

import Control.Monad.Random

import Test.Tasty
import Test.Tasty.HUnit
import Test.Hspec.Expectations
import Test.Tasty.QuickCheck as QC
import Test.QuickCheck.Random

main :: IO ()
main = defaultMain tests

tests :: TestTree
tests = testGroup "Tests" [unitTests, propertyTests]

propertyTests :: TestTree
propertyTests = testGroup "QuickCheck Tests" [singleRandomShipTest, stateWithShipsTest, randomFiringTest]

unitTests :: TestTree
unitTests = testGroup "Unit Tests" [shotHistoryTest, oldShotHistoryTest]

counts :: (Ord a, Foldable t) => t a -> Map a Int
counts = foldl' (\ acc k -> Map.insertWith (+) k 1 acc) Map.empty

shipLengthsMatch :: [Int] -> GameState -> Bool
shipLengthsMatch lengths (GameState grid ships) = let
  cellCounts = counts $ mapMaybe (\ (GridCell _ maybeShipKey) -> maybeShipKey) $ toList grid
  shipHealths = (\ (ShipData health) -> health) <$> ships
  in cellCounts == shipHealths && counts shipHealths == counts lengths

singleRandomShipTest :: TestTree
singleRandomShipTest = QC.testProperty "Randomly adding a ship to an empty board always works" $
  \ (QCGen gen) (Positive h, Positive w) (Positive l) -> (h >= l || w >= l) ==> let
    state = evalRand (addRandomShip l $ emptyState (h,w)) gen
 in shipLengthsMatch [l] state

stateWithShipsTest  :: TestTree
stateWithShipsTest = QC.testProperty "stateWithShips has 2 length 3 ships and 1 length 4 ship" $
  \ (QCGen gen) (h,w) -> h >= 4 && w >=4 ==> shipLengthsMatch [3,3,4] $ evalRand (stateWithShips (h, w)) gen

countHits :: GameState -> Int
countHits (GameState grid _) = length $ filter (\case
                                                GridCell WasHit _ -> True 
                                                GridCell NotHit _ -> False) $ toList grid

totalHealth :: GameState -> Int
totalHealth (GameState _ ships) = sum $ (\ (ShipData health) -> health) <$> ships

validateShotOutcome :: GameState -> ShotOutcome -> GameState -> Bool
validateShotOutcome initial AlreadyTaken final = initial == final
validateShotOutcome initial OffGrid final = initial == final
validateShotOutcome initial outcome final = let
  nHits = case outcome of
            Hit -> 1
            Sunk -> 1
            Win -> 1
            Miss -> 1
            AlreadyTaken -> 0
            OffGrid -> 0
  dHealth = case outcome of
              Hit -> -1
              Sunk -> -1
              Win -> -1
              Miss -> 0
              AlreadyTaken -> 0
              OffGrid -> 0
  in (countHits initial + nHits == countHits final)
     && (totalHealth initial + dHealth == totalHealth final)

shotsHistory :: GameState -> [(Int, Int)] -> [(GameState, ShotOutcome, GameState)]
shotsHistory initialState targets = unfoldr
  (\case 
    (_, []) -> Nothing
    (state, target:targets) -> let
      (shotOutcome, newState) = fire target state 
      in Just ((state, shotOutcome, newState), (newState, targets))
  ) (initialState, targets)

--Note that these can (deliberately) fire off-grid
shotsGen :: (Int, Int) -> Gen [(Int, Int)]
shotsGen (h,w) = replicateM (h*w) $ (,) <$> choose (0, h) <*> choose (0, w)

randomFiringTest  :: TestTree
randomFiringTest = QC.testProperty "random shots yield reasonable results" $
  \ (QCGen gen) (h,w) -> h >= 4 && w >=4 ==> forAll (shotsGen (h,w)) $ \ targets -> let
    initialState = evalRand (stateWithShips (h, w)) gen
    log = shotsHistory initialState targets
 in all (\(state, outcome, finalState) -> validateShotOutcome state outcome finalState) log


oldShotHistoryTest :: TestTree
oldShotHistoryTest = testCase "Pre-prepared sequence of shots (Old firing function)" $ let
  log = (`evalState` emptyState (10,10)) $ do
          modify $ fromJustNote "first ship" . insertShip (ShipKey (0,0)) Horizontal 3
          modify $ fromJustNote "seccond ship" . insertShip (ShipKey (3,3)) Vertical 2
          traverse oldFire [(0,0), (10,5), (0,1), (0,0), (5,5), (5,5), (0,2), (3,3), (0,-1), (4,3)]
       in log `shouldBe` [Hit, OffGrid, Hit, AlreadyTaken, Miss, AlreadyTaken, Sunk, Hit, OffGrid, Win]
  
shotHistoryTest :: TestTree
shotHistoryTest = testCase "Pre-prepared sequence of shots" $ let
  initialState =
    fromJustNote "seccond ship" $ insertShip (ShipKey (3,3)) Vertical 2 $
      fromJustNote "first ship" $ insertShip (ShipKey (0,0)) Horizontal 3 $
      emptyState (10,10)
  targets = [(0,0), (10,5), (0,1), (0,0), (5,5), (5,5), (0,2), (3,3), (0,-1), (4,3)]
  log = (\ (_,outcome,_) -> outcome) <$> shotsHistory initialState targets
  in log `shouldBe` [Hit, OffGrid, Hit, AlreadyTaken, Miss, AlreadyTaken, Sunk, Hit, OffGrid, Win]
