import Lib

import Control.Monad.State

import Test.HUnit hiding (State)
import Test.Hspec.Expectations

main = runTestTT $ TestList [TestCase shotHistoryTest, TestCase missfireTest]

shotHistoryTest = let
  log = (`evalState` emptyState (10,10)) $ do
          modify $ insertShip (ShipKey (0,0)) Horizontal 3
          modify $ insertShip (ShipKey (3,3)) Vertical 2
          traverse fire [(0,0), (0,1), (0,0), (5,5), (5,5), (0,2), (3,3), (4,3)]
       in log `shouldBe` [Hit, Hit, AlreadyTaken, Miss, AlreadyTaken, Sunk, Hit, Win]

missfireTest = do
  print (fire (10,5) `evalState` emptyState (10,10)) `shouldThrow` errorCall "Fired off grid"
  print (fire (0,-1) `evalState` emptyState (10,10)) `shouldThrow` errorCall "Fired off grid"
  
