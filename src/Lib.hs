{-# LANGUAGE TemplateHaskell #-}
module Lib
    ( run
    ) where

import           Control.Monad (forM_)
import           API
import qualified Data.Map as Map

run :: IO()
run = do
  tickCallback tick

tick :: IO ()
tick = do
  putStrLn $ show "TICK"
  room <- Room <$> sources <*> creeps <*> controller
  let actions = think room
  putStrLn $ show room
  putStrLn $ show actions
  forM_ actions runAction

data Behavior = A | B | C

defaultBehavior ::  Room -> Creep -> Behavior
defaultBehavior room creep
  | inRange 2 creep (roomController room) = A
  | adjacent creep targetSource = B
  | otherwise = C
  where
    targetSource = (head (roomObjects room))

data BehaviorOp = BehaviorOp
  (Behavior -> Action)

blaOp0 :: Room -> Creep -> BehaviorOp
blaOp0 room creep = BehaviorOp f
    where 
        f A =
          if not $ empty creep
          then  UpgradeAction creep (roomController room)
          else moveTo creep targetSource
        f B =
          if not $ full creep
          then HarvestAction creep targetSource
          else moveTo creep (roomController room)
        f C =
          if full creep
          then moveTo creep (roomController room)
          else moveTo creep targetSource
        targetSource = (head (roomObjects room))

think :: Room -> [Action]
think room = map f (roomCreeps room)
  where 
      makeBehaviorOp creep = blaOp0 room creep
      makeBehaviorbla creep = defaultBehavior room creep
      f :: Creep -> Action
      f creep = 
        let (BehaviorOp b) = makeBehaviorOp creep
            c = makeBehaviorbla creep
        in b c

moveTo :: Positional a => Creep -> a -> Action
moveTo creep p = MoveAction creep (position p)

inRange :: (Positional a, Positional b) => Int -> a -> b -> Bool
inRange 0 a b = (position a) == (position b)
inRange range a b = ((distance ax bx) <= range) && ((distance ay by) <= range)
  where
    distance a b = abs $ a - b
    (ax, ay) = position a
    (bx, by) = position b




adjacent = inRange 1

carryWeight :: Creep -> Int
carryWeight c = sum $ Map.elems (creepCarry c)

full :: Creep -> Bool
full c = (carryWeight c) >= (creepCarryCapacity c)

empty :: Creep -> Bool
empty c = (carryWeight c) == 0
