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

data Behavior = Gather | Work | Patrol | Attack


defaultBehavior ::  Room -> Creep -> Behavior
defaultBehavior room creep
  | gathering = Gather
  | otherwise = Work
  where
    targetSource = (head (roomObjects room))
    gathering = (empty creep) || ((creep `adjacent` targetSource) && not (full creep))

deliverBehavior ::  Room -> Creep -> Behavior
deliverBehavior room creep
  | gathering = Gather
  | otherwise = Work
  where
    targetSource = (head (roomObjects room))
    gathering = (empty creep) || ((creep `adjacent` targetSource) && not (full creep))

data BehaviorOp = BehaviorOp
  (Behavior -> Action)

--two levels of decisions: Behavior defines a creep role, BehaviorOps get the specfic atomic instructions for them
--consider havig a list of behaviors and pick the best one for the situation
upgradeOp :: Room -> Creep -> BehaviorOp
upgradeOp room creep = BehaviorOp f
  where
      f Work =
        if inRange 2 creep controller
        then UpgradeAction creep controller
        else moveTo creep controller
      f Gather =
        if adjacent creep targetSource
        then HarvestAction creep targetSource
        else moveTo creep targetSource
      targetSource = (head (roomObjects room))
      controller = roomController room

deilverOp :: Room -> Creep -> BehaviorOp
deliverOp room creep = BehaviorOp f
  where
      f Work =
        if adjacent creep targetContainer
        then DeliverAction creep targetContainer
        else moveTo creep controller
      f Gather =
        if adjacent creep targetSource
        then HarvestAction creep targetSource
        else moveTo creep targetSource
      targetSource = (head (roomObjects room))
      targetContainer = _ -- TODO
      controller = roomController room

think :: Room -> [Action]
think room = map f (roomCreeps room)
  where
      makeBehaviorOp creep = blaOp0 room creep
      makeDefaultBehavior creep = defaultBehavior room creep
      f :: Creep -> Action
      f creep =
        let (BehaviorOp b) = makeBehaviorOp creep
            c = makeDefaultBehavior creep
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
