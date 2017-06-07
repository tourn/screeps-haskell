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

think :: Room -> [Action]
think room = map makeAction (roomCreeps room)
  where makeAction creep 
                    | inRange 2 (creepPos creep) (controllerPosition (roomController room)) =
                        if not $ empty creep 
                        then  UpgradeAction creep (roomController room)
                        else moveTo creep (sourcePosition targetSource)
                    | adjacent (creepPos creep) (sourcePosition targetSource) = 
                        if not $ full creep
                        then HarvestAction creep targetSource
                        else moveTo creep (controllerPosition (roomController room))
                    | otherwise = moveTo creep (targetPos creep)
        targetSource = (head (roomObjects room))
        targetPos creep
                | full creep = (controllerPosition (roomController room))
                | otherwise = (sourcePosition targetSource)

moveTo :: Creep -> Position -> Action
moveTo creep pos = MoveAction creep pos

inRange :: Int -> Position -> Position -> Bool
inRange 0 a b = a == b
inRange range (ax, ay) (bx, by) = ((distance ax bx) <= range) && ((distance ay by) <= range)
  where distance a b = abs $ a - b

adjacent = inRange 1

carryWeight :: Creep -> Int
carryWeight c = sum $ Map.elems (creepCarry c)

full :: Creep -> Bool
full c = (carryWeight c) >= (creepCarryCapacity c)

empty :: Creep -> Bool
empty c = (carryWeight c) == 0
