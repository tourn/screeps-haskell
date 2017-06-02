{-# LANGUAGE TemplateHaskell #-}
module Lib
    ( run
    ) where

import           Control.Monad (forM_)
import           API

run :: IO()
run = do
  tickCallback tick

tick :: IO ()
tick = do
  putStrLn $ show "TICK"
  creeps' <- creeps
  sources' <- sources
  let room = Room sources' creeps'
  let actions = think room
  putStrLn $ show room
  putStrLn $ show actions
  forM_ actions runAction

think :: Room -> [Action]
think room = map makeAction (roomCreeps room)
  where makeAction creep 
                   | adjacent (creepPos creep) (sourcePosition targetSource) = HarvestAction creep targetSource
                   | otherwise = moveTo creep (sourcePosition targetSource)
        targetSource = (head (roomObjects room))

moveTo :: Creep -> Position -> Action
moveTo creep pos = MoveAction creep pos

inRange :: Int -> Position -> Position -> Bool
inRange 0 a b = a == b
inRange range (ax, ay) (bx, by) = ((distance ax bx) <= range) && ((distance ay by) <= range)
  where distance a b = abs $ a - b

adjacent = inRange 1

