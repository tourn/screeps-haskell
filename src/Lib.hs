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
  --where makeAction creep = moveTo creep (sourcePosition (head (roomObjects room)))
  where makeAction creep = HarvestAction (creepName creep) (head (roomObjects room))

moveTo :: Creep -> Position -> Action
moveTo creep pos = MoveAction (creepName creep) pos

