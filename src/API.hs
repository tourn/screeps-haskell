{-# LANGUAGE TemplateHaskell #-}

module API
    (
    Positional(..),
    Position,
    Resource,
    CreepInventory,
    Action(..),
    RoomObject(..),
    Room(..),
    Creep(..),
    runAction,
    tickCallback,
    creeps,
    sources,
    controller
    ) where

import qualified Data.JSString    as S
import qualified GHCJS.Prim  as P (getProp, fromJSArray)
import qualified GHCJS.Types as T
import qualified GHCJS.Foreign.Callback as C
import           GHCJS.Marshal.Pure (pFromJSVal)
import           GHCJS.Marshal.Internal (fromJSValUnchecked_pure)
import qualified Data.Map as Map
import qualified JavaScript.Array as A

foreign import javascript unsafe "[{name: 'dummy', carry: { energy: 12}, pos: {x: 14, y: 15}, carryCapacity: 50 }, {name: 'dummy2', carry: {}, pos: {x: 45, y: 11}, carryCapacity: 100 }]" js_getcreeps_dummy :: T.JSVal

foreign import javascript unsafe "module.exports.loop = $1" js_mainLoop :: C.Callback (IO()) -> IO()
foreign import javascript unsafe "Object.keys(Game.creeps).map(function(k){ return Game.creeps[k]})" js_getcreeps :: T.JSVal
foreign import javascript unsafe "Game.rooms.sim.find(FIND_SOURCES)" js_getsources :: T.JSVal
foreign import javascript unsafe "Game.rooms.sim.controller" js_getcontroller :: T.JSVal

foreign import javascript unsafe "Game.creeps[$1].moveTo($2,$3);" js_moveAction :: S.JSString -> Int -> Int -> IO ()
foreign import javascript unsafe "Game.creeps[$1].harvest(Game.getObjectById($2));" js_harvestAction :: S.JSString -> S.JSString -> IO ()
foreign import javascript unsafe "Game.creeps[$1].upgradeController(Game.getObjectById($2));" js_upgradeAction :: S.JSString -> S.JSString -> IO ()

class Positional a where
  position :: a -> Position
  objectId :: a -> String

type ObjectId = String
type Position = (Int, Int)
type Resource = String
type CreepInventory = Map.Map Resource Int

data Action
  = MoveAction Creep Position
  | HarvestAction Creep RoomObject
  | UpgradeAction Creep RoomObject
  | NoopAction Creep
  deriving (Show)

data RoomObject
  = Source
  { roId :: ObjectId
  , roPosition :: Position
  , sourceEnergy :: Int
  }
  | Spawn
  { roId :: ObjectId
  , roPosition :: Position
  }
  | Controller
  { roId :: ObjectId
  , roPosition :: Position
  }
-- | Extension Position
  deriving (Show)

instance Positional Creep where
  position = creepPos
  objectId = \_ -> "" --TODO

instance Positional RoomObject where
  position = roPosition
  objectId = roId

data Room
  = Room
  { roomObjects :: [RoomObject]
  , roomCreeps :: [Creep]
  , roomController :: RoomObject
  }
  deriving (Show)

data Creep
  = Creep
  { creepName :: String
  , creepPos :: Position
  , creepCarry :: CreepInventory
  , creepCarryCapacity :: Int
  } deriving (Show)

tickCallback :: IO() -> IO()
tickCallback f = do
  cb <- C.syncCallback C.ThrowWouldBlock f
  js_mainLoop cb
--  C.releaseCallback cb -- if this is released, not all ticks will run

runAction :: Action -> IO ()
runAction (MoveAction creep (x, y)) = js_moveAction (S.pack $ creepName creep) x y
runAction (HarvestAction creep source) = js_harvestAction (S.pack $ creepName creep) (S.pack (objectId source))
runAction (UpgradeAction creep controller) = js_upgradeAction (S.pack $ creepName creep) (S.pack (objectId controller))
runAction (NoopAction creep) = putStrLn $ (creepName creep) ++ " is doing NOTHING"

readCreep :: T.JSVal -> IO Creep
readCreep v = do
  name   <- P.getProp v "name" 
  carryCapacity <- P.getProp v "carryCapacity" 
  js_carry <- P.getProp v "carry"
  carry <- readCarry js_carry
  js_pos <- P.getProp v "pos"
  pos <- readPos js_pos
  return $ Creep (pFromJSVal name) pos carry (pFromJSVal carryCapacity)

creeps :: IO [Creep]
creeps = do
  creepList <- P.fromJSArray js_getcreeps
  mapM readCreep creepList

readCarry :: T.JSVal -> IO CreepInventory
readCarry v = do
  js_energy <- P.getProp v "energy"
  return $ Map.fromList [("energy", pFromJSVal js_energy)]

readPos :: T.JSVal -> IO Position
readPos v = do
  x <- P.getProp v "x"
  y <- P.getProp v "y"
  return $ ((pFromJSVal x), (pFromJSVal y))

readSource :: T.JSVal -> IO RoomObject
readSource v = do
  js_pos <- P.getProp v "pos"
  pos <- readPos js_pos
  js_energy <- P.getProp v "energy"
  js_id <- P.getProp v "id"
  return $ Source (pFromJSVal js_id) pos (pFromJSVal js_energy)

sources :: IO [RoomObject]
sources = do
  sourceList <- P.fromJSArray js_getsources
  mapM readSource sourceList

readController :: T.JSVal -> IO RoomObject
readController v = do
  js_pos <- P.getProp v "pos"
  pos <- readPos js_pos
  js_id <- P.getProp v "id"
  return $ Controller (pFromJSVal js_id) pos
  
controller :: IO RoomObject
controller = readController js_getcontroller

