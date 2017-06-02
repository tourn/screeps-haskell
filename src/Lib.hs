{-# LANGUAGE TemplateHaskell #-}
module Lib
    ( run
    ) where


import qualified Data.JSString    as S
import qualified GHCJS.Prim  as P (getProp, fromJSArray)
import qualified GHCJS.Types as T
import qualified GHCJS.Foreign.Callback as C
import           GHCJS.Marshal.Pure (pFromJSVal)
import           GHCJS.Marshal.Internal (fromJSValUnchecked_pure)
import           Control.Monad (forM_)
import qualified Data.Map as Map
import qualified JavaScript.Array as A

foreign import javascript unsafe "console.log('hello hello')" js_sayHi :: IO ()
foreign import javascript unsafe "{val: 42}" js_val :: T.JSVal
foreign import javascript unsafe "'bla'" js_str :: S.JSString
foreign import javascript unsafe "{name: \"myName\", energy: 42}" js_creep :: T.JSVal
foreign import javascript unsafe "[{name: 'dummy', carry: { energy: 12}, pos: {x: 14, y: 15}, carryCapacity: 50 }, {name: 'dummy2', carry: {}, pos: {x: 45, y: 11}, carryCapacity: 100 }]" js_getcreeps_dummy :: T.JSVal

foreign import javascript unsafe "module.exports.loop = $1" js_mainLoop :: C.Callback (IO()) -> IO()
foreign import javascript unsafe "Game.creeps[$1].moveTo($2,$3);" js_moveAction :: S.JSString -> Int -> Int -> IO ()
foreign import javascript unsafe "Object.keys(Game.creeps).map(function(k){ return Game.creeps[k]})" js_getcreeps :: T.JSVal
foreign import javascript unsafe "Game.rooms.sim.find(FIND_SOURCES)" js_getsources :: T.JSVal

type CreepName = String
type Position = (Int, Int)

data Action
  = MoveAction CreepName Position
  deriving (Show)

data RoomObject
  = Source
  { position :: Position 
  , energy :: Int
  }
-- | Spawn Position
-- | Controller Position
-- | Extension Position
  deriving (Show)

data Room
  = Room
  { roomObjects :: [RoomObject]
  , roomCreeps :: [Creep]
  }
  deriving (Show)

runAction :: Action -> IO ()
runAction (MoveAction name (x, y)) = js_moveAction (S.pack name) x y

type Resource = String
type CreepInventory = Map.Map Resource Int

data Creep
  = Creep
  { creepName :: String
  , creepPos :: Position
  , creepCarry :: CreepInventory
  , creepCarryCapacity :: Int
  } deriving (Show)

run :: IO()
run = do
  cb <- C.syncCallback C.ThrowWouldBlock tick
  js_mainLoop cb
  C.releaseCallback cb

tick :: IO ()
tick = do
  creepList <- P.fromJSArray js_getcreeps
  creeps <- mapM readCreep creepList
  sourceList <- P.fromJSArray js_getsources
  sources <- mapM readSource sourceList
  let room = Room sources creeps
  putStrLn $ show room
  let actions = think room
  forM_ actions runAction
  putStrLn $ show actions

think :: Room -> [Action]
think room = map makeAction (roomCreeps room)
  where makeAction creep = MoveAction (creepName creep) (newPos creep)
        newPos (Creep _ (x, y) _ _) = (x + 1, y + 1)


readCreep :: T.JSVal -> IO Creep
readCreep v = do
  name   <- P.getProp v "name" 
  carryCapacity <- P.getProp v "carryCapacity" 
  js_carry <- P.getProp v "carry"
  carry <- readCarry js_carry
  js_pos <- P.getProp v "pos"
  pos <- readPos js_pos
  return $ Creep (pFromJSVal name) pos carry (pFromJSVal carryCapacity)

readCarry :: T.JSVal -> IO CreepInventory
readCarry v = do
  --return Map.empty
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
  return $ Source pos (pFromJSVal js_energy)
