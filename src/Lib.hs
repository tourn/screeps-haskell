module Lib
    ( run
    ) where


import qualified Data.JSString    as S
import qualified GHCJS.Prim  as P (getProp)
import qualified GHCJS.Types as T
import qualified GHCJS.Foreign.Callback as C
import           GHCJS.Marshal.Pure (pFromJSVal)
import           Control.Monad (forM_)

foreign import javascript unsafe "console.log('hello hello')" js_sayHi :: IO ()
foreign import javascript unsafe "{val: 42}" js_val :: T.JSVal
foreign import javascript unsafe "'bla'" js_str :: S.JSString
foreign import javascript unsafe "{name: \"myName\", energy: 42}" js_creep :: T.JSVal

foreign import javascript unsafe "module.exports.loop = $1" js_mainLoop :: C.Callback (IO()) -> IO()
foreign import javascript unsafe "Game.creeps[$1].moveTo($2,$3);" js_moveAction :: S.JSString -> Int -> Int -> IO ()

type CreepName = String
type Position = (Int, Int)

data Action
  = MoveAction CreepName Position
  deriving (Show)

runAction :: Action -> IO ()
runAction (MoveAction name (x, y)) = js_moveAction (S.pack name) x y

data Creep
  = Creep
  { creepName :: String
  , creepEnergyCapacity :: Int
  , creepPosition :: (Int, Int)
  } deriving (Show)

run :: IO()
run = do
  cb <- C.asyncCallback tick
  js_mainLoop cb

tick :: IO ()
tick = do
  --creep <- readCreep js_creep
  --runAction $ MoveAction "myName" (10, 20)
  --putStrLn $ show creep
  js_sayHi
  let room = Room 42
  --let creeps =    [ Creep "creep1" 42 (2, 4)    , Creep "creep2" 8  (5, 10)    ]
  let creeps =    [ Creep "Harvester1" 42 (2, 4) ]
  let world = World room creeps
  let actions = think world
  forM_ actions runAction
  putStrLn $ show actions

think :: World -> [Action]
think (World (Room r) creeps) = map makeAction creeps
  where makeAction creep = MoveAction (creepName creep) (newPos creep)
        newPos (Creep _ _ (x, y)) = (x + 1, y + 1)

data Room = Room Int deriving (Show)
data World = World Room [Creep] deriving (Show)

readCreep :: T.JSVal -> IO Creep
readCreep v = do
  name   <- P.getProp v "name" 
  energy <- P.getProp v "energy" 
  return $ Creep (pFromJSVal name) (pFromJSVal energy) (0, 0)
