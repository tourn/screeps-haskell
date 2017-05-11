module Lib
    ( someFunc
    ) where


import qualified Data.JSString    as S
import qualified GHCJS.Prim  as P (getProp)
import qualified GHCJS.Types as T
import           GHCJS.Marshal.Pure (pFromJSVal)

foreign import javascript unsafe "alert($1)" alert :: S.JSString -> IO ()
foreign import javascript unsafe "{val: 42}" js_val :: T.JSVal
foreign import javascript unsafe "'bla'" js_str :: S.JSString
foreign import javascript unsafe "{name: \"myName\", energy: 42}" js_creep :: T.JSVal

type CreepName = String
type Position = (Int, Int)
data MoveAction = MoveAction CreepName Position

class Action a where
  runAction :: a -> IO ()

foreign import javascript unsafe "Game.creeps[$1].moveTo($2,$3);" js_moveAction :: S.JSString -> Int -> Int -> IO ()

instance Action MoveAction where
  runAction (MoveAction name (x, y)) = js_moveAction (S.pack name) x y

data Creep
  = Creep
  { creepName :: String
  , creepEnergyCapacity :: Int
  } deriving (Show)

someFunc :: IO ()
someFunc = do
  creep <- readCreep js_creep
  putStrLn $ show creep

readCreep :: T.JSVal -> IO Creep
readCreep v = do
  name   <- P.getProp v "name" 
  energy <- P.getProp v "energy" 
  return $ Creep (pFromJSVal name) (pFromJSVal energy)
