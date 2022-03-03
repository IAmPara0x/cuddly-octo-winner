module Miku.UI.App (render) where

import Relude hiding ( State
                     , on
                     )

import Control.Lens ((^.))
import Control.Monad.Trans.Except (runExceptT)

import Brick.Main
import Brick.Types (Widget)

import Graphics.Vty( red
                   , black
                   )
import Brick.Widgets.Core ( (<=>)
                          , withAttr
                          , vBox
                          , str
                          )
import Brick.Util (on, fg)
import Brick.AttrMap (attrMap, AttrMap)

import Miku.UI.State

draw :: State -> [Widget n]
draw (State _ (Left err))   = [withAttr "path" $ str (show err)]
draw (State _ (Right path)) = [withAttr "path" $ str (toString path)]

theMap :: AttrMap
theMap = attrMap (red `on` black)
       [
       ]
app :: App State e ()
app = App { appDraw = draw
          , appHandleEvent = resizeOrQuit
          , appStartEvent  = return
          , appAttrMap = const theMap
          , appChooseCursor = neverShowCursor
          }

render :: IO()
render = do
           state <- newState
           defaultMain app state
           return ()

-- go = do
--        state <- runExceptT state
-- defaultMap app 
