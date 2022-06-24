module Miku.UI.Log (app) where

import Data.Text qualified as T
import Graphics.Vty
    ( Attr,
      rgbColor,
      Color,
      Attr,
      white,
      blue,
      cyan,
      green,
      red,
      yellow,
      black,
      withURL
    )
import Brick.Types
  ( Widget
  )
import Brick.Widgets.Core
  ( (<=>)
  , withAttr
  , vBox
  , str
  , hyperlink
  , modifyDefAttr
  )
import Brick.Util    (on, fg)
import Brick.AttrMap (attrMap, AttrMap)

import Brick.Main

import Relude hiding (on)

import Miku.UI.Utils (hexColorToRGB)


color1 :: Color
color1 = hexColorToRGB "#76BA99"

color2 :: Color
color2 = hexColorToRGB "#FF7396"

color3 :: Color
color3 = rgbColor 255 115 150

ui :: Widget n
ui = withAttr "color" $ str (show $ color2 == color3)

globalDefault :: Attr
globalDefault = fg white

theMap :: AttrMap
theMap = attrMap globalDefault
    [ ("color",  fg color2)
    ]

app :: IO ()
app = defaultMain app' ()
  where
   app' :: App () e ()
   app' =
      App { appDraw = const [ui]
          , appHandleEvent = resizeOrQuit
          , appStartEvent = return
          , appAttrMap = const theMap
          , appChooseCursor = neverShowCursor
          }
