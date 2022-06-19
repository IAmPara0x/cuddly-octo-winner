{-# OPTIONS_GHC -Wno-incomplete-patterns #-}
module Miku.UI.Log (app, hexToInteger) where


import Data.Text qualified as T
import Data.List              (groupBy)
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
      withURL )
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

hexColorToRGB :: String -> Color
hexColorToRGB ('#':xs)
  | length xs == 6 = toRGB $ map hexToInteger $ foldr f [] xs
  where
    f :: Char -> [String] -> [String]
    f a []            = [[a]]
    f a (h:hs)
      | length h == 2 = [a]:h:hs
      | otherwise     = (a:h):hs

    toRGB :: [Integer] -> Color
    toRGB [r, g, b] = rgbColor r g b

g :: String -> [Integer]
g ('#':xs)
  | length xs == 6 = map hexToInteger $ foldr f [] xs
  where
    f :: Char -> [String] -> [String]
    f a []            = [[a]]
    f a xs@(h:hs)
      | length h == 2 = [a]:xs
      | otherwise     = (a:h):hs

hexToInteger :: String -> Integer
hexToInteger xs = sum $ zipWith (\p c -> p * hexCodes c) [ 16^i | i <- [len,len-1..0]] xs
  where
    len = length xs - 1

hexCodes :: Char -> Integer
hexCodes '0' = 0
hexCodes '1' = 1
hexCodes '2' = 2
hexCodes '3' = 3
hexCodes '4' = 4
hexCodes '5' = 5
hexCodes '6' = 6
hexCodes '7' = 7
hexCodes '8' = 8
hexCodes '9' = 9
hexCodes 'A' = 10
hexCodes 'B' = 11
hexCodes 'C' = 12
hexCodes 'D' = 13
hexCodes 'E' = 14
hexCodes 'F' = 15
hexCodes _   = error "ERROR: Not a hex char."


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
