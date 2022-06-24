module Miku.UI.Utils (hexColorToRGB, hexToInteger) where

import Graphics.Vty.Attributes.Color (Color, rgbColor)

import Data.Text   qualified as T
import Text.Printf              (printf)

import Relude

hexColorToRGB :: Text -> Color
hexColorToRGB hexColor
  |  T.length hexColor == 7
  && T.head hexColor   == '#' = toRGB $ map hexToInteger $ T.chunksOf 2 (T.tail hexColor)
  | otherwise                 = error $ toText @String $ printf "%s is not a valid hex color code." hexColor

  where
    toRGB :: [Integer] -> Color
    toRGB [r, g, b] = rgbColor r g b
    toRGB l         = error $ toText @String
                    $ printf "list with only 3 elements are accepted, but got %s" (show @String l)

hexToInteger :: Text -> Integer
hexToInteger a = sum $ zipWith (\p c -> 16^p * hexCodes c) [0..]
               $ T.unpack $ T.reverse a

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
hexCodes 'a' = 10
hexCodes 'b' = 11
hexCodes 'c' = 12
hexCodes 'd' = 13
hexCodes 'e' = 14
hexCodes 'f' = 15
hexCodes  c  = error $ toText @String $ printf "ERROR: %c is not a hex character." c
