module Example where

import Graphics.Element exposing (Element,show)
import Hex

main : Element
main = show <| Hex.distance (Hex.Cube 0 0 0) (Hex.Cube 1 -1 0) -- 1
