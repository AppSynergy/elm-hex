module Example where

import Color
import Text
import Graphics.Collage as Draw
import Graphics.Element as Ele
import Hex

radius = 26

hexagon = Draw.ngon 6 radius


main : Ele.Element
main = Draw.collage 600 400 (List.map hexAt shapes)


shapes =
  let a = Hex.Cube 0 0 0 in
  Hex.neighbors a
  --Hex.limitedFloodFill (Hex.Cube 0 0 0) 4
  --  [(Hex.Cube 1 0 -1), (Hex.Cube 0 1 -1), (Hex.Cube -1 1 0)]


lines : Draw.LineStyle
lines =
  { color = Color.lightBrown
  , width = 2.0
  , cap = Draw.Flat
  , join = Draw.Smooth
  , dashing = [3,4]
  , dashOffset = 0
  }


hexAt : Hex.Coordinate -> Draw.Form
hexAt coords =
  let c = Hex.getCoords coords in
  Draw.group
    [ hexagon |> Draw.outlined lines |> Draw.rotate (degrees 30)
    , toString c.x ++ " " ++ toString c.y ++ " " ++ toString c.z
      |> Text.fromString |> Draw.text
    ] |> Draw.move (Hex.toXY coords radius)
