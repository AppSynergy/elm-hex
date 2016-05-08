module Example where

import Color
import Text
import Graphics.Collage as Draw
import Graphics.Element as Ele
import Hex

main : Ele.Element
main = Draw.collage 600 400 shapes

shapes = List.map (\x -> shapeAt hexagon x 0 0) [0,1,2]

r = 50

hexagon = Draw.ngon 6 r

lines : Draw.LineStyle
lines =
  { color = Color.lightBrown
  , width = 2.0
  , cap = Draw.Flat
  , join = Draw.Smooth
  , dashing = [4,2]
  , dashOffset = 3
  }

shapeAt : Draw.Shape -> Float -> Float -> Float -> Draw.Form
shapeAt shape x y z =
  Draw.group
    [ shape |> Draw.outlined lines
    , toString x ++ " " ++ toString y ++ " " ++ toString z
      |> Text.fromString |> Draw.text
    ] |> Draw.move (x*r*2, y*r*2)
