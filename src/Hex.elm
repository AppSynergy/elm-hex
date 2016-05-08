module Hex where

import Bitwise exposing (and)

type alias Point = (Float,Float)

type Hex
  = PointyTopped Point Float
  | FlatTopped Point Float

type alias VectorCube =
  { x : Int, y : Int, z : Int }

type Coordinate
  = Offset Int Int
  | Cube Int Int Int
  | Axial Int Int


getAngles : Hex -> List Int
getAngles hex = case hex of
  PointyTopped _ _ ->  [0,60,120,180,240,300]
  FlatTopped _ _ -> [30,90,150,210,270,330]


hexCorner : Point -> Float -> Int -> Point
hexCorner center radius i =
  let
    angleDeg = 60 * i + 30
    angleRad = degrees angleDeg
  in
  ( (fst center) + radius*(cos angleRad)
  , (snd center) + radius*(sin angleRad)
  )


toAxial : Coordinate -> Coordinate
toAxial coord = case coord of
  Cube x y z -> Axial x z
  _ -> coord


toCube : Coordinate -> Coordinate
toCube coord = case coord of
  Offset x y ->
    let z = y - (x + (x `and` 1)) // 2 in
    Cube x (-x-z) z
  Axial q r -> Cube q (-r-q) r
  _ -> coord


getCoords : Coordinate -> VectorCube
getCoords coord = case coord of
  Cube x y z -> { x = x, y = y, z = z }
  _ -> getCoords (toCube coord)


sort : List Coordinate -> List Coordinate
sort xs =
  let
    comparing a' b' =
      let
        a = getCoords a'
        b = getCoords b'
      in
      if a.x > b.x then GT
      else if a.x < b.x then LT
      else if a.z > b.z then GT
      else if a.z < b.z then LT
      else EQ
  in
  List.sortWith comparing xs


add : Coordinate -> Coordinate -> Coordinate
add a' b' =
  let
    a = getCoords a'
    b = getCoords b'
  in
  Cube (a.x + b.x) (a.y + b.y) (a.z + b.z)


rotateLeft : Coordinate -> Coordinate
rotateLeft a' =
  let a = getCoords a' in
  Cube -a.y -a.z -a.x


rotateRight : Coordinate -> Coordinate
rotateRight a' =
  let a = getCoords a' in
  Cube -a.z -a.x -a.y


neighbors : Coordinate -> List Coordinate
neighbors coord =
  let
    a = [Cube 1 -1  0, Cube 1  0 -1, Cube 0 1 -1,
    Cube -1 1  0, Cube -1  0 1, Cube 0 -1 1]
  in
  List.map (add coord) a


areNeighbors : Coordinate -> Coordinate -> Bool
areNeighbors cc1 cc2 =
  List.member cc1 (neighbors cc2)


distance : Coordinate -> Coordinate -> Int
distance cc1 cc2 =
  let
    c1 = getCoords cc1
    c2 = getCoords cc2
  in
  ((abs (c1.x-c2.x)) + (abs (c1.y-c2.y)) + (abs (c1.z-c2.z))) // 2


lineDraw : Coordinate -> Coordinate -> List Coordinate
lineDraw cc1 cc2 =
  let
    d = distance cc1 cc2
  in
  List.map (\i -> cubeLerp cc1 cc2 ((toFloat i) / (toFloat d))) [0..d]


cubeLerp : Coordinate -> Coordinate -> Float -> Coordinate
cubeLerp cc1 cc2 t =
  let
    a = getCoords cc1
    b = getCoords cc2
  in
  Cube  (a.x + (round <| toFloat (b.x - a.x) * t))
    (a.y + (round <| toFloat (b.y - a.y) * t))
    (a.z + (round <| toFloat (b.z - a.z) * t))


movementRange : Coordinate -> Int -> List Coordinate
movementRange coord i =
  let
    g dx dy =
      let dz = negate (dx + dy) in
      if dx + dy + dz == 0
      then Just (add coord (Cube dx dy dz))
      else Nothing
    f dx = List.map (g dx) [(lb dx)..(ub dx)]
    lb dx = max (negate i) (negate (dx + i))
    ub dx = min i (i - dx)
  in
  List.filterMap identity <|
    List.concatMap f [(-i)..i]


limitedFloodFill : Coordinate -> Int -> List Coordinate -> List Coordinate
limitedFloodFill start i obstacles =
  let
    b = List.filter (\x -> not (List.member x obstacles)) a
    a = (neighbors start)
  in
  [start] ++ b
