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


toXY : Coordinate -> Float -> (Float, Float)
toXY coord radius = case coord of
  Cube x y z -> let
    a = (sqrt 3.0) * radius * ( (toFloat z)/2 + (toFloat x))
    b = 3/2 * radius * (toFloat z)
  in (a, b)
  _ -> toXY (toCube coord) radius


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
      let (a,b) = (getCoords a', getCoords b') in
      if a.x > b.x then GT
      else if a.x < b.x then LT
      else if a.z > b.z then GT
      else if a.z < b.z then LT
      else EQ
  in
  List.sortWith comparing xs


add : Coordinate -> Coordinate -> Coordinate
add a' b' =
  let (a,b) = (getCoords a', getCoords b') in
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
  let xs =
    [ Cube  1 -1  0
    , Cube  1  0 -1
    , Cube  0  1 -1
    , Cube -1  1  0
    , Cube -1  0  1
    , Cube  0 -1  1
    ]
  in List.map (add coord) xs


areNeighbors : Coordinate -> Coordinate -> Bool
areNeighbors a =
  List.member a << neighbors


distance : Coordinate -> Coordinate -> Int
distance a' b' =
  let (a,b) = (getCoords a', getCoords b') in
  ((abs (a.x-b.x)) + (abs (a.y-b.y)) + (abs (a.z-b.z))) // 2


lineDraw : Coordinate -> Coordinate -> List Coordinate
lineDraw a b =
  let d = distance a b in
  List.map (\i -> cubeLerp a b ((toFloat i) / (toFloat d))) [0..d]


cubeLerp : Coordinate -> Coordinate -> Float -> Coordinate
cubeLerp a' b' t =
  let (a,b) = (getCoords a', getCoords b') in
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
    notIn xs = not << (flip List.member) xs
    neighbors' xs = neighbors
      >> List.filter (notIn obstacles)
    fill xs j =
      let bs = List.concatMap (\x -> x :: neighbors' xs x) xs in
      if j < i then bs ++ (fill bs (j+1)) else bs
    dedupe xs = List.foldr (\x y -> if List.member x y then y else x :: y ) [] xs
  in
  dedupe <| fill [start] 1
