module Tests where

import ElmTest exposing (..)

import String
import Hex exposing (..)

all : Test
all =
  suite "Hex Definitions"

    [ test "hexCorner" <|
        assertEqual (5.4,4.54) <|
        hexCorner (5.4,6.54) 2 4

    , test "coordinate centre point" <|
        assertEqual (Axial 0 0) <|
        toAxial (Cube 0 0 0)

    , test "coordinate toAxial" <|
        assertEqual (Axial 2 -1) <|
        toAxial (Cube 2 -1 -1)

    , test "coordinate toCube" <|
        assertEqual (Cube 2 -1 -1) <|
        toCube (Axial 2 -1)

    , test "coordinate other point" <|
        assertEqual (Cube -1 0 1) <|
        toCube (Axial -1 1)

    , test "coordinate distances" <|
        assertEqual 4 <|
        distance (Cube 0 0 0) (Cube 4 -1 -3)

    , test "coordinate distances" <|
        assertEqual 5 <|
        distance (Cube -1 5 -4) (Axial 0 0)

    , test "are neighbors" <|
        assert <|
        areNeighbors (Cube 0 0 0) (Cube 1 -1 0)

    , test "are not neighbors" <|
        assertEqual False <|
        areNeighbors (Cube 0 0 0) (Cube 4 -2 -2)

    , test "are neighbors" <|
        assert <|
        areNeighbors (Cube 5 -2 -3) (Cube 4 -2 -2)

    , test "line draw" <|
        assertEqual [Cube 1 -4 3, Cube 0 -3 3,
          Cube 0 -2 2, Cube -1 -1 2] <|
        lineDraw (Cube 1 -4 3) (Cube -1 -1 2)

    , test "movement range" <|
        assertEqual [Cube 0 -3 3] <|
        movementRange (Cube 0 -3 3) 0

    , test "movement range" <|
        assertEqual [Cube -1 -3 4, Cube -1 -2 3, Cube 0 -4 4,
          Cube 0 -3 3, Cube 0 -2 2, Cube 1 -4 3, Cube 1 -3 2] <|
        movementRange (Cube 0 -3 3) 1

    , test "movement range and neighbors" <|
        assertEqual (sort ((Cube 4 -2 -2)
          :: (neighbors (Cube 4 -2 -2)))) <|
        sort (movementRange (Cube 4 -2 -2) 1)

    , test "rotate left" <|
        assertEqual (Cube -2 5 -3) <|
        rotateLeft (Cube 3 2 -5)

    , test "rotate right" <|
        assertEqual (Cube 1 -5 4) <|
        rotateRight (Cube 5 -4 -1)

    , test "obstacles - trivial" <|
      assertEqual 7 <|
      List.length (limitedFloodFill (Cube 5 -4 1) 1 [])

    , test "obstacles - easy" <|
      assertEqual 5 <|
      List.length (limitedFloodFill (Cube 0 0 0) 1
        [(Cube 1 -1 0), (Cube 0 -1 1)])

    , test "obstacles - moderate" <|
      assertEqual 11 <|
      List.length (limitedFloodFill (Cube 0 0 0) 2
        [(Cube 1 0 -1), (Cube 0 1 -1), (Cube -1 1 0)])
    ]
