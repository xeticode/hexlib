module HexTest exposing (suite)

import Expect exposing (..)
import Fuzz exposing (..)
import Hex exposing (..)
import Hex.Layout exposing (..)
import Test exposing (..)


suite : Test
suite =
    describe "Hex.elm Tests"
        [ test "getQ" <|
            \_ ->
                Expect.equal 5 (getQ (CubeHex 5 -3 -2))
        , test "getR" <|
            \_ ->
                Expect.equal -3 (getR (CubeHex 5 -3 -2))
        , test "getS" <|
            \_ ->
                Expect.equal -2 (getS (CubeHex 5 -3 -2))
        , test "axialToCubic" <|
            \_ ->
                Expect.equal (CubeHex 5 -3 -2) (axialToCubic (AxialHex 5 -2))
        , test "isEqual should be equal" <|
            \_ ->
                Expect.equal True (isEqual (CubeHex 1 1 -2) (axialToCubic (AxialHex 1 -2)))
        , describe "add"
            [ test "axial and axial" <|
                \_ ->
                    let
                        produced_hex =
                            add (AxialHex 1 -3) (AxialHex 2 2)
                    in
                    Expect.equal (AxialHex 3 -1) produced_hex
            , test "axial and cubic" <|
                \_ ->
                    let
                        produced_hex =
                            add (AxialHex 1 1) (CubeHex 1 -3 2)
                    in
                    Expect.equal (CubeHex 2 -5 3) produced_hex
            ]
        , describe "sub"
            [ test "cube and cube" <|
                \_ ->
                    let
                        produced_hex =
                            sub (CubeHex 1 2 -3) (CubeHex -1 -1 2)
                    in
                    Expect.equal (CubeHex 2 3 -5) produced_hex
            , test "axial and cube" <|
                \_ ->
                    let
                        produced_hex =
                            sub (AxialHex 1 -2) (CubeHex 1 4 -5)
                    in
                    Expect.equal (CubeHex 0 -3 3) produced_hex
            ]
        , test "mul" <|
            \_ ->
                Expect.equal (AxialHex 5 5) (mul (AxialHex 1 1) 5)
        , test "hexDistance" <|
            \_ ->
                Expect.equal 2 (hexDistance (CubeHex 1 2 -3) (CubeHex 5 0 -5))
        , todo "getNeighborWithDir"

        -- , test "getNeighborWithDir" <|
        --     \_ ->
        --         let
        --             produced_hex =
        --                 getNeighborWithDir (CubeHex 0 -1 1) N
        --         in
        --         Expect.equal (CubeHex 0 0 0) produced_hex
        , todo "createNeighborList"

        -- , test "createNeighborList" <|
        --     \_ ->
        --         let
        --             test_list =
        --                 [ CubeHex 0 1 -1, CubeHex 1 0 -1, CubeHex 1 -1 0, CubeHex 0 -1 1, CubeHex -1 0 1, CubeHex -1 1 0 ]
        --             -- Order of hexes in the list matters
        --             produced_neighbor_list =
        --                 createNeighborList (CubeHex 0 0 0)
        --         in
        -- Expect.equal test_list produced_neighbor_list
        ]
