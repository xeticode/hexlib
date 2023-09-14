module HexLayoutTest exposing (suite)

import Expect exposing (..)
import Fuzz exposing (..)
import Hex exposing (..)
import Hex.Layout exposing (..)
import Test exposing (..)


suite : Test
suite =
    describe "Hex.Layout Tests"
        [ test "hexToPoint" <|
            \_ ->
                let
                    layout =
                        Layout unitFlatTopOrientation ( 10, 10 ) ( 15, 15 )

                    fm =
                        layout.orientation.forward_matrix

                    x =
                        ((fm.f0 * toFloat 2) + (fm.f1 * toFloat 1)) * Tuple.first layout.size

                    y =
                        ((fm.f2 * toFloat 2) + (fm.f3 * toFloat 1)) * Tuple.second layout.size

                    produced_point =
                        hexToPoint layout (AxialHex 2 1)
                in
                Expect.equal ( x + Tuple.first layout.origin, y + Tuple.second layout.origin ) produced_point
        , test "pointToHex" <|
            \_ ->
                let
                    layout =
                        Layout unitFlatTopOrientation ( 10, 10 ) ( 15, 15 )

                    inv_m =
                        layout.orientation.inverse_matrix

                    ( x, y ) =
                        ( (30 - Tuple.first layout.origin) / Tuple.first layout.size, (40 - Tuple.second layout.origin) / Tuple.second layout.size )

                    q =
                        (inv_m.f0 * x) + (inv_m.f1 * y)

                    r =
                        (inv_m.f2 * x) + (inv_m.f3 * y)

                    produced_float_hex =
                        pointToHex layout ( 30, 40 )
                in
                Expect.equal (FloatCubeHex q r (-q - r)) produced_float_hex
        , test "hexCornerOffset" <|
            \_ ->
                let
                    layout =
                        Layout unitFlatTopOrientation ( 10, 10 ) ( 15, 15 )
                in
                Expect.equal ( 10, 0 ) (hexCornerOffset layout 0)
        , todo "polygonCorners"

        -- , test "polygonCorners" <|
        --     \_ ->
        --         let
        --             layout =
        --                 Layout unitFlatTopOrientation ( 10, 10 ) ( 15, 15 )
        --             produced_list =
        --                 polygonCorners layout (AxialHex 0 0)
        --             test_list =
        --                 [ ( 75, 45 ), ( 60, 70.98076211353316 ), ( 30.000000000000007, 70.98076211353316 ), ( 15, 45.00000000000001 ), ( 29.99999999999999, 19.019237886466843 ), ( 60, 19.019237886466843 ) ]
        --         in
        --         Expect.equal test_list produced_list
        , todo "sectorPolygonCorners"

        -- , test "sectorPolygonCorners" <|
        --     \_ ->
        --         let
        --             layout =
        --                 Layout unitFlatTopOrientation ( 10, 10 ) ( 15, 15 )
        --             ( x, y ) =
        --                 hexToPoint layout (AxialHex 0 0)
        --             offset_list =
        --                 List.map (hexCornerOffset layout) (List.range 0 5)
        --             sector_offset_y =
        --                 Tuple.first layout.size + 2
        --             produced_list =
        --                 sectorPolygonCorners layout ( 20, 20 ) (AxialHex 0 0)
        --             test_list =
        --                 List.map (\( ox, oy ) -> ( (x + ox) + 20, (y + oy) + 20 + sector_offset_y )) offset_list
        --         in
        --         Expect.equal test_list produced_list
        , test "floatHexRound" <|
            \_ ->
                let
                    q =
                        round (13 / 2)

                    r =
                        round (-3 / 2)

                    s =
                        round -5

                    q_diff =
                        abs (toFloat q - 13 / 2)

                    r_diff =
                        abs (toFloat r - (-3 / 2))

                    s_diff =
                        abs (toFloat s - -5)

                    return_hex =
                        if q_diff > r_diff && q_diff > s_diff then
                            CubeHex (-r - s) r s

                        else if r_diff > s_diff then
                            CubeHex q (-q - s) s

                        else
                            CubeHex q r (-q - r)
                in
                Expect.equal return_hex (floatHexRound (FloatCubeHex (13 / 2) (-3 / 2) -5))
        , test "createHexList" <|
            \_ ->
                let
                    a =
                        10

                    b =
                        10
                in
                Expect.equal (a * b) (List.length (createHexList a b))
        , test "cornerToString" <|
            \_ ->
                Expect.equal "10,5 5,8" (cornerToString ( 5, 8 ) "10,5")
        , test "get5thCornerCoords" <|
            \_ ->
                Expect.equal ( 10, 5 ) (get5thCornerCoords "1,1 2,2 3,3 4,4 10,5 6,6")
        , test "pointStringToPoint" <|
            \_ ->
                Expect.equal ( 20, 3 ) (pointStringToPoint "20,3")
        ]
