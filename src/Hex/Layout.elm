module Hex.Layout exposing
    ( AngleMatrix
    , Layout
    , Orientation
    , Point
    , cornerToString
    , createCornerCoordsString
    , createCornerStringsList
    , createCornersList
    , createHexList
    , floatHexRound
    , get4thCornerCoords
    , get5thCornerCoords
    , hexCornerOffset
    , hexToPoint
    , pointStringToPoint
    , pointToHex
    , polygonCorners
    , splitCornerCoordsString
    , unitFlatForwardAngleMatrix
    , unitFlatInverseAngleMatrix
    , unitFlatTopOrientation
    )

import Array as A
import Hex


type alias AngleMatrix =
    { f0 : Float
    , f1 : Float
    , f2 : Float
    , f3 : Float
    }


type alias Point =
    ( Float, Float )


type alias Orientation =
    { forward_matrix : AngleMatrix
    , inverse_matrix : AngleMatrix
    , start_angle : Float
    }


type alias Layout =
    { orientation : Orientation
    , size : Point
    , origin : Point
    }


unitFlatTopOrientation : Orientation
unitFlatTopOrientation =
    { forward_matrix = unitFlatForwardAngleMatrix
    , inverse_matrix = unitFlatInverseAngleMatrix
    , start_angle = 0.0
    }


unitFlatForwardAngleMatrix : AngleMatrix
unitFlatForwardAngleMatrix =
    { f0 = 3.0 / 2.0
    , f1 = 0.0
    , f2 = sqrt 3.0 / 2.0
    , f3 = sqrt 3.0
    }


unitFlatInverseAngleMatrix : AngleMatrix
unitFlatInverseAngleMatrix =
    { f0 = 2.0 / 3.0
    , f1 = 0.0
    , f2 = -1.0 / 3.0
    , f3 = sqrt 3.0 / 3.0
    }


hexToPoint : Layout -> Hex.Hex -> Point
hexToPoint layout hex =
    case hex of
        Hex.AxialHex q r ->
            let
                fm =
                    layout.orientation.forward_matrix

                x =
                    -- Debug.log "hexToPoint x" <|
                    ((fm.f0 * toFloat q) + (fm.f1 * toFloat r)) * Tuple.first layout.size

                y =
                    -- Debug.log "hexToPoint y" <|
                    ((fm.f2 * toFloat q) + (fm.f3 * toFloat r)) * Tuple.second layout.size
            in
            ( x + Tuple.first layout.origin, y + Tuple.second layout.origin )

        Hex.CubeHex q _ s ->
            let
                fm =
                    layout.orientation.forward_matrix

                x =
                    ((fm.f0 * toFloat q) + (fm.f1 * toFloat s)) * Tuple.first layout.size

                y =
                    ((fm.f2 * toFloat q) + (fm.f3 * toFloat s)) * Tuple.second layout.size
            in
            ( x + Tuple.first layout.origin, y + Tuple.second layout.origin )

        Hex.FloatCubeHex q _ s ->
            let
                fm =
                    layout.orientation.forward_matrix

                x =
                    ((fm.f0 * q) + (fm.f1 * s)) * Tuple.first layout.size

                y =
                    ((fm.f2 * q) + (fm.f3 * s)) * Tuple.second layout.size
            in
            ( x + Tuple.first layout.origin, y + Tuple.second layout.origin )


pointToHex : Layout -> Point -> Hex.Hex
pointToHex layout ( px, py ) =
    let
        inv_m =
            layout.orientation.inverse_matrix

        ( x, y ) =
            ( (px - Tuple.first layout.origin) / Tuple.first layout.size, (py - Tuple.second layout.origin) / Tuple.second layout.size )

        q =
            (inv_m.f0 * x) + (inv_m.f1 * y)

        r =
            (inv_m.f2 * x) + (inv_m.f3 * y)
    in
    floatHexRound (Hex.FloatCubeHex q (-q - r) r)



-- the 'r' and 's' positions on this FloatCubeHex may need to be switched.  Its a little confusing


hexCornerOffset : Layout -> Int -> Point
hexCornerOffset layout corner =
    let
        ( sx, sy ) =
            layout.size

        angle =
            2.0 * pi * (layout.orientation.start_angle + toFloat corner) / 6
    in
    ( sx * cos angle, sy * sin angle )


polygonCorners : Layout -> Hex.Hex -> A.Array Point
polygonCorners layout hex =
    let
        ( x, y ) =
            hexToPoint layout hex

        offset_list =
            List.map (hexCornerOffset layout) (List.range 0 5)
    in
    A.map (\( ox, oy ) -> ( x + ox, y + oy )) <| A.fromList offset_list


floatHexRound : Hex.Hex -> Hex.Hex
floatHexRound hex =
    case hex of
        Hex.AxialHex _ _ ->
            hex

        Hex.CubeHex _ _ _ ->
            hex

        Hex.FloatCubeHex q_ r_ s_ ->
            let
                q =
                    round q_

                r =
                    round r_

                s =
                    round s_

                q_diff =
                    abs (toFloat q - q_)

                r_diff =
                    abs (toFloat r - r_)

                s_diff =
                    abs (toFloat s - s_)
            in
            if q_diff > r_diff && q_diff > s_diff then
                Hex.CubeHex (-r - s) r s

            else if r_diff > s_diff then
                Hex.CubeHex q (-q - s) s

            else
                Hex.CubeHex q r (-q - r)


createCornerStringsList : List (A.Array Point) -> List String
createCornerStringsList list_corners =
    List.map createCornerCoordsString list_corners


createHexList : Int -> Int -> List Hex.Hex
createHexList num_cols num_rows =
    List.concatMap (\a -> List.map (\b -> Hex.AxialHex a b) (List.range (0 - (a // 2)) (num_rows - (a // 2) - 1))) (List.range 0 (num_cols - 1))


createCornersList : Layout -> List Hex.Hex -> List (A.Array Point)
createCornersList layout list_hex =
    List.map (polygonCorners layout) list_hex


createCornerCoordsString : A.Array Point -> String
createCornerCoordsString corners =
    A.foldl cornerToString "" corners


cornerToString : Point -> String -> String
cornerToString ( x, y ) string =
    if String.length string == 0 then
        string ++ String.fromFloat x ++ "," ++ String.fromFloat y

    else
        string ++ " " ++ String.fromFloat x ++ "," ++ String.fromFloat y


splitCornerCoordsString : String -> List String
splitCornerCoordsString corner_coords_string =
    String.split " " corner_coords_string


get5thCornerCoords : String -> Point
get5thCornerCoords corner_string =
    let
        list_coord_string =
            splitCornerCoordsString corner_string

        fifth_corner_coord =
            Maybe.withDefault "0,0" (A.get 4 (A.fromList list_coord_string))

        return_point =
            pointStringToPoint fifth_corner_coord
    in
    return_point


get4thCornerCoords : String -> Point
get4thCornerCoords corner_string =
    let
        list_coord_string =
            splitCornerCoordsString corner_string

        fourth_corner_coord =
            Maybe.withDefault "0,0" (A.get 3 (A.fromList list_coord_string))

        return_point =
            pointStringToPoint fourth_corner_coord
    in
    return_point


pointStringToPoint : String -> Point
pointStringToPoint str =
    let
        int_str_list =
            String.split "," str

        x =
            Maybe.withDefault 0 (String.toFloat (Maybe.withDefault "0" (A.get 0 (A.fromList int_str_list))))

        y =
            Maybe.withDefault 0 (String.toFloat (Maybe.withDefault "0" (A.get 1 (A.fromList int_str_list))))
    in
    ( x, y )
