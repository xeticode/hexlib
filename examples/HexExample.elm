module HexExample exposing (main)

import Browser
import Hex
import Hex.Layout as HexL
import Html.Styled as H
import Html.Styled.Attributes as HA
import Html.Styled.Events as HE
import List as L
import Svg.Styled as S
import Svg.Styled.Attributes as SA
import Svg.Styled.Events as SE


main : Program () Model Msg
main =
    Browser.element
        { init = init
        , update = update
        , subscriptions = subscriptions
        , view = \m -> H.toUnstyled <| view m
        }


-- Model


type alias Model =
    { layout : HexL.Layout
    , inner_layout : HexL.Layout
    , num_cols : Int
    , num_rows : Int
    , selected_corner_coords : String
    }


init : () -> ( Model, Cmd Msg )
init () =
    let
        var =
            45.0

        model =
            { layout =
                { orientation = HexL.unitFlatTopOrientation
                , size = ( var, var )
                , origin = ( 1.5 * var, 1.5 * var )
                }
            , inner_layout =
                { orientation = HexL.unitFlatTopOrientation
                , size = ( var / 3, var / 3 )
                , origin = ( -var, 0 )
                }
            , num_cols = 15
            , num_rows = 12
            , selected_corner_coords = ""
            }
    in
    ( model, Cmd.none )



-- Update


type Msg
    = UpdateNumCols String
    | UpdateNumRows String
    | ToggleSelected String


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        UpdateNumCols num_cols_str ->
            let
                new_model =
                    { model | num_cols = Maybe.withDefault 10 (String.toInt num_cols_str) }
            in
            ( new_model, Cmd.none )

        UpdateNumRows num_rows_str ->
            let
                new_model =
                    { model | num_rows = Maybe.withDefault 10 (String.toInt num_rows_str) }
            in
            ( new_model, Cmd.none )

        ToggleSelected corner_coords ->
            let
                new_model =
                    if corner_coords == model.selected_corner_coords then
                        { model | selected_corner_coords = "" }

                    else
                        { model | selected_corner_coords = corner_coords }
            in
            ( new_model, Cmd.none )



-- Subscriptions


subscriptions : Model -> Sub Msg
subscriptions _ =
    Sub.none



-- View


view : Model -> H.Html Msg
view model =
    let
        hex_list =
            Debug.log "hex_list" (HexL.createHexList model.num_cols model.num_rows)

        corners_list =
            Debug.log "corners_list" (HexL.createCornersList model.layout hex_list)

        list_corner_coords_strings =
            Debug.log "list_corner_coords_strings" (HexL.createCornerStringsList corners_list)

        range_min_str =
            "1"

        range_max_str =
            "20"
    in
    H.div
        []
        [ S.svg
            [ SA.x "0", SA.y "0", SA.height (String.fromFloat (svgHeight model)), SA.width (String.fromFloat (svgWidth model)) ]
            (hexGrid model.selected_corner_coords model.inner_layout list_corner_coords_strings)
        , H.div
            [ HA.style "display" "flex" ]
            [ H.div
                [ HA.style "flex" "1" ]
                [ H.input
                    [ HE.onInput UpdateNumCols, HA.style "display" "block", HA.style "margin" "auto", HA.type_ "range", HA.min range_min_str, HA.max range_max_str, HA.value (String.fromInt model.num_cols) ]
                    []
                , H.div
                    [ HA.style "text-align" "center" ]
                    [ H.text ("Number of Columns: " ++ String.fromInt model.num_cols) ]
                ]
            , H.div
                [ HA.style "flex" "1" ]
                [ H.input
                    [ HE.onInput UpdateNumRows, HA.style "display" "block", HA.style "margin" "auto", HA.type_ "range", HA.min range_min_str, HA.max range_max_str, HA.value (String.fromInt model.num_rows) ]
                    []
                , H.div
                    [ HA.style "text-align" "center" ]
                    [ H.text ("Number of Rows: " ++ String.fromInt model.num_rows) ]
                ]
            ]
        ]




-- View Helpers


hexGrid : String -> HexL.Layout -> List String -> List (S.Svg Msg)
hexGrid comp_string inner_layout list_corner_coords_strings =
    List.map (createHex comp_string inner_layout) list_corner_coords_strings


createHex : String -> HexL.Layout -> String -> S.Svg Msg
createHex comp_string inner_layout corner_coords =
    let
        split_corner_coords =
            Debug.log "split_corner_coords" (HexL.splitCornerCoordsString corner_coords)

        new_inner_layout =
            Debug.log "new_inner_layout" (case List.head split_corner_coords of
                Just coord_string ->
                    let
                        ( first_point, second_point ) =
                            HexL.pointStringToPoint coord_string

                        first_origin =
                            Tuple.first inner_layout.origin + first_point

                        second_origin =
                            Tuple.second inner_layout.origin + second_point
                    in
                    { inner_layout | origin = ( first_origin, second_origin ) }

                Nothing ->
                    inner_layout)

        list_inner_coords_strings =
            Debug.log "list_inner_coords_strings" (HexL.createCornerStringsList (HexL.createCornersList new_inner_layout sectorsHexList))
    in
    S.g
        []
        (sectorHexGrid
            comp_string
            corner_coords
            list_inner_coords_strings
        )


sectorsHexList : List Hex.Hex
sectorsHexList =
    [ Hex.AxialHex 0 -1, Hex.AxialHex 1 -1, Hex.AxialHex 1 0, Hex.AxialHex 0 1, Hex.AxialHex -1 1, Hex.AxialHex -1 0, Hex.AxialHex 0 0 ]


sectorHexGrid : String -> String -> List String -> List (S.Svg Msg)
sectorHexGrid comp_string corner_coords list_corner_coords_strings =
    let
        bg_hex_poly =
            S.polygon
                [ SA.stroke "navy", SA.strokeWidth "1px", SA.fill "grey", SA.points corner_coords ]
                []
    in
    [ bg_hex_poly ] ++ List.map (createSectorHex comp_string) list_corner_coords_strings


createSectorHex : String -> String -> S.Svg Msg
createSectorHex selected_corner_coords corner_coords =
    if corner_coords == selected_corner_coords then
        S.polygon
            [ SA.stroke "black", SA.strokeWidth "1px", SA.fill "Yellow", SA.points corner_coords, SE.onClick (ToggleSelected corner_coords) ]
            []

    else
        S.polygon
            [ SA.stroke "black", SA.strokeWidth "1px", SA.fill "silver", SA.points corner_coords, SE.onClick (ToggleSelected corner_coords) ]
            []


svgHeight : Model -> Float
svgHeight model =
    let
        ( _, second_size ) =
            model.layout.size
    in
    (second_size * sqrt 3.0 * toFloat model.num_rows) + Tuple.second model.layout.origin + 4


svgWidth : Model -> Float
svgWidth model =
    let
        ( first_size, _ ) =
            model.layout.size
    in
    ((first_size * 2) * 3 / 4 * toFloat model.num_cols) + Tuple.first model.layout.origin
