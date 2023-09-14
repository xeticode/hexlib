module Hex exposing
    ( FlatDirection(..)
    , Hex(..)
    , add
    , axialToCubic
    , createFlatTopNeighborList
    , createFlatTopNeighborListWithWrap
    , cubicToAxial
    , getNeighborWithFlatDir
    , getQ
    , getR
    , getS
    , hexDistance
    , isEqual
    , mul
    , sub
    , unitFlatDirectionList
    )


type Hex
    = AxialHex Int Int
    | CubeHex Int Int Int
    | FloatCubeHex Float Float Float


type FlatDirection
    = N
    | NE
    | SE
    | S
    | SW
    | NW


unitFlatDirectionList : List FlatDirection
unitFlatDirectionList =
    [ N, NE, SE, S, SW, NW ]


getQ : Hex -> Int
getQ hex =
    case hex of
        AxialHex q _ ->
            q

        CubeHex q _ _ ->
            q

        FloatCubeHex q _ _ ->
            floor q


getR : Hex -> Int
getR hex =
    case hex of
        AxialHex _ r ->
            r

        CubeHex _ r _ ->
            r

        FloatCubeHex _ r _ ->
            floor r


getS : Hex -> Int
getS hex =
    case hex of
        AxialHex _ _ ->
            getS (axialToCubic hex)

        CubeHex _ _ s ->
            s

        FloatCubeHex _ _ s ->
            floor s


axialToCubic : Hex -> Hex
axialToCubic hex =
    case hex of
        AxialHex q r ->
            CubeHex q (-q - r) r

        CubeHex _ _ _ ->
            hex

        FloatCubeHex _ _ _ ->
            hex


cubicToAxial : Hex -> Hex
cubicToAxial hex =
    case hex of
        AxialHex _ _ ->
            hex

        CubeHex q _ s ->
            AxialHex q s

        FloatCubeHex _ _ _ ->
            hex


isEqual : Hex -> Hex -> Bool
isEqual h1 h2 =
    h1 == h2


add : Hex -> Hex -> Hex
add h1 h2 =
    case h1 of
        AxialHex q r ->
            case h2 of
                AxialHex q_ r_ ->
                    AxialHex (q + q_) (r + r_)

                CubeHex q_ r_ s_ ->
                    CubeHex (q + q_) ((-q - r) + r_) (r + s_)

                FloatCubeHex q_ r_ s_ ->
                    FloatCubeHex (toFloat q + q_) (toFloat (-q - r) + r_) (toFloat r + s_)

        CubeHex q r s ->
            case h2 of
                AxialHex q_ r_ ->
                    CubeHex (q + q_) (r + (-q_ - r_)) (s + r_)

                CubeHex q_ r_ s_ ->
                    CubeHex (q + q_) (r + r_) (s + s_)

                FloatCubeHex q_ r_ s_ ->
                    FloatCubeHex (toFloat q + q_) (toFloat r + r_) (toFloat s + s_)

        FloatCubeHex q r s ->
            case h2 of
                AxialHex q_ r_ ->
                    CubeHex (floor q + q_) (floor r + (-q_ - r_)) (floor s + r_)

                CubeHex q_ r_ s_ ->
                    CubeHex (floor q + q_) (floor r + r_) (floor s + s_)

                FloatCubeHex q_ r_ s_ ->
                    FloatCubeHex (q + q_) (r + r_) (s + s_)


sub : Hex -> Hex -> Hex
sub h1 h2 =
    case h1 of
        AxialHex q r ->
            case h2 of
                AxialHex q_ r_ ->
                    AxialHex (q - q_) (r - r_)

                CubeHex q_ r_ s_ ->
                    CubeHex (q - q_) ((-q - r) - r_) (r - s_)

                FloatCubeHex q_ r_ s_ ->
                    FloatCubeHex (toFloat q - q_) (toFloat (-q - r) - r_) (toFloat r - s_)

        CubeHex q r s ->
            case h2 of
                AxialHex q_ r_ ->
                    CubeHex (q - q_) (r - (-q_ - r_)) (s - r_)

                CubeHex q_ r_ s_ ->
                    CubeHex (q - q_) (r - r_) (s - s_)

                FloatCubeHex q_ r_ s_ ->
                    FloatCubeHex (toFloat q - q_) (toFloat r - r_) (toFloat s - s_)

        FloatCubeHex q r s ->
            case h2 of
                AxialHex q_ r_ ->
                    CubeHex (floor q - q_) (floor r - (-q_ - r_)) (floor s - r_)

                CubeHex q_ r_ s_ ->
                    CubeHex (floor q - q_) (floor r - r_) (floor s - s_)

                FloatCubeHex q_ r_ s_ ->
                    FloatCubeHex (q - q_) (r - r_) (s - s_)


mul : Hex -> Int -> Hex
mul hex scalar =
    case hex of
        AxialHex q r ->
            AxialHex (scalar * q) (scalar * r)

        CubeHex q r s ->
            CubeHex (scalar * q) (scalar * r) (scalar * s)

        FloatCubeHex q r s ->
            FloatCubeHex (toFloat scalar * q) (toFloat scalar * r) (toFloat scalar * s)


hexDistance : Hex -> Hex -> Int
hexDistance h1 h2 =
    let
        compare_hex =
            sub h1 h2
    in
    case compare_hex of
        AxialHex q r ->
            max q r

        CubeHex q r s ->
            max q (max r s)

        FloatCubeHex q r s ->
            max (floor q) (max (floor r) (floor s))


getNeighborWithFlatDir : Hex -> FlatDirection -> Hex
getNeighborWithFlatDir hex dir =
    case dir of
        N ->
            case hex of
                AxialHex q r ->
                    AxialHex q (r - 1)

                CubeHex q r s ->
                    CubeHex q (r + 1) (s - 1)

                FloatCubeHex q r s ->
                    FloatCubeHex q (r + 1) (s - 1)

        NE ->
            case hex of
                AxialHex q r ->
                    AxialHex (q + 1) (r - 1)

                CubeHex q r s ->
                    CubeHex (q + 1) r (s - 1)

                FloatCubeHex q r s ->
                    FloatCubeHex (q + 1) r (s - 1)

        SE ->
            case hex of
                AxialHex q r ->
                    AxialHex (q + 1) r

                CubeHex q r s ->
                    CubeHex (q + 1) (r - 1) s

                FloatCubeHex q r s ->
                    FloatCubeHex (q + 1) (r - 1) s

        S ->
            case hex of
                AxialHex q r ->
                    AxialHex q (r + 1)

                CubeHex q r s ->
                    CubeHex q (r - 1) (s + 1)

                FloatCubeHex q r s ->
                    FloatCubeHex q (r - 1) (s + 1)

        SW ->
            case hex of
                AxialHex q r ->
                    AxialHex (q - 1) (r + 1)

                CubeHex q r s ->
                    CubeHex (q - 1) r (s + 1)

                FloatCubeHex q r s ->
                    FloatCubeHex (q - 1) r (s + 1)

        NW ->
            case hex of
                AxialHex q r ->
                    AxialHex (q - 1) r

                CubeHex q r s ->
                    CubeHex (q - 1) (r + 1) s

                FloatCubeHex q r s ->
                    FloatCubeHex (q - 1) (r + 1) s


createFlatTopNeighborList : Hex -> List Hex
createFlatTopNeighborList hex =
    List.map (getNeighborWithFlatDir hex) unitFlatDirectionList

createFlatTopNeighborListWithWrap : Hex -> Int -> Int -> Int -> Int -> List Hex
createFlatTopNeighborListWithWrap hex min_col max_col min_row max_row =
    let
        base_neighbors =
            List.map (getNeighborWithFlatDir hex) unitFlatDirectionList
    in
    List.map
        (\h ->
            case h of
                AxialHex q r ->
                    let
                        min_q =
                            min_col

                        max_q =
                            max_col

                        min_r =
                            (0 - ((q - 1) //2)) + min_row

                        max_r =
                            (0 - ((q - 1) //2)) + max_row

                        modified_q =
                            if q < min_q then
                                max_col

                            else if q > max_q then
                                min_col

                            else
                                q

                        modified_r =
                            if modified_q - q > 0 then
                                if r - 1 < min_r then
                                    (0 - ((max_col - 1) //2)) + max_row

                                else
                                    (0 - ((max_col - 1) //2)) + r - 1

                            else if modified_q == q then
                                if r < min_r then
                                    (0 - ((q - 1) //2)) + max_row

                                else if r > max_r then
                                    min_r

                                else
                                    r

                            else
                                if r > max_r then
                                    (0 - ((max_col - 1) //2)) + max_r
                                
                                else
                                    (0 - ((max_col - 1) //2)) + r - 1 + max_col
                    in
                        AxialHex modified_q modified_r

                CubeHex q _ r ->
                    let
                        min_q =
                            min_col

                        max_q =
                            max_col

                        min_r =
                            (0 - ((q - 1) //2)) + min_row

                        max_r =
                            (0 - ((q - 1) //2)) + max_row

                        modified_q =
                            if q < min_q then
                                max_col

                            else if q > max_q then
                                min_col

                            else
                                q

                        modified_r =
                            if modified_q - q > 0 then
                                if r - 1 < min_r then
                                    (0 - ((max_col - 1) //2)) + max_row

                                else
                                    (0 - ((max_col - 1) //2)) + r - 1

                            else if modified_q == q then
                                if r < min_r then
                                    (0 - ((q - 1) //2)) + max_row

                                else if r > max_r then
                                    min_r

                                else
                                    r

                            else
                                if r > max_r then
                                    (0 - ((max_col - 1) //2)) + max_r
                                
                                else
                                    (0 - ((max_col - 1) //2)) + r - 1 + max_col
                    in
                        axialToCubic <| AxialHex modified_q modified_r

                FloatCubeHex _ _ _ -> -- convert to CubeHex first
                    h
        )
        base_neighbors