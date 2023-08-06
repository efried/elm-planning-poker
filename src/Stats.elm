module Stats exposing (mode)

import Dict


mode : List Int -> List Int
mode vals =
    let
        frequencies : Dict.Dict Int Int
        frequencies =
            List.foldl
                (\val dict ->
                    Dict.update val
                        (\mv -> Maybe.map ((+) 1) mv |> Maybe.withDefault 1 |> Just)
                        dict
                )
                Dict.empty
                vals

        frequencyValues : List Int
        frequencyValues =
            Dict.values frequencies
    in
    if List.maximum frequencyValues == List.minimum frequencyValues then
        []

    else
        frequencies
            |> Dict.filter (\_ freq -> Just freq == List.maximum frequencyValues)
            |> Dict.keys
