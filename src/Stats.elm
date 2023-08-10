module Stats exposing (mostCommon)

import Dict


mostCommon : List Int -> List Int
mostCommon vals =
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
    frequencies
        |> Dict.filter (\_ freq -> Just freq == List.maximum frequencyValues)
        |> Dict.keys
