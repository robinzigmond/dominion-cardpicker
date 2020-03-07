module Randomisers exposing (combineRandoms, filteredRandom, randomiser)

import Random
import Random.List exposing (choose)



-- general random generator to pick n distinct elements from a list


randomiser : Int -> List a -> Random.Generator (List a)
randomiser n l =
    let
        go m xs sofar =
            case m of
                0 ->
                    Random.constant sofar

                x ->
                    choose xs
                        |> Random.andThen
                            (\( maybechosen, left ) ->
                                case maybechosen of
                                    Nothing ->
                                        Random.constant sofar

                                    Just a ->
                                        go (x - 1) left (a :: sofar)
                            )
    in
    go n l []


combineRandoms : Random.Generator (List a) -> Random.Generator (List a) -> Random.Generator (List a)
combineRandoms =
    Random.map2 (++)


filteredRandom : (a -> Bool) -> Int -> List a -> Random.Generator (List a)
filteredRandom p n l =
    randomiser n (List.filter p l)
