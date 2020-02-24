module Main exposing (main)

import Browser
import Html exposing (Html, button, div, h2, img, input, label, text)
import Html.Attributes exposing (alt, checked, src, type_)
import Html.Events exposing (onClick)
import Http
import Json.Decode exposing (Decoder, field, list, string)
import List exposing (map)
import Random
import Random.List exposing (choose)



-- MAIN


main : Program () Model Msg
main =
    Browser.element
        { init = init
        , update = update
        , subscriptions = subscriptions
        , view = view
        }



-- MODEL


type ApiStatus a
    = Failure
    | Loading
    | Success a


type Model
    = GetSets (ApiStatus (List String))
    | Choosing (List String) (List String) -- all sets to choose, and those currently chosen
    | GetCards (List String) (List String) (ApiStatus (List String)) -- as above


init : () -> ( Model, Cmd Msg )
init _ =
    ( GetSets Loading, getSets )



-- UPDATE


type Msg
    = Load
    | GotSets (Result Http.Error (List String))
    | Toggle String
    | Generate (List String)
    | GotCards (Result Http.Error (List String))
    | Randomised (List String)


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Load ->
            ( GetSets Loading, getSets )

        GotSets result ->
            case result of
                Ok sets ->
                    ( GetSets (Success sets), Cmd.none )

                Err _ ->
                    ( GetSets Failure, Cmd.none )

        Toggle toggled ->
            let
                newSets current =
                    if List.member toggled current then
                        List.filter ((/=) toggled) current

                    else
                        toggled :: current
            in
            case model of
                GetSets (Success all) ->
                    ( Choosing all (newSets []), Cmd.none )

                GetSets status ->
                    ( GetSets status, Cmd.none )

                Choosing all current ->
                    ( Choosing all (newSets current), Cmd.none )

                GetCards all current status ->
                    ( GetCards all (newSets current) status, Cmd.none )

        GotCards result ->
            case result of
                Ok cardlist ->
                    case model of
                        GetSets status ->
                            ( GetSets status, Cmd.none )

                        Choosing all current ->
                            ( GetCards all current Loading, Random.generate Randomised (randomiser 10 cardlist) )

                        GetCards all current _ ->
                            ( GetCards all current Loading, Random.generate Randomised (randomiser 10 cardlist) )

                Err _ ->
                    case model of
                        GetSets status ->
                            ( GetSets status, Cmd.none )

                        Choosing all current ->
                            ( Choosing all current, Cmd.none )

                        GetCards all current _ ->
                            ( GetCards all current Failure, Cmd.none )

        Generate chosen ->
            case model of
                GetSets status ->
                    ( GetSets status, Cmd.none )

                Choosing all current ->
                    ( GetCards all current Loading, getCards chosen )

                GetCards all current _ ->
                    ( GetCards all current Loading, getCards chosen )

        Randomised cards ->
            case model of
                GetSets status ->
                    ( GetSets status, Cmd.none )

                Choosing all current ->
                    ( Choosing all current, Cmd.none )

                GetCards all current _ ->
                    ( GetCards all current (Success cards), Cmd.none )



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions _ =
    Sub.none



-- VIEW


view : Model -> Html Msg
view model =
    div []
        [ h2 [] [ text "Dominion cardpicker" ]
        , viewMore model
        ]


viewMore : Model -> Html Msg
viewMore model =
    case model of
        GetSets Failure ->
            div []
                [ text "There was a problem loading the set names."
                , button [ onClick Load ] [ text "Try Again!" ]
                ]

        GetSets Loading ->
            text "Loading sets..."

        GetSets (Success sets) ->
            setChoice sets []

        Choosing sets chosen ->
            setChoice sets chosen

        GetCards sets chosen status ->
            div [] [ setChoice sets chosen, viewCards status ]


setChoice : List String -> List String -> Html Msg
setChoice setnames chosen =
    setnames
        |> map (\set -> div [] [ label [] [ text set ], input [ type_ "checkbox", onClick (Toggle set), checked (List.member set chosen) ] [] ])
        |> (\html -> html ++ [ button [ onClick (Generate chosen) ] [ text "generate cards" ] ])
        |> div []


viewCards : ApiStatus (List String) -> Html Msg
viewCards status =
    case status of
        Failure ->
            div []
                [ text "There was a problem getting cards."
                , button [ onClick Load ] [ text "Try Again!" ]
                ]

        Loading ->
            text "Loading cards..."

        Success cards ->
            div []
                [ div [] <|
                    map getCardImage cards
                ]


getCardImage : String -> Html Msg
getCardImage name =
    let
        transformedName =
            "%PUBLIC_URL%/card-images/"
                ++ (name |> String.split "-" |> map titleCase |> String.join "_")
                ++ ".jpg"

        titleCase str =
            case String.uncons str of
                Nothing ->
                    ""

                Just ( first, rest ) ->
                    String.cons (Char.toUpper first) rest
    in
    img [ src transformedName, alt name ] []



-- HTTP


getSets : Cmd Msg
getSets =
    Http.get
        { url = "http://dominion.zigmond.uk/sets"
        , expect = Http.expectJson GotSets setsDecoder
        }


getCards : List String -> Cmd Msg
getCards sets =
    let
        url =
            "http://dominion.zigmond.uk/cards?is-kingdom&set=" ++ String.join "&set=" sets
    in
    Http.get
        { url = url
        , expect = Http.expectJson GotCards cardDecoder
        }


setsDecoder : Decoder (List String)
setsDecoder =
    list string


cardDecoder : Decoder (List String)
cardDecoder =
    list (field "name" string)



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
                                        Random.constant left

                                    Just a ->
                                        go (x - 1) left (a :: sofar)
                            )
    in
    go n l []
