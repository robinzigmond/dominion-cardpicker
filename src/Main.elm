module Main exposing (main)

import Browser
import Html exposing (Html, button, div, h2, img, text)
import Html.Attributes exposing (src, alt)
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


type Model
    = Failure
    | Loading
    | Success (List String)


init : () -> ( Model, Cmd Msg )
init _ =
    ( Loading, getCards )



-- UPDATE


type Msg
    = Load
    | GotCards (Result Http.Error (List String))
    | Randomised (List String)


update : Msg -> Model -> ( Model, Cmd Msg )
update msg _ =
    case msg of
        Load ->
            ( Loading, getCards )

        GotCards result ->
            case result of
                Ok cardlist ->
                    ( Loading, Random.generate Randomised (randomiser 10 cardlist) )

                Err _ ->
                    ( Failure, Cmd.none )

        Randomised cards -> (Success cards, Cmd.none)



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions _ =
    Sub.none



-- VIEW


view : Model -> Html Msg
view model =
    div []
        [ h2 [] [ text "Dominion cards" ]
        , viewCards model
        ]


viewCards : Model -> Html Msg
viewCards model =
    case model of
        Failure ->
            div []
                [ text "There was a problem getting cards."
                , button [ onClick Load ] [ text "Try Again!" ]
                ]

        Loading ->
            text "Loading cards..."

        Success cards ->
            div []
                [div [] <|
                    map getCardImage cards,
                    button [ onClick Load ] [ text "Get 10 more random cards" ]]


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


getCards : Cmd Msg
getCards =
    Http.get
        { url = "http://dominion.zigmond.uk/cards?is-kingdom&set=base-second-ed"
        , expect = Http.expectJson GotCards cardDecoder
        }


cardDecoder : Decoder (List String)
cardDecoder =
    list (field "name" string)



randomiser : Int -> List a -> Random.Generator (List a)
-- general random generator to pick n distinct elements from a list
randomiser n l =
    let go m xs sofar = case m of
            0 -> Random.constant sofar
            x -> choose xs
                    |> Random.andThen (\(maybechosen, left) -> case maybechosen of
                        Nothing -> Random.constant left
                        Just a -> go (x - 1) left (a :: sofar))
    in go n l []
