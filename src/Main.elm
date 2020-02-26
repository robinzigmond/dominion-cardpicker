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



-- TYPES


type Sets
    = Sets (List String)


type Promos
    = Promos (List String)


type alias SetsToChoose =
    { sets : Sets, promos : Promos }


type Cards
    = Cards (List String)



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
    = GetSets (ApiStatus SetsToChoose)
    | Choosing SetsToChoose SetsToChoose -- all sets to choose, and those currently chosen
    | GetCards SetsToChoose SetsToChoose (ApiStatus Cards) -- as above


init : () -> ( Model, Cmd Msg )
init _ =
    ( GetSets Loading, getSets )



-- UPDATE


type Msg
    = Load
    | GotSets (Result Http.Error Sets)
    | GotPromos (Result Http.Error Promos)
    | Toggle String
    | Generate SetsToChoose
    | GotCards (Result Http.Error Cards)
    | Randomised Cards


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Load ->
            ( GetSets Loading, getSets )

        GotSets result ->
            case result of
                Ok sets ->
                    ( GetSets (Success { sets = sets, promos = Promos [] }), getPromos )

                Err _ ->
                    ( GetSets Failure, Cmd.none )

        GotPromos result ->
            case result of
                Ok promos ->
                    case model of
                        GetSets (Success setsandpromos) ->
                            let
                                allsets =
                                    case setsandpromos.sets of
                                        Sets s ->
                                            s
                            in
                            ( GetSets <| Success <| { sets = Sets (List.filter ((/=) "promo") allsets), promos = promos }, Cmd.none )

                        GetSets status ->
                            ( GetSets status, Cmd.none )

                        Choosing all current ->
                            ( Choosing all current, Cmd.none )

                        GetCards all current status ->
                            ( GetCards all current status, Cmd.none )

                Err _ ->
                    ( GetSets Failure, Cmd.none )

        Toggle toggled ->
            let
                newSets (Sets allSets) (Promos allPromos) current =
                    let
                        currentsets =
                            case current.sets of
                                Sets s ->
                                    s

                        currentpromos =
                            case current.promos of
                                Promos p ->
                                    p
                    in
                    if List.member toggled allSets then
                        if List.member toggled currentsets then
                            { current | sets = Sets (List.filter ((/=) toggled) currentsets) }

                        else
                            { current | sets = Sets (toggled :: currentsets) }

                    else if List.member toggled allPromos then
                        if List.member toggled currentpromos then
                            { current | promos = Promos (List.filter ((/=) toggled) currentpromos) }

                        else
                            { current | promos = Promos (toggled :: currentpromos) }

                    else
                        current
            in
            case model of
                GetSets (Success all) ->
                    ( Choosing all (newSets all.sets all.promos { sets = Sets [], promos = Promos [] }), Cmd.none )

                GetSets status ->
                    ( GetSets status, Cmd.none )

                Choosing all current ->
                    ( Choosing all (newSets all.sets all.promos current), Cmd.none )

                GetCards all current status ->
                    ( GetCards all (newSets all.sets all.promos current) status, Cmd.none )

        GotCards result ->
            case result of
                Ok (Cards cardlist) ->
                    case model of
                        GetSets status ->
                            ( GetSets status, Cmd.none )

                        Choosing all current ->
                            ( GetCards all current Loading, randomiser 10 cardlist |> Random.map Cards |> Random.generate Randomised )

                        GetCards all current _ ->
                            ( GetCards all current Loading, randomiser 10 cardlist |> Random.map Cards |> Random.generate Randomised )

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
            setChoice sets { sets = Sets [], promos = Promos [] }

        Choosing sets chosen ->
            setChoice sets chosen

        GetCards sets chosen status ->
            div [] [ setChoice sets chosen, viewCards sets status ]


setChoice : SetsToChoose -> SetsToChoose -> Html Msg
setChoice all chosen =
    let
        allSets =
            case all.sets of
                Sets s ->
                    s

        allPromos =
            case all.promos of
                Promos p ->
                    p

        chosenSets =
            case chosen.sets of
                Sets s ->
                    s

        chosenPromos =
            case chosen.promos of
                Promos p ->
                    p
    in
    (allSets ++ allPromos)
        |> map (\set -> div [] [ label [] [ text set ], input [ type_ "checkbox", onClick (Toggle set), checked (List.member set chosenSets || List.member set chosenPromos) ] [] ])
        |> (\html -> html ++ [ button [ chosen |> Generate |> onClick ] [ text "generate cards" ] ])
        |> div []


viewCards : SetsToChoose -> ApiStatus Cards -> Html Msg
viewCards sets status =
    case status of
        Failure ->
            div []
                [ text "There was a problem getting cards, please try again." ]

        Loading ->
            text "Loading cards..."

        Success (Cards cards) ->
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


getCards : SetsToChoose -> Cmd Msg
getCards { sets, promos } =
    let
        actualsets =
            case sets of
                Sets s ->
                    s

        actualpromos =
            case promos of
                Promos p ->
                    p

        url =
            -- horrible cheat to make sure we get an empty list back. There should be no need to use
            -- an actual HTTP request here but not sure how to get out of it, there doesn't seem to
            -- be a way to make a "constant command"
            if List.isEmpty actualsets then
                "http://dominion.zigmond.uk/cards?max-coin-cost=-1"

            else
                "http://dominion.zigmond.uk/cards?is-kingdom&set=" ++ String.join "&set=" actualsets
    in
    Http.get
        { url = url
        , expect = Http.expectJson (Result.map (\(Cards cards) -> Cards (cards ++ actualpromos)) >> GotCards) cardDecoder
        }


getPromos : Cmd Msg
getPromos =
    Http.get
        { url = "http://dominion.zigmond.uk/cards?is-kingdom&set=promo"
        , expect = Http.expectJson GotPromos promosDecoder
        }


setsDecoder : Decoder Sets
setsDecoder =
    list string |> Json.Decode.map Sets


cardDecoder : Decoder Cards
cardDecoder =
    list (field "name" string) |> Json.Decode.map Cards


promosDecoder : Decoder Promos
promosDecoder =
    list (field "name" string) |> Json.Decode.map Promos



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



-- NEXT THINGS TO DO:
-- - separate promo cards into individual ones (done, but should separate out into a sublist)
-- - have a nicer interface for ase and intrigue (at the very least, get rid of the "bare" sets)
-- - put logic in so that *kingdom piles" come up, rather than individual cards (knights, castles, split piles)
-- - add a "select all" button
-- SLIGHTLY FURTHER OFF:
-- - add "horizontal cards" (Events etc), with an option to customise a rule for them
-- - add logic for any individual cards:
-- -- (eg Young Witch needs an 11th card, and a way to visually identify it as the Bane)
-- -- Black Market deck (need options for how many cards, or whether to include all!)
-- - other custom logic for additional cards/materials needed
-- - logic for deciding whether to use Platinum/Colony, or Shelters
-- - add facility to ban individual cards
-- - other options like no attacks, no attacks without moat, at least one village etc
-- - option to order cards by name or price?
-- - (eventually) show all needed cards (including Basic cards) in a nice visual layout!
-- - also implement "caching" of the API response, only requesting again when the sets selected have changed
