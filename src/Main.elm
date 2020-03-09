module Main exposing (main)

import Browser
import CardUtils exposing (getKingdomSets, isLandscape)
import Html.Styled exposing (Attribute, Html, div, h2, input, label, strong, text, toUnstyled)
import Html.Styled.Attributes exposing (alt, checked, for, id, src, type_)
import Html.Styled.Events exposing (onClick)
import Http
import Json.Decode exposing (Decoder, bool, field, int, list, null, oneOf, string)
import List exposing (map)
import Random
import Randomisers exposing (combineRandoms, filteredRandom, randomiser)
import Styles exposing (button, cardImage, cardsDiv, gapBelow, landscapeCardImage, mainDiv, selectDiv)
import Types exposing (Cards(..), Promos(..), Sets(..), SetsToChoose)



-- MAIN


main : Program () Model Msg
main =
    Browser.element
        { init = init
        , update = update
        , subscriptions = subscriptions
        , view = view >> toUnstyled
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
    | ChosenCards SetsToChoose SetsToChoose (List String)


init : () -> ( Model, Cmd Msg )
init _ =
    ( GetSets Loading, getSets )



-- UPDATE


type Msg
    = Load
    | GotSets (Result Http.Error Sets)
    | GotPromos (Result Http.Error Promos)
    | Toggle String
    | SelectAll
    | DeselectAll
    | Generate SetsToChoose
    | GotCards (Result Http.Error Cards)
    | Randomised (List String)


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
                            ( GetSets <|
                                Success <|
                                    { sets = Sets (List.filter ((/=) "promo") allsets)
                                    , promos = promos
                                    }
                            , Cmd.none
                            )

                        GetSets status ->
                            ( GetSets status, Cmd.none )

                        Choosing all current ->
                            ( Choosing all current, Cmd.none )

                        GetCards all current status ->
                            ( GetCards all current status, Cmd.none )

                        ChosenCards all current chosen ->
                            ( ChosenCards all current chosen, Cmd.none )

                Err _ ->
                    ( GetSets Failure, Cmd.none )

        Toggle toggled ->
            let
                makeCard all nameToFind =
                    List.filter (.name >> (==) nameToFind) all

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

                    else if List.member toggled (map .name allPromos) then
                        if List.member toggled (map .name currentpromos) then
                            { current
                                | promos =
                                    Promos
                                        (List.filter (.name >> (/=) toggled) currentpromos)
                            }

                        else
                            { current | promos = Promos (makeCard allPromos toggled ++ currentpromos) }
                        -- deal with case of split pile or similar (only Avanto/Sauna at the moment)

                    else
                        let
                            possibleSplitPromos =
                                allPromos
                                    |> map .name
                                    |> List.filter (\name -> List.member name (String.split "/" toggled))

                            splitSelected =
                                currentpromos
                                    |> map .name
                                    |> List.filter (\name -> List.member name (String.split "/" toggled))
                        in
                        if List.length possibleSplitPromos > 0 then
                            if List.length splitSelected == 0 then
                                { current
                                    | promos =
                                        Promos
                                            (currentpromos
                                                ++ List.concatMap (makeCard allPromos) possibleSplitPromos
                                            )
                                }

                            else
                                { current
                                    | promos =
                                        Promos
                                            (List.filter
                                                (\p ->
                                                    possibleSplitPromos
                                                        |> List.member p.name
                                                        |> not
                                                )
                                                currentpromos
                                            )
                                }

                        else
                            current
            in
            case model of
                GetSets (Success all) ->
                    ( Choosing all
                        (newSets all.sets
                            all.promos
                            { sets = Sets []
                            , promos = Promos []
                            }
                        )
                    , Cmd.none
                    )

                GetSets status ->
                    ( GetSets status, Cmd.none )

                Choosing all current ->
                    ( Choosing all (newSets all.sets all.promos current), Cmd.none )

                GetCards all current status ->
                    ( GetCards all (newSets all.sets all.promos current) status, Cmd.none )

                ChosenCards all current picked ->
                    ( ChosenCards all (newSets all.sets all.promos current) picked, Cmd.none )

        SelectAll ->
            case model of
                GetSets (Success all) ->
                    ( Choosing all all, Cmd.none )

                GetSets status ->
                    ( GetSets status, Cmd.none )

                Choosing all _ ->
                    ( Choosing all all, Cmd.none )

                GetCards all _ status ->
                    ( GetCards all all status, Cmd.none )

                ChosenCards all _ picked ->
                    ( ChosenCards all all picked, Cmd.none )

        DeselectAll ->
            let
                noneSelected =
                    { sets = Sets [], promos = Promos [] }
            in
            case model of
                GetSets (Success all) ->
                    ( Choosing all noneSelected, Cmd.none )

                GetSets status ->
                    ( GetSets status, Cmd.none )

                Choosing all _ ->
                    ( Choosing all noneSelected, Cmd.none )

                GetCards all _ status ->
                    ( GetCards all noneSelected status, Cmd.none )

                ChosenCards all _ picked ->
                    ( ChosenCards all noneSelected picked, Cmd.none )

        GotCards result ->
            case result of
                Ok (Cards cardlist) ->
                    case model of
                        GetSets status ->
                            ( GetSets status, Cmd.none )

                        Choosing all current ->
                            ( GetCards all current Loading
                            , cardlist
                                |> (\cards ->
                                        combineRandoms
                                            (cards
                                                |> List.filter .isKingdom
                                                |> getKingdomSets
                                                |> randomiser 10
                                            )
                                            (filteredRandom isLandscape 2 cards |> Random.map (map .name))
                                   )
                                |> Random.generate Randomised
                            )

                        GetCards all current _ ->
                            ( GetCards all current Loading
                            , cardlist
                                |> (\cards ->
                                        combineRandoms
                                            (cards
                                                |> List.filter .isKingdom
                                                |> getKingdomSets
                                                |> randomiser 10
                                            )
                                            (filteredRandom isLandscape 2 cards |> Random.map (map .name))
                                   )
                                |> Random.generate Randomised
                            )

                        ChosenCards all current chosen ->
                            ( ChosenCards all current chosen, Cmd.none )

                Err _ ->
                    case model of
                        GetSets status ->
                            ( GetSets status, Cmd.none )

                        Choosing all current ->
                            ( Choosing all current, Cmd.none )

                        GetCards all current _ ->
                            ( GetCards all current Failure, Cmd.none )

                        ChosenCards all current chosen ->
                            ( ChosenCards all current chosen, Cmd.none )

        Generate chosen ->
            case model of
                GetSets status ->
                    ( GetSets status, Cmd.none )

                Choosing all current ->
                    ( GetCards all current Loading, getCards chosen )

                GetCards all current _ ->
                    ( GetCards all current Loading, getCards chosen )

                ChosenCards all current _ ->
                    ( GetCards all current Loading, getCards chosen )

        Randomised chosen ->
            case model of
                GetCards all current _ ->
                    ( ChosenCards all current chosen, Cmd.none )

                _ ->
                    ( model, Cmd.none )



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
            mainDiv [] [ setChoice sets { sets = Sets [], promos = Promos [] }, cardsDiv [] [] ]

        Choosing sets chosen ->
            mainDiv [] [ setChoice sets chosen, cardsDiv [] [] ]

        GetCards sets chosen status ->
            mainDiv [] [ setChoice sets chosen, viewCards status ]

        ChosenCards sets chosen randomised ->
            mainDiv [] [ setChoice sets chosen, viewCardImages randomised ]


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

        renderAsCheckbox set niceName =
            let
                theDiv =
                    if niceName == "Second Edition" then
                        gapBelow div

                    else
                        div
            in
            theDiv []
                [ label [ for set ] [ text niceName ]
                , input
                    [ type_ "checkbox"
                    , id set
                    , onClick (Toggle set)
                    , checked
                        (List.member set chosenSets
                            || List.any
                                (\name -> List.member name (map .name chosenPromos))
                                (String.split "/" set)
                        )
                    ]
                    []
                ]

        renderInList set =
            case set of
                "base" ->
                    strong [] [ text "Base set" ]

                "base-first-ed" ->
                    renderAsCheckbox "base-first-ed" "First Edition"

                "base-second-ed" ->
                    renderAsCheckbox "base-second-ed" "Second Edition"

                "intrigue" ->
                    strong [] [ text "Intrigue" ]

                "intrigue-first-ed" ->
                    renderAsCheckbox "intrigue-first-ed" "First Edition"

                "intrigue-second-ed" ->
                    renderAsCheckbox "intrigue-second-ed" "Second Edition"

                name ->
                    renderAsCheckbox name (transformName name)

        transformName =
            String.split "-" >> map titleCase >> String.join " "
            >> String.split "/" >> map titleCase >> String.join "/"
    in
    selectDiv []
        [ gapBelow div
            []
            [ button [ onClick SelectAll ] [ text "Select All" ]
            , button [ onClick DeselectAll ] [ text "Deselect All " ]
            ]
        , gapBelow div [] (map renderInList allSets)
        , strong [] [ text "promo cards:" ]
        , (getKingdomSets >> map renderInList >> gapBelow div []) allPromos
        , button
            [ chosen
                |> Generate
                |> onClick
            ]
            [ text "generate cards" ]
        ]


viewCards : ApiStatus Cards -> Html Msg
viewCards status =
    case status of
        Failure ->
            cardsDiv []
                [ text "There was a problem getting cards, please try again." ]

        Loading ->
            cardsDiv [] [ text "Loading cards..." ]

        Success _ ->
            cardsDiv [] [ text "Loading cards..." ]


viewCardImages : List String -> Html Msg
viewCardImages names =
    (List.take 10 names
        |> map (getCardImage cardImage)
    )
        ++ (List.drop 10 names
                |> map (getCardImage landscapeCardImage)
           )
        |> cardsDiv []


getCardImage : (List (Attribute msg) -> List (Html msg) -> Html msg) -> String -> Html msg
getCardImage imgStyle name =
    let
        transformedName =
            "%PUBLIC_URL%/card-images/"
                ++ (name
                        |> String.split "-"
                        |> map titleCase
                        |> String.join "_"
                        |> String.split "/"
                        |> map titleCase
                        |> String.join "_"
                   )
                ++ ".jpg"
    in
    imgStyle [ src transformedName, alt name ] []


titleCase : String -> String
titleCase str =
    case String.uncons str of
        Nothing ->
            ""

        Just ( first, rest ) ->
            String.cons (Char.toUpper first) rest



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
                "http://dominion.zigmond.uk/cards?set="
                    ++ String.join "&set=" actualsets
    in
    Http.get
        { url = url
        , expect =
            Http.expectJson
                (Result.map (\(Cards cards) -> Cards (cards ++ actualpromos))
                    >> GotCards
                )
                cardDecoder
        }


getPromos : Cmd Msg
getPromos =
    Http.get
        { url = "http://dominion.zigmond.uk/cards?set=promo"
        , expect = Http.expectJson GotPromos promosDecoder
        }


setsDecoder : Decoder Sets
setsDecoder =
    list string |> Json.Decode.map Sets


cardDecoder : Decoder Cards
cardDecoder =
    list
        (Json.Decode.map7
            (\name isKingdom linked coinCost potionCost debtCost types ->
                { name = name
                , isKingdom = isKingdom
                , linkedCards = linked
                , coinCost = coinCost
                , potionCost = potionCost
                , debtCost = debtCost
                , types = types
                }
            )
            (field "name" string)
            (field "is-kingdom" bool)
            (field "linked-cards" (list string))
            (field "coin-cost" <| oneOf [ int, null 0 ])
            (field "potion-cost" <| oneOf [ bool, null False ])
            (field "debt-cost" <| oneOf [ int, null 0 ])
            (field "types" (list string))
        )
        |> Json.Decode.map Cards


promosDecoder : Decoder Promos
promosDecoder =
    list
        (Json.Decode.map7
            (\name isKingdom linked coinCost potionCost debtCost types ->
                { name = name
                , isKingdom = isKingdom
                , linkedCards = linked
                , coinCost = coinCost
                , potionCost = potionCost
                , debtCost = debtCost
                , types = types
                }
            )
            (field "name" string)
            (field "is-kingdom" bool)
            (field "linked-cards" (list string))
            (field "coin-cost" <| oneOf [ int, null 0 ])
            (field "potion-cost" <| oneOf [ bool, null False ])
            (field "debt-cost" <| oneOf [ int, null 0 ])
            (field "types" (list string))
        )
        |> Json.Decode.map Promos



-- TO DO:
-- - add/improve styling (ongoing)
-- - add logic for any individual cards:
-- -- (eg Young Witch needs an 11th card, and a way to visually identify it as the Bane)
-- -- Black Market deck (need options for how many cards, or whether to include all!)
-- - other custom logic for additional cards/materials needed
-- - logic for deciding whether to use Platinum/Colony, or Shelters
-- - allow users to alter the logic for picking landscape cards (at the moment it's always 2, but allow
-- "full random" with no limit, or with a limit of 2 or some other number, or with limits per type,
-- or a fixed number of each type...)
-- - add facility to ban individual cards
-- - other options like no attacks, no attacks without moat, at least one village etc
-- - option to order cards by name or price?
-- - (eventually) show all needed cards (including Basic cards) in a nice visual layout!
-- -- (At the very least, sort them in cost order)
-- - also implement "caching" of the API response, only requesting again when the sets selected have changed
