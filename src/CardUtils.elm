module CardUtils exposing (getKingdomSets, isLandscape, toPile)

import List.Extra exposing (uniqueBy)
import Types exposing (Card, PileToShow)


landscapes : List String
landscapes =
    [ "event", "landmark", "project" ]


getKingdomSets : List Card -> List PileToShow
getKingdomSets listOfCards =
    let
        go cardlist groups =
            case cardlist of
                [] ->
                    groups

                card :: cards ->
                    case
                        List.filter
                            (\g ->
                                List.any (\l -> List.member l g)
                                    (card :: linkedKingdoms card cardlist)
                            )
                            groups
                    of
                        [] ->
                            go cards ((card :: linkedKingdoms card cardlist) :: groups)

                        g :: _ ->
                            go cards <|
                                List.map
                                    (\gr ->
                                        if gr == g then
                                            card :: gr

                                        else
                                            gr
                                    )
                                    groups

        friendlyName group =
            if List.any (\card -> card.name == "dame-molly") group then
                "knights"

            else if List.any (\card -> card.name == "small-castle") group then
                "castles"

            else if List.length group == 2 then
                group |> List.map .name |> String.join "/"

            else if List.length group == 1 then
                case group of
                    card :: _ ->
                        card.name

                    [] ->
                        "should-never-happen"

            else
                group |> List.map .name |> String.join "&"

        toPileFromGroup group =
            group
                |> List.sortBy combinedCost
                |> (\cards ->
                        case cards of
                            cheapest :: _ ->
                                let
                                    cheapestAsPile =
                                        toPile cheapest
                                in
                                { cheapestAsPile | name = friendlyName cards }

                            [] ->
                                { name = "strange empty group"
                                , coinCost = 0
                                , potionCost = False
                                , debtCost = 0
                                }
                   )

        combinedCost card =
            100
                * card.coinCost
                + 10
                * card.debtCost
                + (if card.potionCost then
                    1

                   else
                    0
                  )
    in
    go listOfCards []
        |> List.map (uniqueBy .name)
        |> List.map toPileFromGroup


linkedKingdoms : Card -> List Card -> List Card
linkedKingdoms card all =
    card.linkedCards
        |> List.map (cardWithName all)
        |> List.concatMap
            (\maybeCard ->
                case maybeCard of
                    Nothing ->
                        []

                    Just c ->
                        [ c ]
            )


cardWithName : List Card -> String -> Maybe Card
cardWithName cards name =
    cards
        |> List.filter (\c -> c.name == name && c.isKingdom)
        |> List.head


isLandscape : Card -> Bool
isLandscape card =
    case card.types of
        [ type_ ] ->
            List.member type_ landscapes

        _ ->
            False


toPile : Card -> PileToShow
toPile card =
    { name = card.name
    , coinCost = card.coinCost
    , potionCost = card.potionCost
    , debtCost = card.debtCost
    }
