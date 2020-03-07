module CardUtils exposing (getKingdomSets, isLandscape)

import Set
import Types exposing (Card)


landscapes : List String
landscapes =
    [ "event", "landmark", "project" ]


getKingdomSets : List Card -> List String
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
            if List.any ((==) "dame-molly") group then
                "knights"

            else if List.any ((==) "small-castle") group then
                "castles"

            else if
                List.length group == 2
                -- split piles, need to sort by cost but leave for now
            then
                String.join "/" group

            else if List.length group == 1 then
                case group of
                    name :: _ ->
                        name

                    [] ->
                        "should-never-happen"

            else
                String.join "&" group

        combinedCost cardName =
            let
                card =
                    case cardWithName listOfCards cardName of
                        Just c ->
                            c

                        Nothing ->
                            { name = "i-dont-exist"
                            , isKingdom = False
                            , linkedCards = []
                            , coinCost = 0
                            , potionCost = False
                            , debtCost = 0
                            , types = []
                            }
            in
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
        |> List.map
            (List.map .name
                >> Set.fromList
                >> Set.toList
                >> List.sortBy combinedCost
                >> friendlyName
            )


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
