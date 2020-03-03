module CardUtils exposing (getKingdomSets)

import Set
import Types exposing (Card)


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
                                    (card.name :: linkedKingdoms card cardlist)
                            )
                            groups
                    of
                        [] ->
                            go cards ((card.name :: linkedKingdoms card cardlist) :: groups)

                        g :: _ ->
                            go cards <|
                                List.map
                                    (\gr ->
                                        if gr == g then
                                            card.name :: gr

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
    in
    go listOfCards [] |> List.map (Set.fromList >> Set.toList >> friendlyName)


linkedKingdoms : Card -> List Card -> List String
linkedKingdoms card all =
    List.filter
        (\name ->
            List.filter (\c -> c.name == name && c.isKingdom) all
                |> List.isEmpty
                |> not
        )
        card.linkedCards
