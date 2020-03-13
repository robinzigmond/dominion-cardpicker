module Styles exposing
    ( button
    , cardImage
    , cardsDiv
    , gapAbove
    , gapBelow
    , globalStyles
    , landscapeCardImage
    , largeCheckbox
    , mainDiv
    , pointing
    , selectDiv
    )

import Css exposing (..)
import Css.Global exposing (body, global)
import Html.Styled exposing (Attribute, Html, div, img, input, styled)
import Html.Styled.Attributes exposing (type_)


globalStyles : Html msg
globalStyles =
    [ body
        [ margin2 (px 10) (pct 2)
        , backgroundColor (rgb 190 180 86)
        , color (rgb 24 25 51)
        , fontSize (px 18)
        ]
    ]
        |> global


button : List (Attribute msg) -> List (Html msg) -> Html msg
button =
    styled Html.Styled.button
        [ backgroundColor (rgb 15 30 188)
        , padding2 (px 10) (px 20)
        , borderRadius (px 30)
        , borderStyle none
        , border3 (px 1) solid (rgb 250 210 210)
        , color (rgb 250 210 210)
        , cursor pointer
        , margin2 (px 0) (px 5)
        , fontSize (em 1)
        ]


cardImage : List (Attribute msg) -> List (Html msg) -> Html msg
cardImage =
    styled img
        [ width (px 150)
        ]


landscapeCardImage : List (Attribute msg) -> List (Html msg) -> Html msg
landscapeCardImage =
    styled img
        [ width (px 350)
        ]


mainDiv : List (Attribute msg) -> List (Html msg) -> Html msg
mainDiv =
    styled div [ displayFlex, flexWrap wrap ]


selectDiv : List (Attribute msg) -> List (Html msg) -> Html msg
selectDiv =
    styled div [ flex3 (int 1) (int 0) (px 300) ]


cardsDiv : List (Attribute msg) -> List (Html msg) -> Html msg
cardsDiv =
    styled div [ flex3 (int 3) (int 1) (px 500) ]


gapAbove :
    (List (Attribute msg) -> List (Html msg) -> Html msg)
    -> List (Attribute msg)
    -> List (Html msg)
    -> Html msg
gapAbove element =
    styled element [ marginTop (px 15) ]


gapBelow :
    (List (Attribute msg) -> List (Html msg) -> Html msg)
    -> List (Attribute msg)
    -> List (Html msg)
    -> Html msg
gapBelow element =
    styled element [ marginBottom (em 1.1) ]


pointing :
    (List (Attribute msg) -> List (Html msg) -> Html msg)
    -> List (Attribute msg)
    -> List (Html msg)
    -> Html msg
pointing element =
    styled element [ cursor pointer ]


largeCheckbox : List (Attribute msg) -> Html msg
largeCheckbox attrs =
    styled input [ width (px 22), height (px 22) ] (type_ "checkbox" :: attrs) []
