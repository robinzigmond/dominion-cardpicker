module Styles exposing (button, cardImage, cardsDiv, landscapeCardImage, mainDiv, selectDiv)

import Css exposing (..)
import Html.Styled exposing (Attribute, Html, div, img, styled)


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
