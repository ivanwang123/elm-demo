module Page exposing (..)

import Browser exposing (Document)
import Html exposing (..)
import Html.Attributes exposing (..)


type Page
    = Other


view : Page -> { title : String, content : Html msg } -> Document msg
view page { title, content } =
    { title = title
    , body = viewHeader page :: content :: [ viewFooter ]
    }


viewHeader : Page -> Html msg
viewHeader page =
    nav []
        [ text "navbar" ]


viewFooter : Html msg
viewFooter =
    footer []
        [ text "footer" ]
