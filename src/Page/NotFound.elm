module Page.NotFound exposing (..)

import Html exposing (..)
import Html.Attributes exposing (..)


view : { title : String, content : Html msg }
view =
    { title = "Not Found"
    , content = div [ class "center-text-page" ] [ text "Oops, page not found" ]
    }
