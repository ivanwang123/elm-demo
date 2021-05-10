module Page.Blank exposing (..)

import Html exposing (..)
import Html.Attributes exposing (..)


view : { title : String, content : Html msg }
view =
    { title = ""
    , content = text ""
    }
