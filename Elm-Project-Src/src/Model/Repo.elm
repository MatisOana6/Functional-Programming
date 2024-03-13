module Model.Repo exposing (..)

import Html exposing (..)
import Html.Attributes exposing (class, href)
import Json.Decode as De


type alias Repo =
    { name : String
    , description : Maybe String
    , url : String
    , pushedAt : String
    , stars : Int
    }


view : Repo -> Html msg
view repo =
     div [ class "repo" ] [
        div [ class "repo-name"]
              [ a [ href  repo.url ] [ text repo.name ]   
         ]
    ,    p [ class "repo-description" ] [ text (Maybe.withDefault "" repo.description) ]
    ,    p [ class "repo-url" ] [ a [ href repo.url ] [ text "Repository Url" ] ]
    ,    p [ class "repo-stars" ] [ text <| "Stars: " ++ String.fromInt repo.stars ]        
     ]
   -- Debug.todo "Implement Model.Repo.view"


sortByStars : List Repo -> List Repo
sortByStars repos =
    -- []
   -- Debug.todo "Implement Model.Repo.sortByStars"
   repos 
        |> List.sortBy .stars
        |> List.reverse


{-| Deserializes a JSON object to a `Repo`.
Field mapping (JSON -> Elm):

  - name -> name
  - description -> description
  - html\_url -> url
  - pushed\_at -> pushedAt
  - stargazers\_count -> stars

-}
decodeRepo : De.Decoder Repo
decodeRepo =
    -- De.fail "Not implemented"
    De.map5 Repo
        (De.field "name" De.string)
        (De.maybe (De.field "description" De.string))
        (De.field "html_url" De.string)
        (De.field "pushed_at" De.string)
        (De.field "stargazers_count" De.int)
-- Debug.todo "Implement Model.Repo.decodeRepo"
