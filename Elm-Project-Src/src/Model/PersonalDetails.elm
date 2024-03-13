module Model.PersonalDetails exposing (..)

import Html exposing (..)
import Html.Attributes exposing (class, href, id)



type alias DetailWithName =
    { name : String
    , detail : String
    }


type alias PersonalDetails =
    { name : String
    , contacts : List DetailWithName
    , intro : String
    , socials : List DetailWithName
    }


contactsView : DetailWithName -> Html msg
contactsView cont = 
    li [ class "contact-detail"] [ text (cont.name ++ ": " ++ cont.detail) ]  


socialsView : DetailWithName -> Html msg
socialsView soc =
    li [] [ a [ href soc.detail, class "contact-detail"] [text soc.name]]


view : PersonalDetails -> Html msg
view details =
    -- div [] []
    --Debug.todo "Implement the Model.PersonalDetails.view function"
    div [] 
         [ h1 [id "name" ] [text details.name] 
         , em [ id "intro" ] [ text details.intro ]
         , ul [ id "contacts" ] (List.map (\x -> contactsView x) details.contacts)
         , ul [ class "social-link" ] (List.map (\x -> socialsView x) details.socials)
         ]