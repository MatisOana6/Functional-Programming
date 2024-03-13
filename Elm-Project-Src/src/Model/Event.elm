module Model.Event exposing (..)

import Html exposing (..)
import Html.Attributes exposing (class, classList)
import Model.Event.Category exposing (EventCategory(..))
import Model.Interval as Interval exposing (Interval)
import Html.Attributes exposing (href)


type alias Event =
    { title : String
    , interval : Interval
    , description : Html Never
    , category : EventCategory
    , url : Maybe String
    , tags : List String
    , important : Bool
    }


categoryView : EventCategory -> Html Never
categoryView category =
    case category of
        Academic ->
            text "Academic"

        Work ->
            text "Work"

        Project ->
            text "Project"

        Award ->
            text "Award"


customCompare : Event -> Event -> Order
customCompare a b =
    Interval.compare a.interval b.interval

sortByInterval : List Event -> List Event
sortByInterval events =
    -- events
    --Debug.todo "Implement Event.sortByInterval"
    events
    |> List.sortWith (\ev1 -> \ev2 -> Interval.compare ev1.interval ev2.interval)

eventClass : Bool -> String
eventClass important =
    if important then "event event-important" else "event"

view : Event -> Html Never
view event =
    -- div [] []
    --Debug.todo "Implement the Model.Event.view function"
     div [ class <| eventClass event.important ] [
        h2 [ class "event-title" ] [ text event.title ]
    ,   p [ class "event-category" ] [ categoryView event.category ]
    ,   p [ class "event-description" ] [ event.description ]
    ,   p [ class "event-interval" ] [ Interval.view event.interval ] 
    ,   p [ class "event-url" ]
            [ case event.url of
                Just url -> a [ class "event-url", href url ] [ text url ]
                Nothing -> text ""
            ]
    ]
    
