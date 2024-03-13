module Model.Event.Category exposing (EventCategory(..), SelectedEventCategories, allSelected, eventCategories, isEventCategorySelected, set, view)

import Html exposing (Html, div, input, text)
import Html.Attributes exposing (checked, class, style, type_)
import Html.Events exposing (onCheck)
import Html exposing (s)


type EventCategory
    = Academic
    | Work
    | Project
    | Award


eventCategories =
    [ Academic, Work, Project, Award ]


{-| Type used to represent the state of the selected event categories
-}
type SelectedEventCategories = SelectedEventCategories {
        academic : Bool
    ,   work : Bool
    ,   project : Bool
    ,   award : Bool
    }
    --TODOCompleteThisType


{-| Returns an instance of `SelectedEventCategories` with all categories selected

    isEventCategorySelected Academic allSelected --> True

-}
allSelected : SelectedEventCategories
allSelected = SelectedEventCategories {
        academic = True
    ,   work = True
    ,   project = True
    ,   award = True   
   }
    --TODOCompleteThisType
    --Debug.todo "Implement Model.Event.Category.allSelected"

{-| Returns an instance of `SelectedEventCategories` with no categories selected

-- isEventCategorySelected Academic noneSelected --> False

-}
noneSelected : SelectedEventCategories
noneSelected = SelectedEventCategories {
        academic = False
    ,    work = False
    ,    project = False
    ,    award = False 
  }
    -- TODOCompleteThisType
    --Debug.todo "Implement Model.Event.Category.noneSelected"

{-| Given a the current state and a `category` it returns whether the `category` is selected.

    isEventCategorySelected Academic allSelected --> True

-}
isEventCategorySelected : EventCategory -> SelectedEventCategories -> Bool
isEventCategorySelected category current =
    -- False
    case ( category, current ) of
        (Academic, SelectedEventCategories record ) -> record.academic
        (Work, SelectedEventCategories record ) -> record.work
        (Project, SelectedEventCategories record) -> record.project
        (Award, SelectedEventCategories record) -> record.award

    -- Debug.todo "Implement Model.Event.Category.isEventCategorySelected"


{-| Given an `category`, a boolean `value` and the current state, it sets the given `category` in `current` to `value`.

    allSelected |> set Academic False |> isEventCategorySelected Academic --> False

    allSelected |> set Academic False |> isEventCategorySelected Work --> True

-}
set : EventCategory -> Bool -> SelectedEventCategories -> SelectedEventCategories
set category value current =
    --current
    let
        record = case current of SelectedEventCategories s -> s
    in
    case category of
        Academic -> SelectedEventCategories { record | academic = value }
        Work -> SelectedEventCategories { record | work = value }
        Project -> SelectedEventCategories { record | project = value }
        Award -> SelectedEventCategories { record | award = value }
    --Debug.todo "Implement Model.Event.Category.set"


checkbox : String -> Bool -> EventCategory -> Html ( EventCategory, Bool )
checkbox name state category =
    div [ style "display" "inline", class "category-checkbox" ]
        [ input [ type_ "checkbox", onCheck (\c -> ( category, c )), checked state ] []
        , text name
        ]


view : SelectedEventCategories -> Html ( EventCategory, Bool )
view model =
    let
        category = case model of SelectedEventCategories s -> s
    in
    div [] [
         checkbox "Academic" category.academic Academic
    ,    checkbox "Work" category.work Work
    ,    checkbox "Project" category.project Project
    ,    checkbox "Award" category.award Award
    ]
    --Debug.todo "Implement the Model.Event.Category.view function"
