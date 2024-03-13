module Main exposing (..)

import Browser
import Html exposing (..)
import Html.Attributes exposing (style)
import Html.Events exposing (..)
import Http
import Json.Decode as De
import Model exposing (..)
import Model.Event as Event
import Model.Event.Category as EventCategory
import Model.PersonalDetails as PersonalDetails
import Model.Repo as Repo
import Html.Attributes exposing (selected)
import Model.Event.Category exposing (SelectedEventCategories)


type Msg
    = GetRepos
    | GotRepos (Result Http.Error (List Repo.Repo))
    | SelectEventCategory EventCategory.EventCategory
    | DeselectEventCategory EventCategory.EventCategory


main : Program () Model Msg
main =
    Browser.element
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        }


init : () -> ( Model, Cmd Msg )
init _ =
    ( initModel
    , getRepository
    )


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none


getRepository : Cmd Msg
getRepository = Http.get {
    url = "https://api.github.com/MatisOana6?tab=repositories"
 ,  expect = Http.expectJson GotRepos (De.list Repo.decodeRepo)
   }

update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        GetRepos ->
            ( model, getRepository )

        GotRepos res ->
            case res of 
            Ok repos -> ( { model | repos = repos }, Cmd.none)
            Err _ -> ( model, Cmd.none )

        SelectEventCategory category ->
            ( { model | selectedEventCategories = EventCategory.set category True model.selectedEventCategories }, Cmd.none )  

        DeselectEventCategory category ->
            ( { model | selectedEventCategories = EventCategory.set category False model.selectedEventCategories }, Cmd.none )


eventCategoryToMsg : ( EventCategory.EventCategory, Bool ) -> Msg
eventCategoryToMsg ( event, selected ) =
    if selected then
        SelectEventCategory event

    else
        DeselectEventCategory event


view : Model -> Html Msg
view model =
    let
        eventCategoriesView =
            EventCategory.view model.selectedEventCategories |> Html.map eventCategoryToMsg

        eventsView =
            model.events
                |> List.filter (.category >> (\cat -> EventCategory.isEventCategorySelected cat model.selectedEventCategories))
                |> List.map Event.view
                |> div []
                |> Html.map never

        reposView =
            model.repos
                |> Repo.sortByStars
                |> List.take 5
                |> List.map Repo.view
                |> div []
    in
    div []
        [ PersonalDetails.view model.personalDetails
        , h2 [] [ text "Experience" ]
        , eventCategoriesView
        , eventsView
        , h2 [] [ text "My top repos" ]
        , reposView
        ]
