module App exposing (..)

import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (onClick, onInput, on, keyCode)
import Json.Decode as Json


-- MODEL


type alias Todo =
    { completed : Bool
    , text : String
    , id : Int
    }


type alias Model =
    { todos : List Todo
    , input : String
    , uid : Int
    , filter : String
    }


initialModel : Model
initialModel =
    { todos = []
    , input = ""
    , uid = 1
    , filter = "All"
    }



-- UPDATE


onKeyDown : (Int -> msg) -> Attribute msg
onKeyDown tagger =
    on "keydown" (Json.map tagger keyCode)


type Msg
    = ToggleItem Int
    | UpdateInput String
    | KeyDown Int
    | DeleteItem Int
    | ToggleFilter String
    | ClearCompleted


update : Msg -> Model -> Model
update msg model =
    case msg of
        ToggleItem id ->
            let
                toggleCompleted item =
                    if item.id == id then
                        { item | completed = (not item.completed) }
                    else
                        item
            in
                { model | todos = List.map toggleCompleted model.todos }

        KeyDown key ->
            if key == 13 && model.input /= "" then
                { model
                    | input = ""
                    , todos =
                        List.append model.todos
                            [ { id = model.uid
                              , text = model.input
                              , completed = False
                              }
                            ]
                    , uid = model.uid + 1
                }
            else
                model

        UpdateInput newInput ->
            { model | input = newInput }

        DeleteItem id ->
            let
                filterItem item =
                    item.id /= id
            in
                { model | todos = List.filter filterItem model.todos }

        ToggleFilter label ->
            { model | filter = label }

        ClearCompleted ->
            let
                activeTodos =
                    List.filter (\todo -> not todo.completed) model.todos
            in
                { model | todos = activeTodos }



-- HELPERS


remaining : List Todo -> Int
remaining todos =
    todos
        |> List.filter (\todo -> not todo.completed)
        |> List.length


remainingText : List Todo -> String
remainingText todos =
    toString <| remaining todos


completed : List Todo -> Int
completed todos =
    todos
        |> List.filter (\todo -> todo.completed)
        |> List.length


completedText : List Todo -> String
completedText todos =
    toString <| completed todos



-- VIEW


viewInput : Model -> Html Msg
viewInput model =
    div []
        [ input
            [ class "input"
            , placeholder "What needs to be done?"
            , onKeyDown KeyDown
            , onInput UpdateInput
            , value model.input
            ]
            []
        ]


viewTodoItem : Todo -> Html Msg
viewTodoItem todo =
    let
        liClass =
            if todo.completed then
                "item item__checked"
            else
                "item"
    in
        li [ class liClass ]
            [ div [ class "item_content" ]
                [ input
                    [ type_ "checkbox"
                    , class "checkbox"
                    , checked todo.completed
                    , onClick (ToggleItem todo.id)
                    , id (toString todo.id)
                    ]
                    []
                , label [ for (toString todo.id) ] [ text todo.text ]
                ]
            , div
                [ class "item_delete"
                , onClick (DeleteItem todo.id)
                ]
                [ text "x" ]
            ]


viewTodosList : Model -> Html Msg
viewTodosList model =
    let
        filteredTodos =
            case model.filter of
                "Active" ->
                    List.filter (\todo -> not todo.completed) model.todos

                "Completed" ->
                    List.filter (\todo -> todo.completed) model.todos

                _ ->
                    model.todos

        listOfTodos =
            List.map viewTodoItem filteredTodos
    in
        if List.length listOfTodos > 0 then
            ul [ class "todo_list" ] listOfTodos
        else
            ul [ class "empty_todo_list" ] []


viewFiltersButton : String -> String -> Html Msg
viewFiltersButton label activeFilter =
    let
        filterClass =
            if label == activeFilter then
                "filter filter__active"
            else
                "filter"
    in
        button
            [ class filterClass
            , onClick (ToggleFilter label)
            ]
            [ text label ]


viewFilters : Model -> Html Msg
viewFilters model =
    div [ class "footer_filters" ]
        [ viewFiltersButton "All" model.filter
        , viewFiltersButton "Active" model.filter
        , viewFiltersButton "Completed" model.filter
        ]


viewClearCompleted : Model -> Html Msg
viewClearCompleted model =
    if completed model.todos > 0 then
        div
            [ onClick ClearCompleted
            , class "footer_clear"
            ]
            [ text "Clear completed("
            , text (completedText model.todos)
            , text ")"
            ]
    else
        div [] []


viewRemaining : Model -> Html Msg
viewRemaining model =
    let
        numberRemaining =
            remainingText model.todos
    in
        footer [ class "footer" ]
            [ div [ class "footer_content" ]
                [ div
                    [ class "footer_remaining" ]
                    [ text numberRemaining, text " items left" ]
                , viewFilters model
                , viewClearCompleted model
                ]
            ]


viewFooter : Model -> Html Msg
viewFooter model =
    if List.length model.todos > 0 then
        viewRemaining model
    else
        div [] []


view : Model -> Html Msg
view model =
    div [ class "page" ]
        [ div [ class "content" ]
            [ viewInput model
            , viewTodosList model
            , viewFooter model
            ]
        ]


main : Program Never Model Msg
main =
    Html.beginnerProgram
        { model = initialModel
        , view = view
        , update = update
        }
