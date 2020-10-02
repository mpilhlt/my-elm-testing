port module Main exposing (main)

import Browser
import Browser.Dom
import Browser.Events
import Element
import Element.Background
import Element.Border
import Element.Input
import Html
import Html.Attributes
import Html.Events
import Html.Keyed
import Json.Decode
import List
import String
import Task



-- === Model ===


type alias Model =
    { field1 : Crit
    , field2 : Crit
    , focused : Maybe (FormField, MenuMode)
    }


{-| A Search criterium is composed of a mode (and/or), a
field name that is to be searched and a search phrase
that is to be searched for. We also save various information
related to the criterium's role on a search form
-}
type alias Crit =
    { mode : String
    , fieldName : String
    , searchPhrase : String
    , form :
        { formField : FormField
        , completionsShow : Bool
        , completionsState : MenuState
        , completionsHowMany : Int
        , completionsSelected : Maybe Suggestion
        , completionsOptions : List Suggestion
        , fieldnamesShow : Bool
        , fieldnamesState : MenuState
        , fieldnamesHowMany : Int
        , fieldnamesSelected : Maybe String
        , fieldnamesOptions : List String
        }
    }


{-| Tracks keyboard and mouse selection within the menu.
-}
type alias MenuState =
    { key : Maybe String
    , mouse : Maybe String
    }


{-| Tracks which of the field menus - fieldname or searchPhrase - is active
-}
type MenuMode
    = Completions
    | Fieldnames


{-| Configuration for changes to the menu
-}
type alias MenuUpdateConfig msg data =
    { onKeyDown : Int -> Maybe String -> Maybe msg
    , onMouseClick : String -> Maybe msg
    , onMouseEnter : String -> Maybe msg
    , onMouseLeave : String -> Maybe msg
    , onTooHigh : Maybe msg
    , onTooLow : Maybe msg
    , toId : data -> String
    }


completionsMenuUpdateConfig : FormField -> MenuUpdateConfig Msg Suggestion
completionsMenuUpdateConfig formField =
    { onKeyDown =
        \code maybeId ->
            if code == 38 || code == 40 then
                Maybe.map (\b -> PreviewSuggestion formField Completions b) maybeId

            else if code == 13 then
                Maybe.map (\b -> SelectEntryKeyboard formField Completions b) maybeId

            else
                Just (Reset formField Completions)
    , onMouseClick = \id -> Just (SelectEntryMouse formField Completions id)
    , onMouseEnter = \id -> Just (PreviewSuggestion formField Completions id)
    , onMouseLeave = \_ -> Nothing
    , onTooHigh = Just (CursorWrap formField Completions True)
    , onTooLow = Just (CursorWrap formField Completions False)
    , toId = .phrase
    }


fieldnamesMenuUpdateConfig : FormField -> MenuUpdateConfig Msg String
fieldnamesMenuUpdateConfig formField =
    { onKeyDown =
        \code maybeId ->
            if code == 38 || code == 40 then
                Maybe.map (\b -> PreviewSuggestion formField Fieldnames b) maybeId

            else if code == 13 then
                Maybe.map (\b -> SelectEntryKeyboard formField Fieldnames b) maybeId

            else
                Just (Reset formField Fieldnames)
    , onMouseClick = \id -> Just (SelectEntryMouse formField Fieldnames id)
    , onMouseEnter = \id -> Just (PreviewSuggestion formField Fieldnames id)
    , onMouseLeave = \_ -> Nothing
    , onTooHigh = Just (CursorWrap formField Fieldnames True)
    , onTooLow = Just (CursorWrap formField Fieldnames False)
    , toId = identity
    }


{-| Configuration for drawing the menu
(data is what we will have as a single menu entry)
-}
type alias MenuViewConfig data =
    { toId : data -> String
    , ul : List (Html.Attribute Never)
    , li : KeySelected -> MouseSelected -> data -> HtmlDetails Never
    }


{-| True if the element has been selected via keyboard navigation.
-}
type alias KeySelected =
    Bool


{-| True if the element has been selected via mouse hover.
-}
type alias MouseSelected =
    Bool


type alias HtmlDetails msg =
    { attributes : List (Html.Attribute msg)
    , children : List (Html.Html msg)
    }


completionsMenuViewConfig : MenuViewConfig Suggestion
completionsMenuViewConfig =
    let
        customizedLi keySelected mouseSelected suggestion =
            { attributes =
                [ Html.Attributes.classList
                    [ ( "autocomplete-item", True )
                    , ( "key-selected", keySelected || mouseSelected )
                    ]
                , Html.Attributes.id suggestion.phrase
                , if keySelected || mouseSelected then
                    Html.Attributes.style "font-weight" "bold"
                  else
                    Html.Attributes.style "font-weight" "normal"
{-
                , if keySelected || mouseSelected then
                    Html.Attributes.style "background-color" "black"
                  else
                    Html.Attributes.style "background-color" "white"
                , if keySelected || mouseSelected then
                    Html.Attributes.style "color" "white"
                  else
                    Html.Attributes.style "color" "black"
-}
                ]
            , children = [ Html.text suggestion.phrase ]
            }
    in
    { toId = .phrase
    , ul =
        [ Html.Attributes.class "autocomplete-list"
        , Html.Attributes.style "list-style-type" "none"
        , Html.Attributes.style "margin-left" "-1em"
        , Html.Attributes.style "margin-right" "1em"
        , Html.Attributes.style "margin-top" "0.3em"
        , Html.Attributes.style "margin-bottom" "0.3em"
        ]
    , li = customizedLi
    }


fieldnamesMenuViewConfig : MenuViewConfig String
fieldnamesMenuViewConfig =
    let
        customizedLi keySelected mouseSelected suggestion =
            { attributes =
                [ Html.Attributes.classList
                    [ ( "autocomplete-item", True )
                    , ( "key-selected", keySelected || mouseSelected )
                    ]
                , Html.Attributes.id suggestion
                , if keySelected || mouseSelected then
                    Html.Attributes.style "font-weight" "bold"

                  else
                    Html.Attributes.style "font-weight" "normal"
                ]
            , children = [ Html.text suggestion ]
            }
    in
    { toId = identity
    , ul =
        [ Html.Attributes.class "autocomplete-list"
        , Html.Attributes.style "list-style-type" "none"
        , Html.Attributes.style "margin-left" "-1.5em"
        , Html.Attributes.style "margin-right" "0.5em"
        , Html.Attributes.style "margin-top" "0.2em"
        , Html.Attributes.style "margin-bottom" "0.2em"
        ]
    , li = customizedLi
    }


{-| This is how searchphrase suggestions are listed
-}
type alias Suggestion =
    { phrase : String
    , score : Float
    }

type FormField
    = Field1
    | Field2

{-| A Menu State with nothing selected.
-}
emptyMenuState : MenuState
emptyMenuState =
    { key = Nothing, mouse = Nothing }


{-| Dummy/empty criterion record
-}
emptyCrit : FormField -> Crit
emptyCrit formField =
    { mode = ""
    , fieldName = ""
    , searchPhrase = ""
    , form =
        { formField = formField
        , completionsShow = False
        , completionsState = emptyMenuState
        , completionsHowMany = 10
        , completionsSelected = Nothing
        , completionsOptions = dummySuggestions
        , fieldnamesShow = False
        , fieldnamesState = emptyMenuState
        , fieldnamesHowMany = 10
        , fieldnamesSelected = Nothing
        , fieldnamesOptions = dummyFieldnames
        }
    }


{-| Dummy/empty model
-}
initialModel : Model
initialModel =
    { field1 = emptyCrit Field1
    , field2 = emptyCrit Field2
    , focused = Nothing
    }


{-| Helper functions for setting/accessing model parts
-}
getCritFromSearch : FormField -> Model -> Crit
getCritFromSearch formField model =
    if model.field1.form.formField == formField then model.field1
    else if model.field2.form.formField == formField then model.field2
    else model.field1


setCritToSearch : FormField -> Crit -> Model -> Model
setCritToSearch formField crit model =
    if model.field1.form.formField == formField then
        { model | field1 = crit }
    else if model.field2.form.formField == formField then
        { model | field2 = crit }
    else model



-- === Update ===


type Msg
    = CursorWrap FormField MenuMode Bool
    | FetchFocused
    | Focused (Maybe String)
    | HandleEscape FormField MenuMode
    | NoOp
    | PreviewSuggestion FormField MenuMode String
    | Reset FormField MenuMode
    | SelectEntryKeyboard FormField MenuMode String
    | SelectEntryMouse FormField MenuMode String
    | SetSomeState MenuMsg
    | SetState FormField MenuMode MenuMsg
    | Submit
    | UserTypedText FormField MenuMode String
    | UserSelectedMode FormField String

update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        CursorWrap formField mode toTop ->
            let
                crit =
                    getCritFromSearch formField model

                oldForm =
                    crit.form

                newForm =
                    case mode of
                        Completions ->
                            if toTop then
                                { oldForm
                                    | completionsState =
                                        resetToLastItem
                                            (completionsMenuUpdateConfig formField)
                                            (acceptableSuggestions crit.searchPhrase .phrase oldForm.completionsOptions)
                                            oldForm.completionsHowMany
                                            oldForm.completionsState
                                    , completionsSelected =
                                        acceptableSuggestions crit.searchPhrase .phrase oldForm.completionsOptions
                                            |> List.take oldForm.completionsHowMany
                                            |> List.reverse
                                            |> List.head
                                }

                            else
                                { oldForm
                                    | completionsState =
                                        resetToFirstItem (completionsMenuUpdateConfig formField)
                                            (acceptableSuggestions crit.searchPhrase .phrase oldForm.completionsOptions)
                                            oldForm.completionsHowMany
                                            oldForm.completionsState
                                    , completionsSelected =
                                        acceptableSuggestions crit.searchPhrase .phrase oldForm.completionsOptions
                                            |> List.take oldForm.completionsHowMany
                                            |> List.head
                                }

                        Fieldnames ->
                            if toTop then
                                { oldForm
                                    | fieldnamesState =
                                        resetToLastItem
                                            (fieldnamesMenuUpdateConfig formField)
                                            (acceptableSuggestions crit.fieldName identity oldForm.fieldnamesOptions)
                                            oldForm.fieldnamesHowMany
                                            oldForm.fieldnamesState
                                    , fieldnamesSelected =
                                        acceptableSuggestions crit.fieldName identity oldForm.fieldnamesOptions
                                            |> List.take oldForm.fieldnamesHowMany
                                            |> List.reverse
                                            |> List.head
                                }

                            else
                                { oldForm
                                    | fieldnamesState =
                                        resetToFirstItem (fieldnamesMenuUpdateConfig formField)
                                            (acceptableSuggestions crit.fieldName identity oldForm.fieldnamesOptions)
                                            oldForm.fieldnamesHowMany
                                            oldForm.fieldnamesState
                                    , fieldnamesSelected =
                                        acceptableSuggestions crit.fieldName identity oldForm.fieldnamesOptions
                                            |> List.take oldForm.fieldnamesHowMany
                                            |> List.head
                                }

            in
            case mode of
                Completions ->
                    case crit.form.completionsSelected of
                        Just _ ->
                            update (Reset formField mode) model

                        Nothing ->
                            let
                                newField =
                                    { crit | form = newForm }

                                newModel =
                                    setCritToSearch formField newField model
                            in
                            ( newModel, Cmd.none )

                Fieldnames ->
                    let
                        newField =
                            { crit | form = newForm }

                        newModel =
                            setCritToSearch formField newField model
                    in
                    ( newModel, Cmd.none )

        HandleEscape formField mode ->
            let
                crit =
                    getCritFromSearch formField model

                validOptions =
                    case mode of
                        Completions ->
                            not (List.isEmpty (acceptableSuggestions crit.searchPhrase .phrase crit.form.completionsOptions))
                        Fieldnames ->
                            not (List.isEmpty (acceptableSuggestions crit.fieldName identity crit.form.fieldnamesOptions))

                handleEscape a b =
                    if validOptions then
                        model
                            |> removeMenuSelection a b
                            |> resetMenu a b

                    else
                        resetMenuInput a b model

                escapedModel =
                    case mode of
                        Completions ->
                            case crit.form.completionsSelected of
                                Just s ->
                                    if crit.searchPhrase == s.phrase then
                                        resetMenuInput formField mode model

                                    else
                                        handleEscape formField mode

                                Nothing ->
                                    handleEscape formField mode

                        Fieldnames ->
                            case crit.form.fieldnamesSelected of
                                Just s ->
                                    if crit.fieldName == s then
                                        resetMenuInput formField mode model

                                    else
                                        handleEscape formField mode

                                Nothing ->
                                    handleEscape formField mode

            in
            ( escapedModel, Cmd.none )

        FetchFocused ->
            (model, fetchFocused ())

        Focused id ->
            let
                newModel =
                    case id of
                        Just s ->

                            case s of
                                "Field1_searchPhrase" ->
                                    { model | focused = Just (Field1, Completions) }
                                "Field2_searchPhrase" ->
                                    { model | focused = Just (Field2, Completions) }
                                "Field1_fieldName" ->
                                    { model | focused = Just (Field1, Fieldnames) }
                                "Field2_fieldName" ->
                                    { model | focused = Just (Field2, Fieldnames) }
                                _ ->
                                    { model | focused = Nothing }
                        Nothing ->
                            { model | focused = Nothing }
            in
                (newModel, Cmd.none)

        PreviewSuggestion formField mode id ->
            let
                crit =
                    getCritFromSearch formField model

                oldForm =
                    crit.form

                newForm =
                    case mode of
                        Completions ->
                            { oldForm
                                | completionsSelected =
                                    Just (getSuggestionAtId oldForm.completionsOptions id .phrase (Suggestion "" 0))
                            }
                        Fieldnames ->
                            { oldForm
                                | fieldnamesSelected =
                                    Just (getSuggestionAtId oldForm.fieldnamesOptions id identity "")
                            }

                newField =
                    { crit | form = newForm }

                newModel =
                    setCritToSearch formField newField model
            in
            ( newModel, Cmd.none )

        Reset formField mode ->
            let
                crit =
                    getCritFromSearch formField model

                oldForm =
                    crit.form

                newForm =
                    case mode of
                        Completions ->
                            { oldForm
                                | completionsState = emptyMenuState
                                , completionsSelected = Nothing
                            }
                        Fieldnames ->
                            { oldForm
                                | fieldnamesState = emptyMenuState
                                , fieldnamesSelected = Nothing
                            }

                newField =
                    { crit | form = newForm }

                newModel =
                    setCritToSearch formField newField model
            in
            ( newModel, Cmd.none )

        SelectEntryKeyboard formField mode id ->
            let
                newModel =
                    setQuery id formField mode model
                        |> resetMenu formField mode
            in
            ( newModel, Cmd.none )

        SelectEntryMouse formField mode id ->
            let
                newModel =
                    setQuery id formField mode model
                        |> resetMenu formField mode
            in
            ( newModel, Task.attempt (\_ -> NoOp) (Browser.Dom.focus "quicksearch-input") )

        SetSomeState autoMsg ->
            let

{- This is what I need to detect from autoMsg somehow: -}
                (openMenuField, mode) =
                    case model.focused of
                        Just (a, b) ->
                            (a, b)
                        
                        Nothing ->
                            (Field1, Completions)

{- End of what I need to detect -}

                crit =
                    getCritFromSearch openMenuField model

                oldForm =
                    crit.form

                ( newState, maybeMsg ) =
                    case mode of
                        Completions ->
                            updateMenu
                                (completionsMenuUpdateConfig openMenuField)
                                autoMsg
                                oldForm.completionsHowMany
                                oldForm.completionsState
                                (acceptableSuggestions crit.searchPhrase .phrase oldForm.completionsOptions)

                        Fieldnames ->
                            updateMenu
                                (fieldnamesMenuUpdateConfig openMenuField)
                                autoMsg
                                oldForm.fieldnamesHowMany
                                oldForm.fieldnamesState
                                (acceptableSuggestions crit.fieldName identity oldForm.fieldnamesOptions)

                newForm =
                    case mode of
                        Completions ->
                            { oldForm | completionsState = newState }

                        Fieldnames ->
                            { oldForm | fieldnamesState = newState }

                newField =
                    { crit | form = newForm }

                newModel =
                    setCritToSearch openMenuField newField model

            in
            maybeMsg
                |> Maybe.map (\updateMsg -> update updateMsg newModel)
                |> Maybe.withDefault ( newModel, Cmd.none )

        SetState formField mode autoMsg ->
            let
                crit =
                    getCritFromSearch formField model

                oldForm =
                    crit.form

                ( newState, maybeMsg ) =
                    case mode of
                        Completions ->
                            updateMenu (completionsMenuUpdateConfig formField)
                                autoMsg
                                oldForm.completionsHowMany
                                oldForm.completionsState
                                (acceptableSuggestions crit.searchPhrase .phrase oldForm.completionsOptions)

                        Fieldnames ->
                            updateMenu (fieldnamesMenuUpdateConfig formField)
                                autoMsg
                                oldForm.fieldnamesHowMany
                                oldForm.fieldnamesState
                                (acceptableSuggestions crit.fieldName identity oldForm.fieldnamesOptions)

                newForm =
                    case mode of
                        Completions ->
                            { oldForm | completionsState = newState }
                        Fieldnames ->
                            { oldForm | fieldnamesState = newState }

                newField =
                    { crit | form = newForm }

                newModel =
                    setCritToSearch formField newField model
            in
            maybeMsg
                |> Maybe.map (\updateMsg -> update updateMsg newModel)
                |> Maybe.withDefault ( newModel, Cmd.none )

        UserTypedText formField mode newString ->
            let
                crit =
                    getCritFromSearch formField model

                oldForm =
                    crit.form

                newMode =
                    if String.length newString > 0 then
                        case crit.mode of
                            "" ->
                                "and"

                            _ ->
                                crit.mode

                    else
                        crit.mode

                newShow =
                    case mode of
                        Completions ->
                            if String.length newString > 0 then
                                not (List.isEmpty (acceptableSuggestions newString .phrase oldForm.completionsOptions))
                            else
                                False

                        Fieldnames ->
                            if String.length newString > 0 then
                                not (List.isEmpty (acceptableSuggestions newString identity oldForm.fieldnamesOptions))
                            else
                                False


                newForm =
                    case mode of
                        Completions ->
                            { oldForm
                                | completionsShow = newShow
                                , completionsSelected = Nothing
                                , completionsOptions = oldForm.completionsOptions
                            }

                        Fieldnames ->
                            { oldForm
                                | fieldnamesShow = newShow
                                , fieldnamesSelected = Nothing
                                , fieldnamesOptions = oldForm.fieldnamesOptions
                            }

                newField =
                    case mode of
                        Completions ->
                            { crit
                                | searchPhrase = newString
                                , form = newForm
                                , mode = newMode
                            }

                        Fieldnames ->
                            { crit
                                | fieldName = newString
                                , form = newForm
                                , mode = newMode
                            }

                newModel =
                    setCritToSearch formField newField model
            in
            ( newModel, Cmd.none )

        UserSelectedMode formField string ->
            let
                crit =
                    getCritFromSearch formField model

                newField =
                    { crit | mode = string}
                
                newModel =
                    setCritToSearch formField newField model
            in
            ( newModel, Cmd.none )

        NoOp ->
            ( model, Cmd.none )

        Submit ->
            ( model, Cmd.none )


type MenuMsg
    = MenuKeyDown Int
    | MenuMouseClick String
    | MenuMouseEnter String
    | MenuMouseLeave String
    | MenuWentTooHigh
    | MenuWentTooLow
    | NoMenuOp


updateMenu :
    MenuUpdateConfig msg data
    -> MenuMsg
    -> Int
    -> MenuState
    -> List data
    -> ( MenuState, Maybe msg )
updateMenu config msg howManyToShow state data =
    case msg of
        MenuKeyDown keyCode ->
            let
                boundedList =
                    List.map config.toId data
                        |> List.take howManyToShow

                newKey =
                    navigateWithKey keyCode boundedList state.key
            in
            if newKey == state.key && keyCode == 38 then
                updateMenu config MenuWentTooHigh howManyToShow state data

            else if newKey == state.key && keyCode == 40 then
                updateMenu config MenuWentTooLow howManyToShow state data

            else
                ( { key = newKey, mouse = newKey }
                , config.onKeyDown keyCode newKey
                )

        MenuWentTooHigh ->
            ( state, config.onTooHigh )

        MenuWentTooLow ->
            ( state, config.onTooLow )

        MenuMouseClick id ->
            ( resetMouseStateWithId id state
            , config.onMouseClick id
            )

        MenuMouseEnter id ->
            ( resetMouseStateWithId id state
            , config.onMouseEnter id
            )

        MenuMouseLeave id ->
            ( resetMouseStateWithId id state
            , config.onMouseLeave id
            )

        NoMenuOp ->
            ( state, Nothing )


resetMouseStateWithId : String -> MenuState -> MenuState
resetMouseStateWithId id state =
    { key = Just id, mouse = Just id }


navigateWithKey : Int -> List String -> Maybe String -> Maybe String
navigateWithKey code ids maybeId =
    case code of
        38 ->
            Maybe.map (getPreviousItemId ids) maybeId

        40 ->
            Maybe.map (getNextItemId ids) maybeId

        _ ->
            maybeId


getPreviousItemId : List String -> String -> String
getPreviousItemId ids selectedId =
    Maybe.withDefault selectedId (List.foldr (getPrevious selectedId) Nothing ids)


getPrevious : String -> String -> Maybe String -> Maybe String
getPrevious id selectedId resultId =
    if selectedId == id then
        Just id

    else if Maybe.withDefault "" resultId == id then
        Just selectedId

    else
        resultId


getNextItemId : List String -> String -> String
getNextItemId ids selectedId =
    Maybe.withDefault selectedId (List.foldl (getPrevious selectedId) Nothing ids)




-- === View ===

{-| In our view, we want to have several criteria each of which
consists of (a) a boolean mode in which the criterion is applied,
(b) a fieldname that is checked and (c) a search phrase that the
field is checked against. Both fieldnames and search phrases
have autocompletion menus.
The fieldname suggestions come from a list of available
fieldnames, the searchphrase suggestions come from a query to
the server for the occurrences of searchphrase \*in the field
determined in (b).
-}
view : Model -> Html.Html Msg
view model =
    Element.layout
        []
    <|
        Element.column
            [ Element.spacing 5 ]
            [ critForm Field1 "name" model
            , critForm Field2 "title" model
            ]


critForm : FormField -> String -> Model -> Element.Element Msg
critForm formField defaultField oldSearch =
    let
        oldField =
            getCritFromSearch formField oldSearch

        htmlModeFieldname =
            case formField of
                Field1 ->
                    "Field1_mode"
                Field2 ->
                    "Field2_mode"

    in
    Element.row
        [ Element.spacing 5 ]
        [ Element.Input.radio
            [ Element.htmlAttribute <|
                Html.Attributes.id htmlModeFieldname 
            ]
            { onChange = UserSelectedMode formField
            , selected = Just oldField.mode
            , label = Element.Input.labelAbove [] (Element.text "")
            , options =
                [ Element.Input.option "and" (Element.text "AND")
                , Element.Input.option "or" (Element.text "OR")
                ]
            }
        , smartFieldField formField defaultField oldSearch
        , smartTextField formField oldSearch
        ]


smartFieldField : FormField -> String -> Model -> Element.Element Msg
smartFieldField formField defaultValue oldSearch =
    let
        oldField =
            getCritFromSearch formField oldSearch

        oldForm =
            oldField.form

        query =
            oldForm.fieldnamesSelected
                --              |> Maybe.map identity
                |> Maybe.withDefault oldField.fieldName

        fieldnamesMenu =
            if oldForm.fieldnamesShow then
                viewFieldnamesMenu formField oldField

            else
                Element.none

        htmlFieldname =
            case formField of
                Field1 ->
                    "Field1_fieldName"
                Field2 ->
                    "Field2_fieldName"

    in
    Element.column
        [ Element.width (Element.fillPortion 3)
        , Element.below fieldnamesMenu
        ]
        [ Element.Input.search
            ([ Element.htmlAttribute <|
                Html.Events.preventDefaultOn "keydown" (upDownEscDecoder formField Fieldnames oldSearch)
             , Element.htmlAttribute <|
                Html.Attributes.value query
             , Element.htmlAttribute <|
                Html.Attributes.id htmlFieldname
             , Element.htmlAttribute <|
                 Html.Attributes.style "z-index" "0"
             ]
                ++ onEvent
                    [ ( "keyup", Submit )
                    , ( "focus", FetchFocused )
                    ]
            )
            { onChange = UserTypedText formField Fieldnames
            , text = oldField.fieldName
            , placeholder = Just <| Element.Input.placeholder [] <| Element.text defaultValue
            , label = Element.Input.labelAbove [] (Element.text "")
            }
        ]


smartTextField : FormField -> Model -> Element.Element Msg
smartTextField formField oldSearch =
    let
        oldField =
            getCritFromSearch formField oldSearch

        oldForm =
            oldField.form

        query =
            oldForm.completionsSelected
                |> Maybe.map .phrase
                |> Maybe.withDefault oldField.searchPhrase

        completionsMenu =
            if oldForm.completionsShow then
                viewCompletionsMenu formField oldField

            else
                Element.none
        
        htmlFieldname =
            case formField of
                Field1 ->
                    "Field1_searchPhrase"
                Field2 ->
                    "Field2_searchPhrase"

    in
    Element.column
        [ Element.width (Element.fillPortion 6)
        , Element.below completionsMenu
        ]
        [ Element.Input.text
            ([ Element.htmlAttribute <|
                Html.Events.preventDefaultOn "keydown" (upDownEscDecoder formField Completions oldSearch)
             , Element.htmlAttribute <|
                Html.Attributes.value query
             , Element.htmlAttribute <|
                Html.Attributes.id htmlFieldname 
             , Element.htmlAttribute <|
                 Html.Attributes.style "z-index" "0"
             ]
                ++ onEvent
                    [ ( "keyup", Submit )
                    , ( "focus", FetchFocused )
                    ]
            )
            { onChange = UserTypedText formField Completions
            , text = oldField.searchPhrase
            , placeholder = Just <| Element.Input.placeholder [] <| Element.text "Type your search string"
            , label = Element.Input.labelAbove [] (Element.text "")
            }
        ]


onEvent : List ( String, msg ) -> List (Element.Attribute msg)
onEvent l =
    List.map
        (\( s, m ) ->
            case s of
                "keyup" ->
                    Element.htmlAttribute
                        (Html.Events.on s
                            (Json.Decode.field "key" Json.Decode.string
                                |> Json.Decode.andThen
                                    (\key ->
                                        if key == "Enter" then
                                            Json.Decode.succeed m

                                        else
                                            Json.Decode.fail "Not the enter key"
                                    )
                            )
                        )

                _ ->
                    Element.htmlAttribute
                        (Html.Events.on s (Json.Decode.succeed m))
        )
        l


viewFieldnamesMenu : FormField -> Crit -> Element.Element Msg
viewFieldnamesMenu formField field =
    Element.el
        [ Element.Background.color (Element.rgb 0.96 0.96 0.96)
        , Element.Border.solid
        , Element.Border.width 1
        , Element.Border.color (Element.rgb 0.1 0.1 0.1)        
        , Element.htmlAttribute <|
                    Html.Attributes.style "z-index" "99"
        ]
        ( Element.html <|
            Html.div
                [ Html.Attributes.class "autocomplete-menu" ]
                [ Html.map (SetState formField Fieldnames) <|
                    menuView fieldnamesMenuViewConfig
                        field.form.fieldnamesHowMany
                        field.form.fieldnamesState
                        (acceptableSuggestions field.fieldName identity field.form.fieldnamesOptions)
                ]
        )


viewCompletionsMenu : FormField -> Crit -> Element.Element Msg
viewCompletionsMenu formField field =
    Element.el
        [ Element.Background.color (Element.rgb 0.96 0.96 0.96)
        , Element.Border.solid
        , Element.Border.width 1
        , Element.Border.color (Element.rgb 0 0 0)        
        , Element.htmlAttribute <|
                    Html.Attributes.style "z-index" "99"
        ]
        ( Element.html <|
        Html.div
            [ Html.Attributes.class "autocomplete-menu" ]
            [ Html.map (SetState formField Completions) <|
                menuView completionsMenuViewConfig
                    field.form.completionsHowMany
                    field.form.completionsState
                    (acceptableSuggestions field.searchPhrase .phrase field.form.completionsOptions)
            ]
        )


menuView : MenuViewConfig data -> Int -> MenuState -> List data -> Html.Html MenuMsg
menuView config howManyToShow state data =
    let
        customUlAttr =
            List.map mapNeverToMsg config.ul

        getKeyedItems datum =
            ( config.toId datum, viewItem config state datum )
    in
    List.take howManyToShow data
        |> List.map getKeyedItems
        |> Html.Keyed.ul customUlAttr


viewItem : MenuViewConfig data -> MenuState -> data -> Html.Html MenuMsg
viewItem { toId, li } { key, mouse } data =
    let
        id =
            toId data

        isSelected maybeId =
            case maybeId of
                Just someId ->
                    someId == id

                Nothing ->
                    False

        listItemData =
            li (isSelected key) (isSelected mouse) data

        customAttributes =
            List.map mapNeverToMsg listItemData.attributes

        customLiAttr =
            List.append customAttributes
                [ Html.Events.onMouseEnter (MenuMouseEnter id)
                , Html.Events.onMouseLeave (MenuMouseLeave id)
                , Html.Events.onClick (MenuMouseClick id)
                ]
    in
    Html.li customLiAttr (List.map (Html.map (\_ -> NoMenuOp)) listItemData.children)


reset : MenuUpdateConfig msg data -> MenuState
reset _ =
    emptyMenuState


{-| Like `reset` but defaults to a keyboard selection of the first item.
-}
resetToFirstItem : MenuUpdateConfig msg data -> List data -> Int -> MenuState -> MenuState
resetToFirstItem config data howManyToShow state =
    resetToFirst config (List.take howManyToShow data) state


resetToFirst : MenuUpdateConfig msg data -> List data -> MenuState -> MenuState
resetToFirst config data state =
    let
        setFirstItem datum newState =
            { newState | key = Just (config.toId datum) }
    in
    case List.head data of
        Nothing ->
            emptyMenuState

        Just datum ->
            setFirstItem datum emptyMenuState


{-| Like `reset` but defaults to a keyboard selection of the last item.
-}
resetToLastItem : MenuUpdateConfig msg data -> List data -> Int -> MenuState -> MenuState
resetToLastItem config data howManyToShow state =
    let
        reversedData =
            List.reverse (List.take howManyToShow data)
    in
    resetToFirst config reversedData state


resetMenuInput : FormField -> MenuMode -> Model -> Model
resetMenuInput formField mode model =
    let
        crit =
            getCritFromSearch formField model

        newField =
            case mode of
                Completions ->
                    { crit | searchPhrase = "" }
                Fieldnames ->
                    { crit | fieldName = "" }

        newModel =
            setCritToSearch formField newField model
    in
    newModel
        |> removeMenuSelection formField mode
        |> resetMenu formField mode


resetMenu : FormField -> MenuMode -> Model -> Model
resetMenu formField mode model =
    let
        crit =
            getCritFromSearch formField model

        oldForm =
            crit.form

        newForm =
            case mode of
                Completions ->
                    { oldForm
                        | completionsState = emptyMenuState
                        , completionsShow = False
                    }
                Fieldnames ->
                    { oldForm
                        | fieldnamesState = emptyMenuState
                        , fieldnamesShow = False
                    }

        newField =
            { crit | form = newForm }

        newModel =
            setCritToSearch formField newField model
    in
    newModel


removeMenuSelection : FormField -> MenuMode -> Model -> Model
removeMenuSelection formField mode model =
    let
        crit =
            getCritFromSearch formField model

        oldForm =
            crit.form

        newForm =
            case mode of
                Completions ->
                    { oldForm | completionsSelected = Nothing }
                Fieldnames ->
                    { oldForm | fieldnamesSelected = Nothing }

        newField =
            { crit | form = newForm }

        newModel =
            setCritToSearch formField newField model
    in
    newModel


getSuggestionAtId : List a -> String -> (a -> String) -> a -> a
getSuggestionAtId options id toId default =
    List.filter (\s -> toId s == id) options
        |> List.head
        |> Maybe.withDefault default


setQuery : String -> FormField -> MenuMode -> Model -> Model
setQuery id formField mode model =
    let
        crit =
            getCritFromSearch formField model

        oldForm =
            crit.form

        newForm =
            case mode of
                Completions ->
                    { oldForm
                        | completionsSelected =
                            Just
                                (getSuggestionAtId
                                    oldForm.completionsOptions
                                    id
                                    .phrase
                                    (Suggestion "" 0)
                                )
                    }
                Fieldnames ->
                    { oldForm
                        | fieldnamesSelected =
                            Just
                                (getSuggestionAtId
                                    oldForm.fieldnamesOptions
                                    id
                                    identity
                                    ""
                                )
                    }

        newField =
            case mode of
                Completions ->
                    { crit
                        | searchPhrase =
                            .phrase
                                (getSuggestionAtId
                                    oldForm.completionsOptions
                                    id
                                    .phrase
                                    (Suggestion "" 0)
                                )
                        , form = newForm
                    }
                Fieldnames ->
                    { crit
                        | fieldName =
                            (getSuggestionAtId
                                oldForm.fieldnamesOptions
                                id
                                identity
                                ""
                            )
                        , form = newForm
                    }

        newModel =
            setCritToSearch formField newField model
    in
    newModel


acceptableSuggestions : String -> (a -> String) -> List a -> List a
acceptableSuggestions s toId options =
    let
        lowerQuery =
            String.toLower s
    in
    List.filter (String.contains lowerQuery << String.toLower << toId) options


mapNeverToMsg : Html.Attribute Never -> Html.Attribute MenuMsg
mapNeverToMsg msg =
    Html.Attributes.map (\_ -> NoMenuOp) msg


upDownEscDecoderHelper : FormField -> MenuMode -> Model -> Int -> Json.Decode.Decoder Msg
upDownEscDecoderHelper formField mode model code =
    let
        crit =
            getCritFromSearch formField model
    in
    if code == 38 || code == 40 then
        Json.Decode.succeed NoOp

    else if code == 27 then
        Json.Decode.succeed (HandleEscape formField mode)

    else
        Json.Decode.fail "not handling that key"


upDownEscDecoder : FormField -> MenuMode -> Model -> Json.Decode.Decoder ( Msg, Bool )
upDownEscDecoder formField mode model =
    Html.Events.keyCode
        |> Json.Decode.andThen (upDownEscDecoderHelper formField mode model)
        |> Json.Decode.map (\msg -> ( msg, True ))



-- === Subscriptions ===


subscriptions : Model -> Sub Msg
subscriptions _ =
    Sub.batch
        [ Sub.map (\m -> SetSomeState m) menuSubscription
        , focusedFetched Focused
        ]


menuSubscription : Sub MenuMsg
menuSubscription =
    Browser.Events.onKeyDown (Json.Decode.map MenuKeyDown Html.Events.keyCode)



-- === Ports ===


port focusedFetched : (Maybe String -> msg) -> Sub msg

port fetchFocused : () -> Cmd msg



-- === Main ===


main : Program () Model Msg
main =
    Browser.element
        { view = view
        , init = \_ -> ( initialModel, Cmd.none )
        , update = update
        , subscriptions = subscriptions
        }



-- === Conf settings ===


dummyFieldnames : List String
dummyFieldnames =
    [ "name"
    , "title"
    , "address"
    , "email"
    , "zip code"
    ]

dummySuggestions : List Suggestion
dummySuggestions =
    [ Suggestion "blabla" 0.9
    , Suggestion "bloblo" 0.8
    , Suggestion "blaublau" 0.7
    , Suggestion "foobar" 0.6
    , Suggestion "foobaz" 0.5
    , Suggestion "xxxx" 0.4
    , Suggestion "yyyy" 0.3
    ]
