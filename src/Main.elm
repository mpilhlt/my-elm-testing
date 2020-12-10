module Main exposing (main)

import Browser
import Browser.Events exposing (onKeyPress)
import Element
import Element.Background
import Element.Border
import Element.Font
import Element.Input
import Html
import Html.Attributes
import Html.Events
import Json.Decode as Decode
import List
import String

-- === Model ===


type alias Model =
    { matters : List Matter }

type alias Matter =
    { matter : String
    , law_datum : String
    , law_form : String
    , ter_label : String
    , legislator_name : String
    , law_publication : String
    , law_archive : String
    }

type Msg =
    None

-- === View ===

view : Model -> Html.Html Msg
view model =
    Element.layout
        [ Element.height Element.fill ]
        ( Element.column
            [ Element.height Element.fill
            , Element.width Element.fill
            ]
            [ listPanel model ]
        )

listPanel : Model -> Element.Element Msg
listPanel model =
    Element.column
        [ Element.height Element.fill
        , Element.width Element.fill
        , Element.clip
        , Element.paddingEach
            { top = 0
            , right = 15
            , bottom = 0
            , left = 22
            }
        ]
        [ resultsetNavBar
        , lTable model.matters
        ]

resultsetNavBar : Element.Element Msg
resultsetNavBar =
    Element.row
        [ Element.alignRight
        , Element.paddingEach
            { top = 5
            , right = 5
            , bottom = 5
            , left = 5
            }
        , Element.spacingXY 2 0
        ]
        [ Element.Input.button
            [ Element.Border.solid
            , Element.Border.width 1
            , Element.Border.color <| Element.rgb255 0 0 0
            , Element.padding 2
            ]
            { onPress = Nothing
            , label = Element.text " |< "
            }
        , Element.Input.button
            [ Element.Border.solid
            , Element.Border.width 1
            , Element.Border.color <| Element.rgb255 0 0 0
            , Element.padding 2
            ]
            { onPress = Nothing
            , label = Element.text " < "
            }
        , Element.Input.button
            [ Element.Border.solid
            , Element.Border.width 1
            , Element.Border.color <| Element.rgb255 0 0 0
            , Element.padding 2
            ]
            { onPress = Nothing
            , label = Element.text " > "
            }
        , Element.Input.button
            [ Element.Border.solid
            , Element.Border.width 1
            , Element.Border.color <| Element.rgb255 0 0 0
            , Element.padding 2
            ]
            { onPress = Nothing
            , label = Element.text " >| "
            }
        ]

headerFormat : List (Element.Attribute msg)
headerFormat =
    [ Element.clip
    , Element.Font.bold
    , Element.Background.color <| Element.rgb255 238 238 238
    , Element.paddingXY 8 2
    ]

cellFormat : Int -> List (Element.Attribute msg)
cellFormat index =
    [ Element.paddingXY 8 2
    , Element.Border.solid
    , Element.Border.width 0
    , Element.Border.color <| Element.rgb255 0 0 0
    , Element.clip
    ]
    ++ rowFormat index

rowFormat : Int -> List (Element.Attribute msg)
rowFormat index =
    if modBy 2 index == 1 then
        [ Element.Background.color <| Element.rgb255 238 238 238 ]

    else
        [ Element.Background.color <| Element.rgb255 255 255 255 ]

lTable : List Matter -> Element.Element Msg
lTable matters =
    let
        longstring =
            45

        maxLengthString s =
            if String.length s < longstring then
                s

            else
                String.slice 0 longstring s ++ "..."

    in
    Element.indexedTable
        [ Element.spacing 0
        , Element.width Element.fill
        , Element.height Element.fill
        , Element.clip
        , Element.scrollbarY
        , Element.explain Debug.todo
        ]
        { data = matters
        , columns =
            [ { header = Element.text " "
              , width = Element.fillPortion 2
              , view =
                    \index _ ->
                        Element.el
                            ( cellFormat index )
                            ( Element.text ">" )
              }
            , { header = Element.el headerFormat ( Element.text "Gruppe/Policeymaterie" )
              , width = Element.fillPortion 20
              , view =
                    \index matter ->
                        Element.el
                            ( cellFormat index )
                            ( Element.text (maxLengthString matter.matter) )
              }
            , { header = Element.el headerFormat ( Element.text "Datum" )
              , width = Element.fillPortion 8
              , view =
                    \index matter ->
                        Element.el
                            ( cellFormat index )
                            ( Element.text matter.law_datum )
              }
            , { header = Element.el headerFormat (Element.text "Gesetzesform")
              , width = Element.fillPortion 10
              , view =
                    \index matter ->
                        Element.el
                            ( cellFormat index )
                            ( Element.text (maxLengthString matter.law_form) )
              }
            , { header = Element.el headerFormat (Element.text "Territorium")
              , width = Element.fillPortion 20
              , view =
                    \index matter ->
                        Element.el
                            ( cellFormat index )
                            ( Element.text (maxLengthString matter.ter_label) )
              }
            , { header = Element.el headerFormat (Element.text "Gesetzgeber")
              , width = Element.fillPortion 20
              , view =
                    \index matter ->
                        Element.el
                            ( cellFormat index )
                            ( Element.text (maxLengthString matter.legislator_name) )
              }
            , { header = Element.el headerFormat (Element.text "Druck | Archiv")
              , width = Element.fillPortion 20
              , view =
                    \index matter ->
                        let
                            field_contents =
                                String.join
                                    " | "
                                    (List.filter (not << String.isEmpty) [ matter.law_publication, matter.law_archive ])
                        in
                        Element.el
                            ( cellFormat index )
                            (Element.text (maxLengthString field_contents) )
              }
            ]
        }


-- === Main ===

main : Program () Model Msg
main =
    Browser.element
        { view = view
        , init = \_ -> ( initialModel, Cmd.none )
        , subscriptions = \_ -> onKeyPress (Decode.succeed None)
        , update = \_ model -> (model, Cmd.none)
        }

initialModel : Model
initialModel =
    { matters =
        [ Matter
            "1.1 Bettelmönche"
            "21.09.1495"
            "Landesordnung"
            "Baden (1475-1503)"
            "Christoph I."
            "Carlebach I, S. 93-118; Schmelzeisen, Polizei... foo"
            "bar blah"
        , Matter
            "1.1 Bettelmönche"
            "09.02.1497"
            "Reichsabschied"
            "Deutsches Reich"
            "Maximilian I."
            "NSRA II, S. 29-35; RTA MR VI, S. 338-352 (kei... foo"
            "bar blah"
        , Matter
            "1.1 Bettelmönche"
            "04.09.1498"
            "Reichsabschied"
            "Deutsches Reich"
            "Maximilian I."
            "NSRA II, S. 38-54; RTA MR VI, S. 718-746 (kei... foo"
            "bar blah"
        , Matter
            "1.1 Bettelmönche"
            "10.09.1500"
            "Reichsabschied"
            "Deutsches Reich"
            "Maximilian I."
            "NSRA II, S. 63-91 foo"
            "bar blah"
        , Matter
            "1.1 Bettelmönche"
            "22.11.1514"
            "Ratsbeschluß"
            "Köln"
            "Rat der Stadt"
            "Regest: Groten, Beschlüsse, Bd. 2, S. 213 (Nr... foo"
            "bar blah"
        , Matter
            "1.1 Bettelmönche"
            "03.07.1525"
            "Verordnung"
            "Jülich-Berg (1511-1609)"
            "Johann III."
            "Redlich, Kirchenpolitik 1, S. 232-236 (Auszug... foo"
            "bar blah"
        , Matter
            "1.1 Bettelmönche"
            "08.07.1525"
            "Kirchenordnung"
            "Kleve und Mark"
            "Johann III., der Friedfertige"
            "SJB I, S. 19-25 (Nr. 21) foo"
            "bar blah"
        , Matter
            "1.1 Bettelmönche"
            "08.04.1533"
            "Kirchenordnung"
            "Jülich-Berg (1511-1609)"
            "Johann III."
            "SCM 1, S. 62-83 (Nr. 33); Redlich, Kirchenpol... foo"
            "bar blah"
        , Matter
            "1.1 Bettelmönche"
            "08.04.1533"
            "Kirchenordnung"
            "Kleve und Mark"
            "Johann III., der Friedfertige"
            "SCM I, S. 62-82 (Nr. 33) foo"
            "bar blah"
        , Matter
            "1.1 Bettelmönche"
            "04.12.1588"
            "Reskript"
            "Baden-Baden (1588-1594)"
            "Eduard Fortunat"
            "GLA 74/2792 (fol. 78 f.) foo"
            "bar blah"
        , Matter
            "1.1 Bettelmönche"
            "30.08.1659"
            "Generalbefehl"
            "Pfalz (1648-1777), restituiert ohne Oberpfalz..."
            "Karl I. Ludwig"
            "GLA KA, 67/1570 foo"
            "bar blah"
        , Matter
            "1.1 Bettelmönche"
            "27.09.1666"
            "Verordnung"
            "Würzburg"
            "Johann Philipp I. von Schönborn"
            "TJF II, S. 1944 f. foo"
            "bar blah"
        , Matter
            "1.1 Bettelmönche"
            "26.04.1668"
            "[Reskript]"
            "Augsburg, Hochstift"
            "Johann Christoph von Freyberg-Eisenberg"
            "Nachweis: Braun, Geschichte der Bischöfe von ... foo"
            "bar blah"
        , Matter
            "1.1 Bettelmönche"
            "04.04.1673"
            "Befehl"
            "Bayern (1623-1799) mit Oberpfalz"
            "Ferdinand Maria"
            "MGS 4, S. 745 foo"
            "bar blah"
        , Matter
            "1.1 Bettelmönche"
            "24.06.1684"
            "Verordnung"
            "Würzburg"
            "Konrad Wilhelm"
            "SWL 1, S. 327 foo"
            "bar blah"
        , Matter
            "1.1 Bettelmönche"
            "02.09.1702"
            "Mandat"
            "Würzburg"
            "Johann Philipp II."
            "SWL 1, S. 528 f. foo"
            "bar blah"
        , Matter
            "1.1 Bettelmönche"
            "17.10.1705"
            "Verordnung"
            "Köln"
            "Joseph Clemens, Herzog von Bayern"
            "HStA Düsseldorf, KK II 3125, Nr. 110 foo"
            "bar blah"
        , Matter
            "1.1 Bettelmönche"
            "19.04.1706"
            "[Verordnung]"
            "Kleve und Mark"
            "Friedrich (III.) I."
            "SCM II, S. 740 (Nr. 558) foo"
            "bar blah"
        , Matter
            "1.1 Bettelmönche"
            "22.08.1710"
            "Verordnung"
            "Mainz"
            "Lothar Franz von Schönborn"
            "Rösch, Armenreform, Anhang, S. 3-11 | StdA MZ... foo"
            "bar blah"
        , Matter
            "1.1 Bettelmönche"
            "05.11.1720"
            "Vagantenordnung"
            "Augsburg, Hochstift"
            "Alexander Sigmund von Pfalz-Neuburg"
            "StAA Ho Aug NA Akten 649a, 662c; Druckschrift.. foo"
            "bar blah"
        , Matter
            "1.1 Bettelmönche"
            "23.01.1740"
            "Verordnung"
            "Trier"
            "Franz Georg von Schönborn"
            "Nachweis: TK 5, S. 119 foo"
            "bar blah"
        , Matter
            "1.1 Bettelmönche"
            "16.10.1742"
            "Verordnung"
            "Würzburg"
            "Friedrich Karl"
            "SWL 2, S. 312 foo"
            "bar blah"
        , Matter
            "1.1 Bettelmönche"
            "16.10.1742"
            "Verordnung"
            "Würzburg"
            "Friedrich Karl"
            "SWL 2, S. 313 foo"
            "bar blah"
        , Matter
            "1.1 Bettelmönche"
            "06.08.1743"
            "Reskript"
            "Baden-Baden (1666-1771)"
            "Ludwig Georg Simpert"
            "Wesentlicher Inhalt I, S. 98 foo"
            "bar blah"
        , Matter
            "1.1 Bettelmönche"
            "20.09.1743"
            "Verordnung"
            "Pfalz (1648-1777), restituiert ohne Oberpfalz..."
            "Karl Theodor"
            "GLA KA, 77/3174 foo"
            "bar blah"
        , Matter
            "1.1 Bettelmönche"
            "02.01.1748"
            "Bettelordnung"
            "Bayern (1623-1799) mit Oberpfalz"
            "Max III. Joseph"
            "MGS 2, S. 721-726 foo"
            "bar blah"
        , Matter
            "1.1 Bettelmönche"
            "00.00.1749"
            "Vagantenordnung"
            "Augsburg, Hochstift"
            "Joseph von Hessen"
            "StAA Ho Aug NA Akten 649a, 662c; Druckschrift... foo"
            "bar blah"
        , Matter
            "1.1 Bettelmönche"
            "05.05.1750"
            "Reskript"
            "Mainz"
            "Johann Friedrich Karl von Ostein"
            "HHStA WBD, 100/214; StdA MZ, LVO; BStA WBG, M... foo"
            "bar blah"
        , Matter
            "1.1 Bettelmönche"
            "06.04.1752"
            "Reskript"
            "Trier"
            "Franz Georg von Schönborn"
            "LHA KO, 1C/1115 foo"
            "bar blah"
        , Matter
            "1.1 Bettelmönche"
            "04.12.1752"
            "Verordnung"
            "Würzburg"
            "Karl Philipp"
            "SWL 2, S. 628 foo"
            "bar blah"
        , Matter
            "1.1 Bettelmönche"
            "16.02.1754"
            "Reskript"
            "Speyer"
            "Franz Christoph von Hutten"
            "SpDV II, Nr. 65, S. 213 foo"
            "bar blah"
        ]
    }
