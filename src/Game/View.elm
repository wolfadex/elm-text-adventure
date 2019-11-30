module Game.View exposing (ParentMsg, Size(..), view)

{-| The program for playing your game.

    import Game
    import Game.View

    {-| The most basic game possible
    -}
    main =
        Game.makeGame "Sample Game"
            |> Game.view.program

@docs program

-}

import Color exposing (ColorSet)
import Dict
import Element exposing (Attribute, Color, Element)
import Element.Background as Background
import Element.Border as Border
import Element.Font as Font
import Element.Input as Input
import Game.Internal exposing (Connection, Detail(..), Game, Id, Item(..), Locked(..), Mode(..), Msg(..), RoomId(..), Theme(..))
import Html exposing (Html)
import Html.Attributes
import Set exposing (Set)


type alias ParentMsg msg =
    Msg -> msg


type Size
    = Large
    | Small


view : ParentMsg msg -> Size -> Game -> Html msg
view parentMsg size game =
    Element.layout
        [ Element.width Element.fill
        , Element.height Element.fill
        , Background.color (colorFromTheme .background game.theme)
        , Font.color (colorFromTheme .font game.theme)
        ]
        (case game.mode of
            Running ->
                viewRunning parentMsg size game

            Finished ->
                viewFinished parentMsg game

            Building ->
                viewBuilding
        )


colorFromTheme : (ColorSet -> Color) -> Theme -> Color
colorFromTheme part theme =
    case theme of
        Light ->
            part Color.light

        Dark ->
            part Color.dark


viewRunning : ParentMsg msg -> Size -> Game -> Element msg
viewRunning parentMsg size game =
    let
        nameView =
            viewGameName game.theme parentMsg game.name

        roomDescView =
            viewRoomDesc parentMsg size game

        exitView =
            viewExits parentMsg size game

        itemView =
            game
                |> Game.Internal.getCurrentRoom
                |> .contents
                |> viewItemList (parentMsg ToggleRoomItems) .roomItemsDetail size "Items in Room" (viewRoomItem game.theme parentMsg) game

        inventoryView =
            game
                |> .inventory
                |> viewItemList (parentMsg ToggleInventory) .inventoryDetail size "Inventory" (viewPlayerItem game.theme parentMsg) game
    in
    case size of
        Large ->
            Element.row
                [ Element.centerX
                , Element.width (Element.fill |> Element.maximum (scaled 22))
                , Element.height Element.fill
                ]
                [ customStyles
                , Element.column
                    [ Element.width Element.fill
                    , Element.height Element.fill
                    ]
                    [ nameView
                    , spacerHorizontal game.theme
                    , roomDescView
                    , spacerHorizontal game.theme
                    , exitView
                    , spacerHorizontal game.theme
                    , itemView
                    , spacerHorizontal game.theme
                    , inventoryView
                    ]
                , spacerVertical game.theme
                , viewGameLog game
                ]

        Small ->
            Element.column
                [ Element.centerX
                , Element.width Element.fill
                , Element.height Element.fill
                , Font.size 32
                ]
                [ customStyles
                , nameView
                , spacerHorizontal game.theme
                , roomDescView
                , spacerHorizontal game.theme
                , exitView
                , spacerHorizontal game.theme
                , itemView
                , spacerHorizontal game.theme
                , inventoryView
                , spacerHorizontal game.theme
                , viewGameLog game
                ]


viewCollapsible : Theme -> msg -> Detail -> String -> Element msg -> Element msg
viewCollapsible theme action detailState label child =
    Element.column
        [ Element.padding (scaled 1)
        , Element.width Element.fill
        , Background.color (colorFromTheme .background theme)
        ]
        [ Element.row
            []
            [ button
                { label =
                    case detailState of
                        Expanded ->
                            ">"

                        Collapsed ->
                            "^"
                , action = Just action
                , theme = theme
                }
            , Element.el
                [ Element.paddingXY (scaled 1) 0 ]
                (Element.text (label ++ ":"))
            ]
        , case detailState of
            Expanded ->
                child

            Collapsed ->
                Element.none
        ]


viewGameName : Theme -> ParentMsg msg -> String -> Element msg
viewGameName theme parentMsg name =
    Element.row
        [ Element.padding (scaled 1)
        , Element.width Element.fill
        , Background.color (colorFromTheme .background theme)
        ]
        [ Element.el
            [ Element.width Element.fill ]
            (Element.text name)
        , button
            { label =
                case theme of
                    Light -> "Dark"
                    Dark -> "Light"
            , action = Just (parentMsg ToggleTheme)
            , theme = theme
            }
        ]


viewFinished : ParentMsg msg -> Game -> Element msg
viewFinished parentMsg game =
    Element.column
        [ Element.centerX
        , Element.width (Element.fill |> Element.maximum (scaled 18))
        , Element.height Element.fill
        , Background.color (colorFromTheme .background game.theme)
        ]
        [ customStyles
        , viewGameLog game
        , Element.el
            [ Element.padding (scaled 2) ]
            (button { label = "Restart", action = Just (parentMsg Restart), theme = game.theme })
        ]


viewBuilding : Element msg
viewBuilding =
    Element.none


viewRoomDesc : ParentMsg msg -> Size -> Game -> Element msg
viewRoomDesc parentMsg size game =
    let
        currentRoom =
            Game.Internal.getCurrentRoom game
    in
    case size of
        Large ->
            Element.paragraph
                [ Element.padding (scaled 1)
                , whiteSpacePre
                , Background.color (colorFromTheme .background game.theme)
                , Element.scrollbarY
                , Element.height Element.fill
                ]
                [ Element.text (.name currentRoom ++ ":")
                , Element.text (.description currentRoom)
                ]

        Small ->
            viewCollapsible
                game.theme
                (parentMsg ToggleDescription)
                game.descriptionDetail
                (.name currentRoom)
                (Element.paragraph
                    [ Element.padding (scaled 1)
                    , whiteSpacePre
                    , Background.color (colorFromTheme .background game.theme)
                    , Element.scrollbarY
                    , Element.height Element.fill
                    ]
                    [ Element.text (.description currentRoom)
                    ]
                )


viewExits : ParentMsg msg -> Size -> Game -> Element msg
viewExits parentMsg size game =
    let
        exits =
            List.map
                (viewExit game.theme parentMsg)
                (game |> Game.Internal.getCurrentRoom |> .connections)
    in
    case size of
        Large ->
            Element.column
                [ Element.spacing (scaled 1)
                , Element.padding (scaled 1)
                , Element.scrollbarY
                , Background.color (colorFromTheme .background game.theme)
                , Element.width Element.fill
                , Element.height Element.fill
                ]
                (Element.text "Exits:" :: exits)

        Small ->
            viewCollapsible
                game.theme
                (parentMsg ToggleExits)
                game.exitsDetail
                "Exits"
                (Element.column
                    [ Element.spacing (scaled 1)
                    , Element.padding (scaled 1)
                    , Element.scrollbarY
                    , Background.color (colorFromTheme .background game.theme)
                    , Element.width Element.fill
                    , Element.height Element.fill
                    ]
                    exits
                )


viewExit : Theme -> ParentMsg msg -> Connection -> Element msg
viewExit theme parentMsg { name, description, to, locked, message } =
    Element.wrappedRow
        []
        [ button
            { label = name
            , action =
                case locked of
                    Locked ->
                        Nothing

                    Unlocked ->
                        Just (parentMsg (MoveRoom to message))
            , theme = theme
            }
        , buttonSpacer
        , Element.el
            [ whiteSpacePre
            , Element.width Element.fill
            ]
            (Element.text description)
        ]


viewItemList : msg -> (Game -> Detail) -> Size -> String -> (Id -> ( String, String ) -> List (Element msg)) -> Game -> Set Id -> Element msg
viewItemList toggleAction getDetailState size label viewFn game idSet =
    let
        content =
            if Set.isEmpty idSet then
                [ Element.text "Empty" ]

            else
                idSet
                    |> (\ids -> Dict.filter (\id _ -> Set.member id ids) game.items)
                    |> Dict.toList
                    |> List.map
                        (\( id, item ) ->
                            viewItemContainer id item viewFn
                        )
    in
    case size of
        Large ->
            Element.column
                (itemListStyle game.theme)
                (Element.text (label ++ ":") :: content)

        Small ->
            viewCollapsible
                game.theme
                toggleAction
                (getDetailState game)
                label
                (Element.column (itemListStyle game.theme) content)


itemListStyle : Theme -> List (Attribute msg)
itemListStyle theme =
    [ Element.spacing (scaled 1)
    , Element.padding (scaled 1)
    , Element.scrollbarY
    , Background.color (colorFromTheme .background theme)
    , Element.width Element.fill
    , Element.height Element.fill
    ]


viewItemContainer : Int -> Item -> (Int -> ( String, String ) -> List (Element msg)) -> Element msg
viewItemContainer id item viewFn =
    let
        nameDesc =
            case item of
                Tool { name, description } ->
                    ( name, description )

                Container { name, description } ->
                    ( name, description )
    in
    Element.wrappedRow
        []
        (viewFn id nameDesc)


viewRoomItem : Theme -> ParentMsg msg -> Id -> ( String, String ) -> List (Element msg)
viewRoomItem theme parentMsg id ( n, d ) =
    [ button { label = n, action = Just (parentMsg (PickUpItem id)), theme = theme }
    , buttonSpacer
    , Element.text d
    ]


viewPlayerItem : Theme -> ParentMsg msg -> Id -> ( String, String ) -> List (Element msg)
viewPlayerItem theme parentMsg id ( n, d ) =
    [ button { label = "Drop", action = Just (parentMsg (DropItem id)), theme = theme }
    , buttonSpacer
    , button { label = "Use", action = Just (parentMsg (UseItem id)), theme = theme }
    , buttonSpacer
    , Element.text <| n ++ ":"
    , buttonSpacer
    , Element.text d
    ]


viewGameLog : Game -> Element msg
viewGameLog game =
    case game.mode of
        Running ->
            game.log
                |> List.map (\log -> Element.paragraph [ whiteSpacePre ] [ Element.text log ])
                |> List.intersperse (logSeparator game.theme)
                |> Element.column
                    [ Element.spacing <| scaled 1
                    , Element.padding <| scaled 1
                    , Element.scrollbarY
                    , Element.width Element.fill
                    , Element.height Element.fill
                    , Background.color (colorFromTheme .background game.theme)
                    ]

        Finished ->
            game.log
                |> List.take 2
                |> List.map (\log -> Element.paragraph [ whiteSpacePre ] [ Element.text log ])
                |> List.intersperse (logSeparator game.theme)
                |> Element.column
                    [ Element.padding (scaled 1)
                    , Element.spacing (scaled 1)
                    , Background.color (colorFromTheme .background game.theme)
                    ]

        Building ->
            Element.none


spacerHorizontal : Theme -> Element msg
spacerHorizontal theme =
    Element.el
        [ Border.color (colorFromTheme .border theme)
        , Border.width 3
        , Element.width Element.fill
        ]
        Element.none


spacerVertical : Theme -> Element msg
spacerVertical theme =
    Element.el
        [ Border.color (colorFromTheme .border theme)
        , Border.width 3
        , Element.height Element.fill
        ]
        Element.none


button : { label : String, action : Maybe msg, theme : Theme } -> Element msg
button { label, action, theme } =
    Input.button
        [ Border.shadow
            { offset = ( 1, 1 )
            , size = 1
            , blur = 2
            , color = colorFromTheme .button theme
            }
        , Border.rounded 3
        , Element.paddingXY 5 1
        , Background.color <|
            case action of
                Nothing ->
                    Element.rgb 0.4 0.4 0.2

                Just _ ->
                    colorFromTheme .button theme
        ]
        { onPress = action
        , label = Element.text label
        }


buttonSpacer : Element msg
buttonSpacer =
    Element.el
        [ Element.width <| Element.px (scaled 1) ]
        Element.none


logSeparator : Theme -> Element msg
logSeparator theme =
    Element.el
        [ Element.width Element.fill
        , Border.width 1
        , Border.color (colorFromTheme .border theme)
        ]
        Element.none


scaled : Int -> Int
scaled i =
    Element.modular 16 1.25 i |> floor


whiteSpacePre : Element.Attribute msg
whiteSpacePre =
    Html.Attributes.class "wolfadex__elm-text-adventure__white-space_pre" |> Element.htmlAttribute


customStyles : Element msg
customStyles =
    Element.html <|
        Html.node "style"
            []
            [ Html.text """
.wolfadex__elm-text-adventure__white-space_pre > .t {
  white-space: pre-wrap !important;
}""" ]
