module Game.View exposing (view, ParentMsg, Size(..))

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

import Dict
import Element exposing (Element, Color, Attribute)
import Element.Background as Background
import Element.Border as Border
import Element.Font as Font
import Element.Input as Input
import Game.Internal exposing (Game, Detail(..), Id, Item(..), Mode(..), Msg(..), RoomId(..), Locked(..), Connection)
import Html exposing (Html)
import Html.Attributes
import Set exposing (Set)


type alias ParentMsg msg = Msg -> msg


type Size
    = Large
    | Small


view : ParentMsg msg -> Size -> Game -> Html msg
view parentMsg size game =
    Element.layout
        [ Element.width Element.fill
        , Element.height Element.fill
        , Background.color <| Element.rgb 0.1 0.1 0.1
        ]
        (case game.mode of
            Running ->
                viewRunning parentMsg size game

            Finished ->
                viewFinished parentMsg game

            Building ->
                viewBuilding
        )


viewRunning : ParentMsg msg -> Size -> Game -> Element msg
viewRunning parentMsg size game =
    let
        nameView = viewGameName game.name

        roomDescView = viewRoomDesc parentMsg size game

        exitView = viewExits parentMsg size game

        itemView =
            game
                |> Game.Internal.getCurrentRoom
                |> .contents
                |> viewItemList (parentMsg ToggleRoomItems) .roomItemsDetail size "Items in Room" (viewRoomItem parentMsg) game

        inventoryView =
            game
                |> .inventory
                |> viewItemList (parentMsg ToggleInventory) .inventoryDetail size "Inventory" (viewPlayerItem parentMsg) game
    in
    case size of
        Large ->
            Element.row
                [ Element.centerX
                , Element.width (Element.fill |> Element.maximum (scaled 22))
                , Element.height Element.fill
                , Font.color colorWhite
                , mobileStyling
                , Html.Attributes.class "wolfadex__elm-text-adventure__main-view" |> Element.htmlAttribute
                ]
                [ customStyles
                , Element.column
                    [ Element.width Element.fill
                    , Element.height Element.fill
                    ]
                    [ nameView
                    , spacerHorizontal
                    , roomDescView
                    , spacerHorizontal
                    , exitView
                    , spacerHorizontal
                    , itemView
                    , spacerHorizontal
                    , inventoryView
                    ]
                , spacerVertical
                , viewGameLog game
                ]

        Small ->
            Element.column
                [ Element.centerX
                , Element.width Element.fill
                , Element.height Element.fill
                , Font.color colorWhite
                , Font.size 32
                , mobileStyling
                , Html.Attributes.class "wolfadex__elm-text-adventure__main-view" |> Element.htmlAttribute
                ]
                [ customStyles
                , nameView
                , spacerHorizontal
                , roomDescView
                , spacerHorizontal
                , exitView
                , spacerHorizontal
                , itemView
                , spacerHorizontal
                , inventoryView
                , spacerHorizontal
                , viewGameLog game
                ]


viewCollapsible : msg -> Detail -> String -> Element msg -> Element msg
viewCollapsible action detailState label child =
    Element.column
        [ Element.padding (scaled 1)
        , Element.width Element.fill
        , Background.color colorGrayLight
        ]
        [ Element.row
            []
            [ button
                { label = case detailState of
                            Expanded -> ">"
                            Collapsed -> "^"
                , action = Just action
                }
            , Element.el 
                [ Element.paddingXY (scaled 1) 0]
                (Element.text (label ++ ":"))
            ]
        , case detailState of
            Expanded ->
                child

            Collapsed ->
                Element.none
        ]



viewGameName : String -> Element msg
viewGameName name =
    Element.el
        [ Element.padding (scaled 1)
        , Element.width Element.fill
        , Background.color colorGrayLight
        ]
        (Element.text name)


viewFinished : ParentMsg msg -> Game -> Element msg
viewFinished parentMsg game =
    Element.column
        [ Element.centerX
        , Element.width (Element.fill |> Element.maximum (scaled 18))
        , Element.height Element.fill
        , Background.color colorGrayLight
        , Font.color colorWhite
        , mobileStyling
        , Html.Attributes.class "wolfadex__elm-text-adventure__main-view" |> Element.htmlAttribute
        ]
        [ customStyles
        , viewGameLog game
        , Element.el
            [ Element.padding (scaled 2) ]
            (button { label = "Restart", action = Just (parentMsg Restart) })
        ]


viewBuilding : Element msg
viewBuilding =
    Element.none


viewRoomDesc : ParentMsg msg -> Size -> Game -> Element msg
viewRoomDesc parentMsg size game =
    let
        currentRoom = Game.Internal.getCurrentRoom game
    in
    case size of
        Large ->
            Element.paragraph
                [ Element.padding (scaled 1)
                , whiteSpacePre
                , mobileStyling
                , Background.color colorGrayLight
                , Element.scrollbarY
                , Element.height Element.fill
                ]
                [ Element.text (.name currentRoom ++ ":")
                , Element.text (.description currentRoom)
                ]

        Small ->
            viewCollapsible
                (parentMsg ToggleDescription)
                game.descriptionDetail
                (.name currentRoom)
                (Element.paragraph
                    [ Element.padding (scaled 1)
                    , whiteSpacePre
                    , mobileStyling
                    , Background.color colorGrayLight
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
                (viewExit parentMsg)
                (game |> Game.Internal.getCurrentRoom |> .connections)
    in
    case size of
        Large ->
            Element.column
                [ Element.spacing (scaled 1)
                , Element.padding (scaled 1)
                , Element.scrollbarY
                , Background.color colorGrayLight
                , Element.width Element.fill
                , Element.height Element.fill
                ]
                (Element.text "Exits:" :: exits)

        Small ->
            viewCollapsible
                (parentMsg ToggleExits)
                game.exitsDetail
                "Exits"
                (Element.column
                    [ Element.spacing (scaled 1)
                    , Element.padding (scaled 1)
                    , Element.scrollbarY
                    , Background.color colorGrayLight
                    , Element.width Element.fill
                    , Element.height Element.fill
                    ]
                    exits
                )


viewExit : ParentMsg msg -> Connection -> Element msg
viewExit parentMsg { name, description, to, locked, message } =
    Element.wrappedRow
        [ mobileStyling ]
        [ button
            { label = name
            , action =
                case locked of
                    Locked ->
                        Nothing

                    Unlocked ->
                        Just (parentMsg (MoveRoom to message))
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
                itemListStyle
                (Element.text (label ++ ":") :: content)

        Small ->
            viewCollapsible
                toggleAction
                (getDetailState game)
                label
                (Element.column itemListStyle content)


itemListStyle : List (Attribute msg)
itemListStyle =
    [ Element.spacing (scaled 1)
    , Element.padding (scaled 1)
    , Element.scrollbarY
    , Background.color colorGrayLight
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
        [ mobileStyling ]
        (viewFn id nameDesc)


viewRoomItem : ParentMsg msg -> Id -> ( String, String ) -> List (Element msg)
viewRoomItem parentMsg id ( n, d ) =
    [ button { label = n, action = Just (parentMsg (PickUpItem id)) }
    , buttonSpacer
    , Element.text d
    ]


viewPlayerItem : ParentMsg msg -> Id -> ( String, String ) -> List (Element msg)
viewPlayerItem parentMsg id ( n, d ) =
    [ button { label = "Drop", action = Just (parentMsg (DropItem id)) }
    , buttonSpacer
    , button { label = "Use", action = Just (parentMsg (UseItem id)) }
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
                |> List.map (\log -> Element.paragraph [ whiteSpacePre, mobileStyling ] [ Element.text log ])
                |> List.intersperse logSeparator
                |> Element.column
                    [ Element.spacing <| scaled 1
                    , Element.padding <| scaled 1
                    , Element.scrollbarY
                    , Element.width Element.fill
                    , Element.height Element.fill
                    , Background.color colorGrayLight
                    ]

        Finished ->
            game.log
                |> List.take 2
                |> List.map (\log -> Element.paragraph [ whiteSpacePre, mobileStyling ] [ Element.text log ])
                |> List.intersperse logSeparator
                |> Element.column
                    [ Element.padding (scaled 1)
                    , Element.spacing (scaled 1)
                    , Background.color colorGrayLight
                    ]

        Building ->
            Element.none


spacerHorizontal : Element msg
spacerHorizontal =
    Element.el
        [ Border.color colorGrayDark
        , Border.width 3
        , Element.width Element.fill
        ]
        Element.none


spacerVertical : Element msg
spacerVertical =
    Element.el
        [ Border.color colorGrayDark
        , Border.width 3
        , Element.height Element.fill
        ]
        Element.none


button : { label : String, action : Maybe msg } -> Element msg
button { label, action } =
    Input.button
        [ Border.shadow
            { offset = ( 1, 1 )
            , size = 1
            , blur = 2
            , color = Element.rgba 1 1 1 0.5
            }
        , Element.paddingXY 3 1
        , Background.color <|
            case action of
                Nothing ->
                    Element.rgb 0.4 0.4 0.2

                Just _ ->
                    Element.rgb 0.4 0.4 0.8
        , mobileStyling
        ]
        { onPress = action
        , label = Element.text label
        }


buttonSpacer : Element msg
buttonSpacer =
    Element.el
        [ Element.width <| Element.px (scaled 1) ]
        Element.none


logSeparator : Element msg
logSeparator =
    Element.el
        [ Element.width Element.fill
        , Border.width 1
        , Border.color (Element.rgb 0.4 0.4 0.4)
        ]
        Element.none


scaled : Int -> Int
scaled i =
    Element.modular 16 1.25 i |> floor


colorGrayDark : Color
colorGrayDark =
    Element.rgb 0.07 0.07 0.07


colorGrayLight : Color
colorGrayLight =
    Element.rgb 0.15 0.15 0.15


colorWhite : Color
colorWhite =
    Element.rgb 0.8 0.8 0.8


whiteSpacePre : Element.Attribute msg
whiteSpacePre =
    Html.Attributes.class "wolfadex__elm-text-adventure__white-space_pre" |> Element.htmlAttribute


mobileStyling : Element.Attribute msg
mobileStyling =
    Html.Attributes.class "wolfadex__elm-text-adventure__mobile" |> Element.htmlAttribute


customStyles : Element msg
customStyles =
    Element.html <|
        Html.node "style"
            []
            [ Html.text """
/*@media only screen and (max-width: 600px) {
  .wolfadex__elm-text-adventure__mobile.wolfadex__elm-text-adventure__main-view {
    max-width: 100%;
  }

  .wolfadex__elm-text-adventure__mobile > .t {
    font-size: 3rem !important;
  }
}*/

.wolfadex__elm-text-adventure__white-space_pre > .t {
  white-space: pre-wrap !important;
}""" ]
