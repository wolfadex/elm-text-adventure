module Game.View exposing (view)

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
import Element exposing (Element)
import Element.Background as Background
import Element.Border as Border
import Element.Font as Font
import Element.Input as Input
import Game.Internal exposing (Game, Id, Item(..), Mode(..), Msg(..), RoomId(..), View(..), Locked(..), Connection)
import Html exposing (Html)
import Html.Attributes
import Set exposing (Set)


view : Game -> Html Msg
view game =
    Element.layout
        [ Element.width Element.fill
        , Element.height Element.fill
        , Background.color <| Element.rgb 0.1 0.1 0.1
        ]
    <|
        viewGame game


viewGame : Game -> Element Msg
viewGame game =
    case game.mode of
        Running ->
            Element.column
                [ Element.centerX
                , Element.width
                    (Element.fill
                        |> Element.maximum (scaled 18)
                    )
                , Element.height Element.fill
                , Border.shadow
                    { offset = ( 2, 2 )
                    , size = 1
                    , blur = 8
                    , color = Element.rgba 0 0 0 0.5
                    }
                , Background.color <| Element.rgb 0.15 0.15 0.15
                , Font.color <| Element.rgb 0.8 0.8 0.8
                , mobileStyling
                , Html.Attributes.class "wolfadex__elm-text-adventure__main-view" |> Element.htmlAttribute
                ]
                [ customStyles
                , Element.wrappedRow
                    [ Element.padding (scaled 1)
                    , Element.spaceEvenly
                    , Element.width Element.fill
                    ]
                    [ button { label = "Describe Room", action = Just (SetView RoomDescription) }
                    , button { label = "Search Room", action = Just (SetView RoomInventory) }
                    , button { label = "Exits", action = Just (SetView RoomExits) }
                    , button { label = "Inventory", action = Just (SetView PersonInventory) }
                    ]
                , spacer
                , game
                    |> Game.Internal.getCurrentRoom
                    |> .name
                    |> Element.text
                    |> Element.el
                        [ Font.underline
                        , Element.padding <| scaled 1
                        , mobileStyling
                        ]
                , spacer
                , game
                    |> .viewing
                    |> (\v ->
                            case v of
                                RoomDescription ->
                                    renderRoomDesc game

                                RoomExits ->
                                    renderExits game

                                RoomInventory ->
                                    game
                                        |> Game.Internal.getCurrentRoom
                                        |> .contents
                                        |> renderItemList renderRoomItem game

                                PersonInventory ->
                                    game
                                        |> .inventory
                                        |> renderItemList renderPlayerItem game
                       )
                , spacer
                , renderGameLog game
                ]

        Finished ->
            Element.column
                [ Element.centerX
                , Element.width
                    (Element.fill
                        |> Element.maximum (scaled 18)
                    )
                , Element.height Element.fill
                , Border.shadow
                    { offset = ( 2, 2 )
                    , size = 1
                    , blur = 8
                    , color = Element.rgba 0 0 0 0.5
                    }
                , Background.color <| Element.rgb 0.15 0.15 0.15
                , Font.color <| Element.rgb 0.8 0.8 0.8
                , mobileStyling
                , Html.Attributes.class "wolfadex__elm-text-adventure__main-view" |> Element.htmlAttribute
                ]
                [ customStyles
                , renderGameLog game
                , Element.el
                    [ Element.padding (scaled 2) ]
                    (button { label = "Restart", action = Just Restart })
                ]

        Building ->
            Element.none


renderRoomDesc : Game -> Element Msg
renderRoomDesc game =
    game
        |> Game.Internal.getCurrentRoom
        |> .description
        |> Element.text
        |> List.singleton
        |> Element.paragraph
            [ Element.padding (scaled 1)
            , whiteSpacePre
            , mobileStyling
            ]


renderExits : Game -> Element Msg
renderExits game =
    game
        |> Game.Internal.getCurrentRoom
        |> .connections
        |> List.map renderExit
        |> Element.column
            [ Element.spacing (scaled 1)
            , Element.padding (scaled 1)
            ]


renderExit : Connection -> Element Msg
renderExit { name, description, to, locked, message } =
    Element.wrappedRow
        [ mobileStyling ]
        [ button
            { label = name
            , action =
                case locked of
                    Locked ->
                        Nothing

                    Unlocked ->
                        Just (MoveRoom to message)
            }
        , buttonSpacer
        , Element.el
            [ whiteSpacePre
            , Element.width Element.fill
            ]
            (Element.text description)
        ]


renderItemList : (Id -> ( String, String ) -> List (Element Msg)) -> Game -> Set Id -> Element Msg
renderItemList renderFn game idSet =
    idSet
        |> (\ids -> Dict.filter (\id _ -> Set.member id ids) game.items)
        |> Dict.toList
        |> List.map
            (\( id, item ) ->
                renderItemContainer id item renderFn
            )
        |> Element.column
            [ Element.spacing (scaled 1)
            , Element.padding (scaled 1)
            ]


renderItemContainer : Int -> Item -> (Int -> ( String, String ) -> List (Element Msg)) -> Element Msg
renderItemContainer id item renderFn =
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
        (renderFn id nameDesc)


renderRoomItem : Id -> ( String, String ) -> List (Element Msg)
renderRoomItem id ( n, d ) =
    [ button { label = n, action = Just (PickUpItem id) }
    , buttonSpacer
    , Element.text d
    ]


renderPlayerItem : Id -> ( String, String ) -> List (Element Msg)
renderPlayerItem id ( n, d ) =
    [ button { label = "Drop", action = Just (DropItem id) }
    , buttonSpacer
    , button { label = "Use", action = Just (UseItem id) }
    , buttonSpacer
    , Element.text <| n ++ ":"
    , buttonSpacer
    , Element.text d
    ]


renderGameLog : Game -> Element Msg
renderGameLog game =
    case game.mode of
        Running ->
            game.log
                |> List.map (\log -> Element.paragraph [ whiteSpacePre, mobileStyling ] [ Element.text log ])
                |> List.intersperse logSeparator
                |> Element.column
                    [ Element.spacing <| scaled 1
                    , Element.padding <| scaled 1
                    , Element.scrollbarY
                    ]

        Finished ->
            game.log
                |> List.take 2
                |> List.map (\log -> Element.paragraph [ whiteSpacePre, mobileStyling ] [ Element.text log ])
                |> List.intersperse logSeparator
                |> Element.column
                    [ Element.padding (scaled 1)
                    , Element.spacing (scaled 1)
                    ]

        Building ->
            Element.none


spacer : Element msg
spacer =
    Element.el
        [ Border.color (Element.rgb 0.07 0.07 0.07)
        , Border.width 3
        , Element.width Element.fill
        ]
        Element.none


button : { label : String, action : Maybe Msg } -> Element Msg
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
@media only screen and (max-width: 600px) {
  .wolfadex__elm-text-adventure__mobile.wolfadex__elm-text-adventure__main-view {
    max-width: 100%;
  }

  .wolfadex__elm-text-adventure__mobile > .t {
    font-size: 3rem !important;
  }
}

.wolfadex__elm-text-adventure__white-space_pre > .t {
  white-space: pre-wrap !important;
}""" ]
