module Game.View exposing (program)

{-| The program for playing your game.

    import Game
    import Game.View

    {-| The most basic game possible
    -}
    main =
        Gma.makeGame "Sample Game"
            |> Game.view.program

@docs program

-}

import Browser
import Dict
import Element exposing (Element)
import Element.Background as Background
import Element.Border as Border
import Element.Font as Font
import Element.Input as Input
import Game
import Game.Internal exposing (Game, Item(..), Msg(..), RoomId(..), View(..))
import Html exposing (Html)
import Html.Attributes
import Set


{-| Starts and displays your game.
-}
program : Game -> Program () Game Msg
program game =
    Browser.element
        { init = \_ -> ( game, Cmd.none )
        , view = view
        , update = Game.Internal.update
        , subscriptions = \_ -> Sub.none
        }


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
    Element.column
        [ Element.centerX
        , Element.width
            (Element.fill
                |> Element.maximum (scaled 17)
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
        ]
        [ Element.wrappedRow
            [ Element.padding (scaled 1)
            , Element.spaceEvenly
            , Element.width Element.fill
            ]
            [ button "Describe Room" (SetView RoomDescription)
            , button "Search Room" (SetView RoomInventory)
            , button "Exits" (SetView RoomExits)
            , button "Inventory" (SetView PersonInventory)
            ]
        , spacer
        , game
            |> Game.Internal.getCurrentRoom
            |> .name
            |> Element.text
            |> Element.el
                [ Font.underline
                , Element.padding <| scaled 1
                ]
        , spacer
        , Element.html <|
            Html.node "style"
                []
                [ Html.text """ .wolfadex__elm-text-adventure__white-space_pre > .t {
  white-space: pre-wrap !important;
}""" ]
        , game
            |> .viewing
            |> (\v ->
                    case v of
                        RoomDescription ->
                            renderRoomDesc game

                        RoomExits ->
                            renderExits game

                        RoomInventory ->
                            renderRoomContents game

                        PersonInventory ->
                            renderInventory game
               )
        , spacer
        , renderGameLog game
        ]


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
            ]


renderExits : Game -> Element Msg
renderExits game =
    game
        |> Game.Internal.getCurrentRoom
        |> .connections
        |> List.map
            (\{ name, description, to, message } ->
                renderExit name description to message
            )
        |> Element.column
            [ Element.spacing (scaled 1)
            , Element.padding (scaled 1)
            ]


renderExit : String -> String -> RoomId -> String -> Element Msg
renderExit name desc to msg =
    Element.wrappedRow
        []
        [ button name (MoveRoom to msg)
        , buttonSpacer
        , Element.text desc
        ]


renderRoomContents : Game -> Element Msg
renderRoomContents game =
    game
        |> Game.Internal.getCurrentRoom
        |> .contents
        |> (\ids -> Dict.filter (\id _ -> Set.member id ids) game.items)
        |> Dict.toList
        |> List.map
            (\( id, item ) -> renderRoomItem id item)
        |> Element.column
            [ Element.spacing (scaled 1)
            , Element.padding (scaled 1)
            ]


renderRoomItem : Int -> Item -> Element Msg
renderRoomItem id item =
    let
        ( n, d ) =
            case item of
                Tool { name, description } ->
                    ( name, description )

                Container { name, description } ->
                    ( name, description )
    in
    Element.wrappedRow
        []
        [ button n (PickUpItem id)
        , buttonSpacer
        , Element.text d
        ]


renderInventory : Game -> Element Msg
renderInventory game =
    game
        |> .inventory
        |> (\ids -> Dict.filter (\id _ -> Set.member id ids) game.items)
        |> Dict.toList
        |> List.map
            (\( id, item ) -> renderInventoryItem id item)
        |> Element.column
            [ Element.spacing (scaled 1)
            , Element.padding (scaled 1)
            ]


renderInventoryItem : Int -> Item -> Element Msg
renderInventoryItem id item =
    let
        ( n, d ) =
            case item of
                Tool { name, description } ->
                    ( name, description )

                Container { name, description } ->
                    ( name, description )
    in
    Element.wrappedRow
        []
        [ button "Drop" (DropItem id)
        , buttonSpacer
        , button "Use" (UseItem id)
        , buttonSpacer
        , Element.text <| n ++ ":"
        , buttonSpacer
        , Element.text d
        ]


renderGameLog : Game -> Element Msg
renderGameLog game =
    game
        |> .log
        |> List.map (\log -> Element.paragraph [ whiteSpacePre ] [ Element.text log ])
        |> List.intersperse
            (Element.el
                [ Element.width Element.fill
                , Border.width 1
                , Border.color (Element.rgb 0.4 0.4 0.4)
                ]
                Element.none
            )
        |> Element.column
            [ Element.spacing <| scaled 1
            , Element.padding <| scaled 1
            , Element.scrollbarY
            ]


spacer : Element msg
spacer =
    Element.el
        [ Border.color (Element.rgb 0.07 0.07 0.07)
        , Border.width 3
        , Element.width Element.fill
        ]
        Element.none


button : String -> Msg -> Element Msg
button label action =
    Input.button
        [ Border.shadow
            { offset = ( 1, 1 )
            , size = 1
            , blur = 2

            --, color = Element.rgb 0.4 0.4 0.8
            , color = Element.rgba 1 1 1 0.5
            }
        , Element.paddingXY 3 1

        --, Background.color (Element.rgb 1 1 1)
        , Background.color (Element.rgb 0.4 0.4 0.8)

        --, Font.color (Element.rgb 0.2 0.2 0.2)
        ]
        { onPress = Just action
        , label = Element.text label
        }


buttonSpacer : Element msg
buttonSpacer =
    Element.el
        [ Element.width <| Element.px (scaled 1) ]
        Element.none


scaled : Int -> Int
scaled i =
    Element.modular 16 1.25 i |> floor


whiteSpacePre =
    Html.Attributes.class "wolfadex__elm-text-adventure__white-space_pre" |> Element.htmlAttribute
