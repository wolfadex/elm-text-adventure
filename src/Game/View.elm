module Game.View exposing (view)

import Element exposing (Element)
import Element.Background as Background
import Element.Border as Border
import Element.Font as Font
import Element.Input as Input
import Game exposing (Game, Msg(..))
import Html exposing (Html)


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
        , Element.padding <| scaled 3
        , Element.width
            (Element.fill
                |> Element.maximum (scaled 17)
            )
        , Border.shadow
            { offset = ( 2, 2 )
            , size = 1
            , blur = 8
            , color = Element.rgba 0 0 0 0.5
            }
        , Background.color <| Element.rgb 0.15 0.15 0.15
        , Font.color <| Element.rgb 0.8 0.8 0.8
        , Element.spacingXY 0 (scaled 3)
        ]
        [ game
            |> Game.getCurrentRoom
            |> Game.thingName
            |> Element.text
            |> Element.el
                [ Font.underline ]
        , game
            |> .currentDescription
            |> Element.text
            |> List.singleton
            |> Element.paragraph []
        , Element.wrappedRow
            [ Element.spacing <| scaled 1 ]
            [ button "Describe Room" DescribeRoom
            , button "List Inventory" ListInventory
            , button "Items in Room" ItemsInRoom
            ]
        ]


button : String -> Msg -> Element Msg
button label action =
    Input.button
        []
        { onPress = Just action
        , label = Element.text ("[ " ++ label ++ " ]")
        }


scaled : Int -> Int
scaled i =
    Element.modular 16 1.25 i |> floor
