module Main exposing (Model, Msg(..), Team, init, main, update, view, viewTeam)

import Browser
import Html exposing (Html, div, h1, text)
import Html.Events
import Parser exposing ((|.), (|=))
import Parser.Extras
import Regex
import Result



---- MODEL ----


type alias Team =
    { url : String
    , name : String
    , matchups : List String
    , points : Int
    }


type alias Model =
    List Team


init : ( Model, Cmd Msg )
init =
    ( [], Cmd.none )



---- UPDATE ----


type Msg
    = NewHtml String


splitToTeamTag : String -> List String
splitToTeamTag =
    Regex.find (Regex.fromString "<tr.*?>(.*?)<\\/tr" |> Maybe.withDefault Regex.never)
        >> List.map (.submatches >> List.head >> Maybe.withDefault (Just "") >> Maybe.withDefault "")


parsePick : List String -> Parser.Parser (Parser.Step (List String) (List String))
parsePick list =
    Parser.succeed identity
        |. Parser.chompUntil "class="
        |. Parser.symbol "class=\""
        |= Parser.oneOf
            [ Parser.succeed ()
                |. Parser.symbol "sum"
                |> Parser.map (\_ -> Parser.Done (List.reverse list))
            , Parser.succeed (\pick -> Parser.Loop (pick :: list))
                |. Parser.chompUntil ">"
                |. Parser.symbol ">"
                |= Parser.getChompedString (Parser.chompUntil "</td>")
                |. Parser.symbol "</td>"
            ]


parseTeam : Parser.Parser Team
parseTeam =
    Parser.succeed Team
        |. Parser.chompUntil "href=\""
        |. Parser.symbol "href=\""
        |= Parser.getChompedString (Parser.chompUntil "\"")
        |. Parser.chompUntil ">"
        |. Parser.symbol ">"
        |= Parser.getChompedString (Parser.chompUntil "</a>")
        |. Parser.chompUntil "<td"
        |= (Parser.loop [] parsePick
                |> Parser.map
                    (List.map
                        (\pick ->
                            if pick == "&nbsp;" then
                                ""

                            else
                                pick
                        )
                    )
           )
        |. Parser.chompUntil "<strong>"
        |. Parser.symbol "<strong>"
        |= Parser.int


process : String -> List Team
process input =
    input
        |> String.split "Team Name"
        |> List.drop 1
        |> List.head
        |> Maybe.map splitToTeamTag
        |> Maybe.withDefault []
        |> List.map (Parser.run parseTeam)
        |> List.filterMap Result.toMaybe


update : Msg -> Model -> ( Model, Cmd Msg )
update msg _ =
    case msg of
        NewHtml input ->
            ( process input, Cmd.none )



---- VIEW ----


view : Model -> Html Msg
view model =
    div []
        [ Html.textarea [ Html.Events.onInput NewHtml ] []
        , h1 [] [ Html.text "Output" ]
        , Html.table []
            [ Html.thead [] <|
                [ Html.th [] [ text "Team" ]
                , Html.th [] [ text "Team url" ]
                , Html.th [] [ text "Points" ]
                , Html.th [] [ text "Picks" ]
                ]
                    ++ []
            , Html.tbody [] <| List.map viewTeam <| model
            ]
        ]


viewTeam : Team -> Html Msg
viewTeam team =
    Html.tr [] <|
        [ Html.td [] [ text team.name ]
        , Html.td [] [ text team.url ]
        , Html.td [] [ text <| String.fromInt team.points ]
        ]
            ++ List.map (Html.td [] << List.singleton << text) team.matchups



---- PROGRAM ----


main : Program () Model Msg
main =
    Browser.element
        { view = view
        , init = \_ -> init
        , update = update
        , subscriptions = always Sub.none
        }
