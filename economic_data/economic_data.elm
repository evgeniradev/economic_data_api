module Events exposing (..)

import Html exposing (..)
import Html.Events exposing (onClick)
import Http exposing (..)
import Json.Decode exposing (..)


main : Program Never Model Msg
main =
    program
        { init = init
        , view = view
        , update = update
        , subscriptions = \_ -> Sub.none
        }


init : ( Model, Cmd Msg )
init =
    ( Model [], Cmd.none )


type alias Model =
    { data : List Event }


view : Model -> Html Msg
view model =
    div []
        [ button [ onClick SendHttpRequest ]
            [ text "Get data from server" ]
        , viewEvents model.data
        ]


viewTableHeader : Html Msg
viewTableHeader =
    tr []
        [ th []
            [ text "Date" ]
        , th []
            [ text "Title" ]
        , th []
            [ text "Value" ]
        ]


viewEvents : List Event -> Html Msg
viewEvents events =
    div []
        [ h3 [] [ text "Pulls US Economic Data from an API." ]
        , table []
            ([ viewTableHeader ] ++ List.map viewEvent events)
        ]


viewEvent : Event -> Html Msg
viewEvent event =
    tr []
        [ td []
            [ text event.latest_value_date ]
        , td []
            [ text event.title ]
        , td []
            [ text (toString event.latest_value) ]
        ]


type Msg
    = SendHttpRequest
    | DataReceived (Result Http.Error (List Event))


type Error
    = BadUrl String
    | Timeout
    | NetworkError
    | BadStatus (Response String)
    | BadPayload String (Response String)


eventsDecoder : Json.Decode.Decoder Event
eventsDecoder =
    map3 Event
        (field "LatestValueDate" string)
        (field "Title" string)
        (field "LatestValue" float)


httpCommand : Cmd Msg
httpCommand =
    list eventsDecoder
        |> Http.get "https://api.tradingeconomics.com/country/united%20states/?client=guest:guest&format=json"
        |> Http.send DataReceived


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        SendHttpRequest ->
            ( model, httpCommand )

        DataReceived (Ok incoming_data) ->
            ( { model | data = incoming_data }, Cmd.none )

        DataReceived (Err httpError) ->
            ( model, Cmd.none )


type alias Event =
    { latest_value_date : String
    , title : String
    , latest_value : Float
    }
