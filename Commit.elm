module Commit (Model, update, init, view, Action) where

import Html exposing (Html, div, button, text, span)
import Html.Events exposing (onClick)
import Html.Attributes exposing (class)

type alias Model = { message: String, confirmed: Bool }

init : String -> Model
init message =
    {  message = message
    ,  confirmed = False
    }

type Action = QAConfirmed

update : Action -> Model -> Model
update action model =
    case action of
        QAConfirmed -> { model | confirmed <- True }

view : Signal.Address Action -> Model -> Html
view address commit =
    case commit.confirmed of
        False -> awaitingQAView address commit
        True -> confirmedView commit

confirmedView : Model -> Html
confirmedView commit =
    div [ class "commit" ]
        [ span [] [ text commit.message ]
        ]

awaitingQAView : Signal.Address Action -> Model -> Html
awaitingQAView address commit =
    div [ class "commit" ]
        [ span [] [ text commit.message ]
        , button [ onClick address QAConfirmed ] [ text "QA Confirmed" ]
        ]

