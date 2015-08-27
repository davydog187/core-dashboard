import Html exposing (Html, div, h2, text)
import Html.Events exposing (onClick)
import Html.Attributes exposing (class, style)
import StartApp.Simple as StartApp
import Commit

main =
  StartApp.start { model = init, view = view, update = update }


type alias ID = Int

type alias CommitList =
    { commits: List (ID, Commit.Model)
    , nextID: ID
    }

type alias Model = CommitList

init : Model
init =
    { commits = [  (0, (Commit.init "[SB] (WEB-3598) Break production"))
                ,  (1, (Commit.init "[DL] (WEB-1234) Fix everything"))
                ,  (2, (Commit.init "[DL] (WEB-2935) Some feature"))
                ,  (3, (Commit.init "[DL] (WEB-4918) Erase track.js"))
                ,  (4, (Commit.init "[DL] (WEB-1259) Ads things"))
                ]
    , nextID = 5
    }

type Actions = Modify ID Commit.Action

update action model =
  case action of
    Modify id commitAction ->
      let updateCommit (commitID, commitModel) =
            if commitID == id
                then (commitID, Commit.update commitAction commitModel)
                else (commitID, commitModel)
      in
          { model | commits <- List.map updateCommit model.commits }

commitIsWaiting : (ID, Commit.Model) -> Bool
commitIsWaiting (_, commit) = commit.confirmed

view : Signal.Address Actions -> Model -> Html
view address model =
  let viewsForCommits commits = List.map (viewCommit address) commits
      (confirmed, waiting) = List.partition commitIsWaiting model.commits
  in
      div [ outerStyle ]
        [ div [ groupStyle "left", class "waiting-qa" ] ((title "waiting") :: viewsForCommits waiting)
        , div [ groupStyle "right", class "qa-confirmed" ] ((title "QA Confirmed") :: viewsForCommits confirmed)
        ]

title : String -> Html
title name = h2 [] [ text name ]

viewCommit : Signal.Address Actions -> (ID, Commit.Model) -> Html
viewCommit address (id, model) =
    Commit.view (Signal.forwardTo address (Modify id)) model

outerStyle =
    style
      [ ("margin", "0 auto")
      , ("width", "600px")
      , ("position", "relative")
      ]

groupStyle alignment =
    style
        [ ("position", "absolute")
        , (alignment, "0")
        ]
