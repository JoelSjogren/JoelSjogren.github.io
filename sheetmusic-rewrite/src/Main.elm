module Main exposing (..)

-- List the available songs.
--

import Browser exposing (Document)
import Html exposing (..)--(Html, text, pre, h1, h2, div, img, hr)
import Html.Attributes exposing (..) --(href, src, width)
import Http

import List exposing (map,head)
import String exposing (..)

import RBY
import GSC
import Protocol exposing (Metadata,removeHeader)

-- MAIN


main =
  Browser.document --application --document --element
    { init = init
    , view = view
    , update = update
    , subscriptions = (\_ -> Sub.none)
--    , onUrlChange = UrlChanged
--    , onrlRequest = LinkClicked
    }



-- MODEL

type alias Model
  = { rby : Maybe Metadata
    , gsc : Maybe Metadata }

init : () -> (Model, Cmd Msg)
init _ =
  ( { rby = Nothing, gsc = Nothing }
  , Cmd.batch [
         Http.get
             { url = "../data/rby/audio.asm"
             , expect = Http.expectString GotRBY
             },
             Http.get
             { url = "../data/gsc/audio.asm"
             , expect = Http.expectString GotGSC
             }
        ]
  )
  

-- UPDATE

type Msg
 = GotRBY (Result Http.Error String)
 | GotGSC (Result Http.Error String)

update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
 case msg of
    GotRBY result -> case result of
      Ok fullText -> ({model | rby = Just (RBY.parseMetadata (removeHeader fullText))}, Cmd.none)
      Err _ -> Debug.todo "rby failed" --(model, Cmd.none)
    GotGSC result -> case result of
      Ok fullText -> ({model | gsc = Just (GSC.parseMetadata (removeHeader fullText))}, Cmd.none)
      Err _ -> Debug.todo "gsc failed" --(model, Cmd.none)

-- VIEW


view : Model -> Document Msg --Html Msg
view {rby, gsc} =
  { title = "Sheetmusic"
  , body = [-- h1 [] [ text "Assembly Sheet Music" ]
             showMetadata gsc
           , showMetadata rby
           ]
  }

showMetadata : Maybe Metadata -> Html Msg
showMetadata game =
  case game of
    Nothing -> div [] []
    Just { title, covers, songs } ->
      div [] [-- h2 [] [ text title ]
               div [] (List.map showCover covers)
             , div [ class "pieces" ] (List.map showSong songs)
--             , hr [] []
             ]

showCover : String -> Html Msg
showCover path = img [ src path, width 250 ] []
          
showSong : {name : String, thumb : String, addr : String} -> Html Msg
showSong { name, thumb, addr } = div [] [ img [ src thumb ] []
                                         , Html.a [ href addr ] [ text name ]
                                        ]
















