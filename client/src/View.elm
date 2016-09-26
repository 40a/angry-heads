module View exposing (view)

import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (onClick)

import Model exposing (..)


view : Model -> Html Msg
view m =
  div []
    [ nav m
    , page m ]


nav : Model -> Html Msg
nav m =
  node "nav" [ class "navbar navbar-inverse navbar-fixed-top" ]
    <: container
       [ div [ class "navbar-header" ]
           [ node "button" [ type' "button"
                           , class "navbar-toggle collapsed"
                           , onClick ToggleNav
                           ]
               [ span [ class "sr-only" ] [ text "Toggle navigation" ]
               -- "hamburger" menu
               , span [ class "icon-bar"] []
               , span [ class "icon-bar"] []
               , span [ class "icon-bar"] []
               ]
           , a [ class "navbar-brand", href "#" ] <: text "Angry Heads"
           ]
       , div [ id "navbar"
             , classList [ ("navbar-collapse", True)
                         , ("collapse", True)
                         , ("in", m.menuIsVisible)
                         ]
             ]
           <: ul [ class "nav navbar-nav" ]
              [ li [ class "active" ] <: a [ href "#" ] <: text "Home"
              , li [] <: a [ href "#about" ] <: text "About"
              , li [] <: a [ href "#contact" ] <: text "Contact"
              ]
       ]


page : Model -> Html a
page m =
  container [
    h1 [] [ text m.message ],
    a [ href "https://hh.ru/oauth/authorize?response_type=code&client_id=UA6JURGKSH4MMV6H2VQ2SQ3NJUGOAQ136SPL8J5R9V2LDJR7UIAUJN65F73CIFJ8"
      , title "Аутентификация через Head Hunter"]
      [ img [ src "/img/hh-black.png" ]
        []
      ]
    ]

-- Helpers

infixr 9 <:
(<:) : (List a -> b) -> a -> b
(<:) n1 n2 = n1 <| [n2]

container : List (Html a) -> Html a
container = div [ class "container" ]
