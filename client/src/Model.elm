module Model exposing (..)

import Http
import Json.Decode exposing (Decoder, object4, (:=), string)
import Task

type alias User =
    { id : String
    , firstName : String
    , middleName : String
    , lastName : String
    }

type alias Model =
    { menuIsVisible : Bool
    , message : String
    , user : Maybe User
    }


type Msg
    = ToggleNav
    | Authenticate
    | FetchSucceed User
    | FetchFail Http.Error

init : (Model, Cmd Msg)
init =
    (Model False "Hello, world!" Nothing, authenticate)

authenticate : Cmd Msg
authenticate =
  let url = "http://angry-heads.prog.msk.ru/api/v1/users/current"
  in Task.perform FetchFail FetchSucceed (Http.get getUser url)

getUser : Decoder User
getUser =
  object4 User
    ("id" := string)
    ("first_name" := string)
    ("middle_name" := string)
    ("last_name" := string)
