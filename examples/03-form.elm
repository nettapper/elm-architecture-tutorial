import Html exposing (..)
import Html.App as Html
import Html.Attributes exposing (..)
import Html.Events exposing (onInput)
import String exposing (length)
import Char exposing (isDigit)

main =
  Html.beginnerProgram
    { model = model
    , view = view
    , update = update
    }


-- MODEL

type alias Model =
  { name : String
  , password : String
  , passwordAgain : String
  }

model : Model
model =
  Model "" "" ""


-- UPDATE

type Msg
    = Name String
    | Password String
    | PasswordAgain String

update : Msg -> Model -> Model
update msg model =
  case msg of
    Name name ->
      { model | name = name }
    Password password ->
      { model | password = password }
    PasswordAgain password ->
      { model | passwordAgain = password }


-- VIEW

view : Model -> Html Msg
view model =
  div []
    [ input [ type' "text", placeholder "Name", onInput Name ] []
    , input [ type' "password", placeholder "Password", onInput Password ] []
    , input [ type' "password", placeholder "Re-enter Password", onInput PasswordAgain ] []
    , viewValidation model
    ]

viewValidation : Model -> Html msg
viewValidation model =
  let
    (color, message) = passwordValidation model.password model.passwordAgain
  in
    div [ style [("color", color)] ] [ text message ]

passwordValidation : String -> String -> (String, String)
passwordValidation password passwordAgain =
  let
    isAllLowerCase x = (String.toLower x) == x
    hasUpperCase     = (not (isAllLowerCase password)) && (not (isAllLowerCase passwordAgain))
    isAllUpperCase x = (String.toUpper x) == x
    hasLowerCase     = (not (isAllUpperCase password)) && (not (isAllUpperCase passwordAgain))
    digits x         = String.filter isDigit x
    hasDigits        = (String.length (digits password) > 0) && (String.length (digits passwordAgain) > 0)
  in
    if password /= passwordAgain then
      ("red", "Passwords do not match!")
    else if Basics.max (String.length password) (String.length passwordAgain) < 8 then
      ("red", "Passwords must be longer than 8 characters!")
    else if not hasUpperCase then
      ("red", "Passwords must have upercase letters.")
    else if not hasLowerCase then
      ("red", "Passwords must have lowercase letters.")
    else if not hasDigits then
      ("red", "Passwords must have digits.")
    else
      ("green", "OK")

-- ("red", "Passwords must have upercase letters, lower case letters and numeric characters.")
