import Html exposing (..)
import Html.App as Html
import Html.Attributes exposing (..)
import Html.Events exposing (onInput, onClick)
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
  , age : String
  , validate : Bool
  , password : String
  , passwordAgain : String
  }

model : Model
model =
  { name = ""
  , age = ""
  , validate = False
  , password = ""
  , passwordAgain = ""
  }


-- UPDATE

type Msg
    = Name String
    | Age String
    | Validate
    | Password String
    | PasswordAgain String

update : Msg -> Model -> Model
update msg model =
  case msg of
    Name name ->
      { model | name = name, validate = False }
    Age age ->
      { model | age = age, validate = False }
    Validate ->
      { model | validate = True }
    Password password ->
      { model | password = password, validate = False }
    PasswordAgain password ->
      { model | passwordAgain = password, validate = False }


-- VIEW

view : Model -> Html Msg
view model =
  div []
    [ input [ type' "text", placeholder "Name", onInput Name ] []
    , input [ type' "text", placeholder "Age", onInput Age ] []
    , input [ type' "password", placeholder "Password", onInput Password ] []
    , input [ type' "password", placeholder "Re-enter Password", onInput PasswordAgain ] []
    , button [ onClick Validate ] [ text "Validate" ]
    , viewValidation model
    ]

viewValidation : Model -> Html msg
viewValidation model =
  if model.validate then
    let
      (color, message) = modelValidation model.password model.passwordAgain model.age
    in
      div [ style [("color", color)] ] [ text message ]
  else
    div [] [ text "Wating to validate..." ]

modelValidation : String -> String -> String -> (String, String)
modelValidation password passwordAgain age =
  let
    isAllLowerCase x = (String.toLower x) == x
    hasUpperCase     = (not (isAllLowerCase password)) && (not (isAllLowerCase passwordAgain))
    isAllUpperCase x = (String.toUpper x) == x
    hasLowerCase     = (not (isAllUpperCase password)) && (not (isAllUpperCase passwordAgain))
    digits x         = String.filter isDigit x
    hasDigits        = (String.length (digits password) > 0) && (String.length (digits passwordAgain) > 0)
    ageLength        = (String.length (digits age))
    ageIsANumber     =  (ageLength == (String.length age)) && (ageLength > 0)
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
    else if not ageIsANumber then
      ("red", "Age must be a number.")
    else
      ("green", "OK")
