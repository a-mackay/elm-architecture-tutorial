import Browser
import Char exposing (isUpper, isLower, isDigit)
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (onInput, onClick)
import List exposing (any)
import String exposing (length, toList, toInt)

main =
  Browser.sandbox { init = init, update = update, view = view }

type alias Model =
  { name : String
  , password : String
  , passwordAgain : String
  , age : String
  }

init : Model
init =
  Model "" "" "" ""


type Msg
  = Name String
  | Password String
  | PasswordAgain String
  | Age String
  | DoNothing


update : Msg -> Model -> Model
update msg model =
  case msg of
    Name name ->
      { model | name = name }

    Password password ->
      { model | password = password }

    PasswordAgain password ->
      { model | passwordAgain = password }

    Age age ->
      { model | age = age }

    DoNothing -> model


view : Model -> Html Msg
view model =
  div []
    [ viewInput "text" "Name" model.name Name
    , viewInput "password" "Password" model.password Password
    , viewInput "password" "Re-enter Password" model.passwordAgain PasswordAgain
    , viewInput "text" "Age in Years (an integer)" model.age Age
    , viewValidation model
    , button [ onClick DoNothing ] [ text "Submit" ]
    ]


viewInput : String -> String -> String -> (String -> msg) -> Html msg
viewInput t p v toMsg =
  input [ type_ t, placeholder p, value v, onInput toMsg ] []


viewValidation : Model -> Html msg
viewValidation model =
  if isModelValid model then
    div [ style "color" "green" ] [ text "OK" ]
  else
    div [ style "color" "red" ] [ text "Model does not pass validation!" ]

isModelValid : Model -> Bool
isModelValid model =
  let minimumNumberOfCharacters = 5
      passwordsMatch = model.password == model.passwordAgain
      passwordExceedsMinimumNumberOfCharacters = String.length model.password >= minimumNumberOfCharacters
      passwordChars = String.toList model.password
      containsUppercaseChar = List.any (\char -> Char.isUpper char) passwordChars
      containsDigitChar = List.any (\char -> Char.isDigit char) passwordChars
      containsLowercaseChar = List.any (\char -> Char.isLower char) passwordChars
      ageIsAnInt = case String.toInt model.age of
        Just _ -> True
        Nothing -> False
  in (
    passwordsMatch
    && passwordExceedsMinimumNumberOfCharacters
    && ageIsAnInt
    && containsUppercaseChar
    && containsDigitChar
    && containsLowercaseChar
  )