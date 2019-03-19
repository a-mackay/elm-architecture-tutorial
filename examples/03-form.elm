import Browser
import Char exposing (isUpper, isLower, isDigit)
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (onInput, onClick)
import List exposing (any, head, filter)
import Result exposing (andThen)
import String exposing (length, toList, toInt)

main =
  Browser.sandbox { init = init, update = update, view = view }

type alias Model =
  { name : String
  , password : String
  , passwordAgain : String
  , age : String
  , showValidationMessage : Bool
  }

init : Model
init =
  Model "" "" "" "" False


type Msg
  = Name String
  | Password String
  | PasswordAgain String
  | Age String
  | Submit


update : Msg -> Model -> Model
update msg model =
  case msg of
    Name name -> { model | name = name, showValidationMessage = False }
    Password password -> { model | password = password, showValidationMessage = False }
    PasswordAgain passwordAgain -> { model | passwordAgain = passwordAgain, showValidationMessage = False }
    Age age -> { model | age = age, showValidationMessage = False }
    Submit -> { model | showValidationMessage = True }


view : Model -> Html Msg
view model =
  div []
    [ viewInput "text" "Name" model.name Name
    , viewInput "password" "Password" model.password Password
    , viewInput "password" "Re-enter Password" model.passwordAgain PasswordAgain
    , viewInput "text" "Age in Years (an integer)" model.age Age
    , if model.showValidationMessage then viewValidation model else div [] []
    , button [ onClick Submit ] [ text "Submit" ]
    ]


viewInput : String -> String -> String -> (String -> msg) -> Html msg
viewInput t p v toMsg =
  input [ type_ t, placeholder p, value v, onInput toMsg ] []


viewValidation : Model -> Html msg
viewValidation model =
  case isModelValid model of
    Ok _ -> div [ style "color" "green" ] [ text "OK" ]
    Err errorMsg -> div [ style "color" "red" ] [ text errorMsg ]

isModelValid : Model -> Result String ()
isModelValid model =
  let minimumNumberOfCharacters = 5
      passwordsMatch = if model.password == model.passwordAgain
        then Ok ()
        else Err "Passwords don't match"
      passwordExceedsMinimumNumberOfCharacters = if String.length model.password >= minimumNumberOfCharacters
        then Ok ()
        else Err "Password too short"
      passwordChars = String.toList model.password
      containsUppercaseChar = if List.any (\char -> Char.isUpper char) passwordChars
        then Ok ()
        else Err "Need uppercase char"
      containsDigitChar = if List.any (\char -> Char.isDigit char) passwordChars
        then Ok ()
        else Err "Need digit char"
      containsLowercaseChar = if List.any (\char -> Char.isLower char) passwordChars
        then Ok ()
        else Err "Need lowercase char"
      ageIsAnInt = case String.toInt model.age of
        Just _ -> Ok ()
        Nothing -> Err "Age isn't an int"
      allErrors = filter (\result -> case result of
        Err _ -> True
        Ok _ -> False) [ passwordsMatch
                       , passwordExceedsMinimumNumberOfCharacters
                       , ageIsAnInt
                       , containsUppercaseChar
                       , containsDigitChar
                       , containsLowercaseChar
                       ]
  in (
    case head allErrors of
      Just errorResult -> errorResult
      Nothing -> Ok ()
  )
