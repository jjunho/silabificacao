module Pages.Home_ exposing (Model, Msg, page)

import Api
import Api.Syllable exposing (SyllabifyWord)
import Html exposing (Html)
import Html.Attributes exposing (class, disabled, placeholder, value)
import Html.Events exposing (onClick, onInput)
import Http
import Page exposing (Page)
import View exposing (View)


page : Page Model Msg
page =
    Page.element
        { init = init
        , update = update
        , subscriptions = subscriptions
        , view = view
        }



-- INIT


type alias Model =
    { inputText : String
    , syllabificationData : Api.Data (List SyllabifyWord)
    }


init : ( Model, Cmd Msg )
init =
    ( { inputText = ""
      , syllabificationData = Api.Success []
      }
    , Cmd.none
    )



-- UPDATE


type Msg
    = UpdateInputText String
    | SyllabifyText
    | SyllabifyApiResponded (Result Http.Error (List SyllabifyWord))


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        UpdateInputText text ->
            ( { model | inputText = text }
            , Cmd.none
            )

        SyllabifyText ->
            ( { model | syllabificationData = Api.Loading }
            , Api.Syllable.syllabify
                { text = model.inputText
                , onResponse = SyllabifyApiResponded
                }
            )

        SyllabifyApiResponded (Ok syllabifiedWords) ->
            ( { model | syllabificationData = Api.Success syllabifiedWords }
            , Cmd.none
            )

        SyllabifyApiResponded (Err httpError) ->
            ( { model | syllabificationData = Api.Failure httpError }
            , Cmd.none
            )



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions _ =
    Sub.none



-- VIEW


view : Model -> View Msg
view model =
    { title = "Separador de Sílabas"
    , body =
        [ Html.div [ class "hero is-primary py-6 has-text-centered" ]
            [ Html.h1 [ class "title is-1" ] [ Html.text "Separador de Sílabas" ]
            , Html.h2 [ class "subtitle is-4" ] [ Html.text "Separe palavras em sílabas automaticamente" ]
            ]
        , Html.div [ class "container py-6" ]
            [ Html.div [ class "columns is-centered" ]
                [ Html.div [ class "column is-half" ]
                    [ viewInputForm model
                    , viewResults model.syllabificationData
                    ]
                ]
            ]
        ]
    }


viewInputForm : Model -> Html Msg
viewInputForm model =
    Html.div [ class "box" ]
        [ Html.div [ class "field" ]
            [ Html.label [ class "label" ] [ Html.text "Digite o texto a ser silabificado:" ]
            , Html.div [ class "control" ]
                [ Html.textarea
                    [ class "textarea"
                    , placeholder "Exemplo: tecnologia aero-espacial"
                    , value model.inputText
                    , onInput UpdateInputText
                    ]
                    []
                ]
            ]
        , Html.div [ class "field" ]
            [ Html.div [ class "control" ]
                [ Html.button
                    [ class "button is-primary is-fullwidth"
                    , onClick SyllabifyText
                    , disabled (String.isEmpty (String.trim model.inputText))
                    ]
                    [ Html.text "Silabificar" ]
                ]
            ]
        ]


viewResults : Api.Data (List SyllabifyWord) -> Html Msg
viewResults syllabificationData =
    case syllabificationData of
        Api.Loading ->
            Html.div [ class "box has-text-centered" ]
                [ Html.div [ class "icon is-large" ]
                    [ Html.i [ class "fas fa-spinner fa-spin fa-2x" ] []
                    ]
                , Html.p [ class "mt-3" ] [ Html.text "Processando..." ]
                ]

        Api.Success syllabifiedWords ->
            if List.isEmpty syllabifiedWords then
                Html.div [ class "box has-text-centered" ]
                    [ Html.text "Digite um texto e clique em \"Silabificar\" para ver os resultados." ]

            else
                Html.div [ class "box" ]
                    [ Html.h3 [ class "title is-4" ] [ Html.text "Resultado:" ]
                    , Html.div [] (List.map viewSyllabifiedWord syllabifiedWords)
                    ]

        Api.Failure httpError ->
            let
                errorType =
                    case httpError of
                        Http.BadUrl url ->
                            "BadUrl: " ++ url

                        Http.Timeout ->
                            "Timeout"

                        Http.NetworkError ->
                            "NetworkError"

                        Http.BadStatus code ->
                            "BadStatus: " ++ String.fromInt code

                        Http.BadBody msg ->
                            "BadBody: " ++ msg
            in
            Html.div [ class "box has-text-centered has-background-danger-light" ]
                [ Html.div [ class "icon is-large has-text-danger" ]
                    [ Html.i [ class "fas fa-exclamation-triangle fa-2x" ] []
                    ]
                , Html.p [ class "mt-3 has-text-danger" ]
                    [ Html.text (Api.toUserFriendlyMessage httpError) ]
                , Html.p [ class "mt-2" ]
                    [ Html.text ("Origem: " ++ errorType) ]
                ]


viewSyllabifiedWord : SyllabifyWord -> Html Msg
viewSyllabifiedWord syllabifiedWord =
    Html.div [ class "mb-4" ]
        [ Html.div [ class "tags has-addons" ]
            [ Html.span [ class "tag is-dark" ] [ Html.text syllabifiedWord.word ]
            , Html.span [ class "tag is-primary" ]
                [ Html.text (String.join "-" syllabifiedWord.syllables) ]
            ]
        ]
