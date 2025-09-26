module Pages.Home_ exposing (Model, Msg, page)

{-|

    # Página principal do Separador de Sílabas

    Interface web para interação com a API de silabificação.
    - Formulário para entrada de texto
    - Botão para enviar requisição à API
    - Exibição dos resultados silábicos
    - Notificações de erro e carregamento

    @docs Model, Msg, page

-}

import Api
import Api.Syllable exposing (SyllabifyWord)
import Html exposing (Html)
import Html.Attributes exposing (class)
import Html.Events
import Http
import Page exposing (Page)
import View exposing (View)



-- INIT


{-| Modelo da página principal.
inputText: texto digitado pelo usuário
syllabificationData: estado da requisição (carregando, sucesso, erro)
-}
type alias Model =
    { inputText : String
    , syllabificationData : Api.Data (List SyllabifyWord)
    }


{-| Inicializa o modelo da página.
-}
init : ( Model, Cmd Msg )
init =
    ( { inputText = ""
      , syllabificationData = Api.Success []
      }
    , Cmd.none
    )



-- UPDATE


{-| Mensagens possíveis da página principal:
- UpdateInputText: atualiza o texto do formulário
- SyllabifyText: dispara requisição à API
- SyllabifyApiResponded: recebe resposta da API
-}
type Msg
    = UpdateInputText String
    | SyllabifyText
    | SyllabifyApiResponded (Result Http.Error (List SyllabifyWord))


{-| Atualiza o modelo de acordo com a mensagem recebida.
-}
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


{-| Subscrições da página (nenhuma).
-}
subscriptions : Model -> Sub Msg
subscriptions _ =
    Sub.none



-- VIEW


{-| Função principal da página, define ciclo de vida Elm Land.
-}
page : Page Model Msg
page =
    Page.element
        { init = init
        , update = update
        , subscriptions = subscriptions
        , view = view
        }


{-| Renderiza a página principal, incluindo cabeçalho, formulário e resultados.
-}
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


{-| Renderiza o formulário de entrada de texto e botão de silabificação.
-}
viewInputForm : Model -> Html Msg
viewInputForm model =
    Html.div [ class "box" ]
        [ Html.div [ class "field" ]
            [ Html.label [ class "label" ] [ Html.text "Digite o texto a ser silabificado:" ]
            , Html.div [ class "control" ]
                [ Html.textarea
                    [ class "textarea is-medium"
                    , Html.Attributes.placeholder "Exemplo: tecnologia aero-espacial"
                    , Html.Attributes.value model.inputText
                    , Html.Events.onInput UpdateInputText
                    ]
                    []
                ]
            ]
        , Html.div [ class "field" ]
            [ Html.div [ class "control" ]
                [ Html.button
                    [ class "button is-primary is-fullwidth"
                    , Html.Attributes.disabled (String.isEmpty (String.trim model.inputText))
                    , Html.Events.onClick SyllabifyText
                    ]
                    [ Html.text "Silabificar" ]
                ]
            ]
        ]


{-| Renderiza os resultados da silabificação, incluindo estados de carregamento, sucesso e erro.
-}
viewResults : Api.Data (List SyllabifyWord) -> Html Msg
viewResults syllabificationData =
    case syllabificationData of
        Api.Loading ->
            Html.div [ class "notification is-info has-text-centered" ] [ Html.text "Processando..." ]

        Api.Success syllabifiedWords ->
            if List.isEmpty syllabifiedWords then
                Html.div [ class "notification is-warning has-text-centered" ] [ Html.text "Digite um texto e clique em \"Silabificar\" para ver os resultados." ]

            else
                Html.div [ class "box" ]
                    [ Html.h3 [ class "title is-4" ] [ Html.text "Resultado:" ]
                    , Html.div [] (List.map viewSyllabifiedWord syllabifiedWords)
                    ]

        Api.Failure httpError ->
            Html.div [ class "notification is-danger has-text-centered" ] [ Html.text (Api.toUserFriendlyMessage httpError) ]


{-| Renderiza uma palavra silabificada como tags Bulma.
-}
viewSyllabifiedWord : SyllabifyWord -> Html Msg
viewSyllabifiedWord syllabifiedWord =
    Html.div [ class "mb-4" ]
        [ Html.div [ class "tags has-addons" ]
            [ Html.span [ class "tag is-dark" ] [ Html.text syllabifiedWord.word ]
            , Html.span [ class "tag is-primary" ]
                [ Html.text (String.join "-" syllabifiedWord.syllables) ]
            ]
        ]
