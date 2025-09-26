module Api exposing
    ( Data(..)
    , toUserFriendlyMessage
    )

import Http


type Data value
    = Loading
    | Success value
    | Failure Http.Error


toUserFriendlyMessage : Http.Error -> String
toUserFriendlyMessage httpError =
    case httpError of
        Http.BadUrl _ ->
            "URL inválida"

        Http.Timeout ->
            "Tempo limite da requisição excedido"

        Http.NetworkError ->
            "Erro de conexão com o servidor"

        Http.BadStatus code ->
            if code == 404 then
                "Recurso não encontrado"

            else
                "Erro do servidor: " ++ String.fromInt code

        Http.BadBody _ ->
            "Resposta inesperada do servidor"
