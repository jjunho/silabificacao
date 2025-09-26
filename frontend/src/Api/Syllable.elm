module Api.Syllable exposing (SyllabifyWord, syllabify)

import Http
import Json.Decode
import Json.Encode


type alias SyllabifyWord =
    { word : String
    , syllables : List String
    }


syllabify :
    { text : String
    , onResponse : Result Http.Error (List SyllabifyWord) -> msg
    }
    -> Cmd msg
syllabify options =
    let
        cleanText =
            String.trim options.text
    in
    Http.post
        { url = "/api/syllabify"
        , body = Http.jsonBody (encodeRequest cleanText)
        , expect = Http.expectJson options.onResponse decoder
        }


encodeRequest : String -> Json.Encode.Value
encodeRequest text =
    Json.Encode.object
        [ ( "text", Json.Encode.string text ) ]


decoder : Json.Decode.Decoder (List SyllabifyWord)
decoder =
    Json.Decode.field "results" (Json.Decode.list syllabifyWordDecoder)


syllabifyWordDecoder : Json.Decode.Decoder SyllabifyWord
syllabifyWordDecoder =
    Json.Decode.map2 SyllabifyWord
        (Json.Decode.field "word" Json.Decode.string)
        (Json.Decode.field "syllables" (Json.Decode.list Json.Decode.string))
