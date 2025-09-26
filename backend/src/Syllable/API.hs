{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators     #-}

module Syllable.API where

import           Data.Aeson                           (FromJSON, ToJSON)
import           Data.Char                            (isAlpha)
import           GHC.Generics                         (Generic)
import           Network.Wai
import           Network.Wai.Handler.Warp             (run)
import           Network.Wai.Middleware.RequestLogger (logStdoutDev)
import           Servant
import           Syllable.Core

-- | Tipo para requisição da API
newtype SyllabifyRequest = SyllabifyRequest
  { text :: String
  } deriving (Show, Generic)

instance FromJSON SyllabifyRequest

-- | Tipo para uma palavra silabificada
data SyllabifyWord = SyllabifyWord
  { word      :: String -- ^ A palavra original
  , syllables :: [String] -- ^ Lista de sílabas
  } deriving (Show, Generic)

instance ToJSON SyllabifyWord

-- | Tipo para resposta da API
newtype SyllabifyResponse = SyllabifyResponse
  { results :: [SyllabifyWord] -- ^ Lista de palavras silabificadas
  } deriving (Show, Generic)

instance ToJSON SyllabifyResponse

-- | Definição da API REST
-- POST /syllabify { "text": "tecnologia aero-espacial" } => { "results": [{"word": "tecnologia", "syllables": ["tec","no","lo","gi","a"]}, ...] }
type API
  = "syllabify" :> ReqBody '[ JSON] SyllabifyRequest :> Post
      '[ JSON]
      SyllabifyResponse :<|> "api" :> "syllabify" :> ReqBody
      '[ JSON]
      SyllabifyRequest :> Post '[ JSON] SyllabifyResponse

-- | Servidor da API
server :: Server API
server = syllabifyHandler :<|> syllabifyHandler

-- | Handler principal da API que processa a requisição de silabificação
syllabifyHandler :: SyllabifyRequest -> Handler SyllabifyResponse
syllabifyHandler (SyllabifyRequest t) =
  let wordList = tokenizeWords t
      syllabifiedResults = map processWord wordList
   in return $ SyllabifyResponse syllabifiedResults

-- | Processa uma palavra e retorna um SyllabifyWord com sílabas
processWord :: String -> SyllabifyWord
processWord w =
  case syllabify w of
    Right ss -> SyllabifyWord w (map showSyllable ss)
    Left _   -> SyllabifyWord w []

-- | Tokeniza string em palavras, ignorando pontuação
tokenizeWords :: String -> [String]
tokenizeWords =
  words
    . map
        (\c ->
           if isAlpha c || c == '-'
             then c
             else ' ')

-- | Converte uma Syllable em String
showSyllable :: Syllable -> String
showSyllable (Syllable ps) = map phonemeChar ps
  where
    phonemeChar (Vowel c)     = c
    phonemeChar (Consonant c) = c

-- | Aplicação WAI da API
app :: Application
app = logStdoutDev $ serve (Proxy :: Proxy API) server

-- | Função para iniciar o servidor REST na porta 8080
runAPI :: IO ()
runAPI = run 8080 app
