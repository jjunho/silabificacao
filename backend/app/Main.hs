module Main where

import           Syllable.API       (runAPI)
import           Syllable.Core
import           System.Environment (getArgs)

-- | Converte uma Syllable em String
syllableToString :: Syllable -> String
syllableToString (Syllable phonemes) = concatMap phonemeToChar phonemes
  where
    phonemeToChar (Vowel c)     = [c]
    phonemeToChar (Consonant c) = [c]

-- | Processa e imprime a silabificação de uma palavra
processWord :: String -> IO ()
processWord word = do
  case syllabify word of
    Left err -> putStrLn $ "Erro em '" ++ word ++ "': " ++ err
    Right syllables -> do
      putStrLn $ "Palavra: " ++ word
      putStrLn $ "Sílabas: " ++ unwords (map syllableToString syllables)
      putStrLn $ "Total: " ++ show (length syllables) ++ " sílabas"
      putStrLn ""

-- | Função principal do executável
main :: IO ()
main = do
  args <- getArgs
  case args of
    [] -> do
      putStrLn "Uso: syllable-exe <palavras...> | api"
      putStrLn "Exemplo: syllable-exe tecnologia \"aero-espacial\""
      putStrLn "         syllable-exe api (inicia backend REST na porta 8080)"
    (cmd:rest) ->
      if cmd == "api"
        then do
          putStrLn "Iniciando servidor REST: http://localhost:8080"
          runAPI
        else do
          let wordList = cmd : rest
          mapM_ processWord wordList
