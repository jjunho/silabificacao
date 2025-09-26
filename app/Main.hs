module Main where

import           Syllable.Core
import           System.Environment (getArgs)

-- | Converte uma sílaba para string
syllableToString :: Syllable -> String
syllableToString (Syllable phonemes) = concatMap phonemeToChar phonemes
  where
    phonemeToChar (Vowel c)     = [c]
    phonemeToChar (Consonant c) = [c]

-- | Função principal que processa argumentos da linha de comando
main :: IO ()
main = do
  args <- getArgs
  case args of
    [] -> do
      putStrLn "Uso: syllable-exe <palavra>"
      putStrLn "Exemplo: syllable-exe \"palavra\""
    (word:_) -> do
      case syllabify word of
        Left err -> putStrLn $ "Erro: " ++ err
        Right syllables -> do
          putStrLn $ "Palavra: " ++ word
          putStrLn $ "Sílabas: " ++ unwords (map syllableToString syllables)
          putStrLn $ "Total: " ++ show (length syllables) ++ " sílabas"
