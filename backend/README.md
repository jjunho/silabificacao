# Syllable

Uma biblioteca Haskell para separação de palavras portuguesas em sílabas usando regras fonéticas.

## Status do Projeto ✅

- ✅ Separação automática de sílabas implementada
- ✅ API REST completa com Servant
- ✅ Interface de linha de comando funcional
- ✅ Tratamento de palavras compostas com hífen
- ✅ Ignora pontuação na entrada
- ✅ 100% cobertura de documentação Haddock
- ✅ Testes automatizados passando
- ✅ Build limpo sem warnings de compilação
- ✅ Código profissional e bem documentado

## Funcionalidades

- Separa automaticamente sílabas em português brasileiro
- Suporte a vogais acentuadas e nasalizadas
- Tratamento de grupos consonantais perfeitos (pr, br, tr, etc.)
- Sistema de exceções para palavras irregulares
- Tratamento de palavras compostas com hífen
- Ignora pontuação na entrada
- Interface de linha de comando e API REST

## Como usar

### Como biblioteca

```haskell
import Syllable.Core

case syllabify "palavra" of
    Right syllables -> print syllables
    Left error -> putStrLn $ "Erro: " ++ error
```

### Como executável

```bash
# Silabificar palavras
cabal run syllable-exe -- tecnologia "aero-espacial"

# Iniciar API REST
cabal run syllable-exe -- api
```

### Como backend REST

**Endpoint:** `POST /syllabify`

**Requisição:**

```json
{
  "text": "tecnologia aero-espacial, por exemplo!"
}
```

**Resposta:**

```json
{
  "results": [
    {
      "word": "tecnologia",
      "syllables": ["tec", "no", "lo", "gi", "a"]
    },
    {
      "word": "aero-espacial",
      "syllables": ["a", "e", "ro", "es", "pa", "ci", "al"]
    },
    {
      "word": "por",
      "syllables": ["por"]
    },
    {
      "word": "exemplo",
      "syllables": ["e", "xem", "plo"]
    }
  ]
}
```

A API ignora pontuação e processa todas as palavras do texto.

## Estrutura do projeto

- `src/Syllable/Core.hs` - Lógica principal de silabação
- `src/Syllable/API.hs` - API REST com Servant
- `app/Main.hs` - CLI e inicialização do backend
- `test/Spec.hs` - Testes automatizados

## Compilação e execução

```bash
cabal build
cabal test
cabal run syllable-exe -- "exemplo"
cabal run syllable-exe -- api
```

## Regras de silabação implementadas

1. **Princípio do Onset Máximo**: Consoantes intervocálicas preferem o onset da sílaba seguinte
2. **Preservação de Clusters**: Grupos consonantais válidos (pr, br, cl, etc.) são mantidos
3. **Tratamento de hífen**: Palavras compostas são silabificadas corretamente
4. **Hiato especial**: Ditongos ortográficos antes de "nh" são tratados como hiato

## Dependências

- Haskell com Cabal
- Servant para API REST
- Hspec para testes
