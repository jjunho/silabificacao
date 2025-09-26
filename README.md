# Syllable

Um biblioteca Haskell para separação de palavras portuguesas em sílabas usando regras fonéticas.

## Funcionalidades

- Separação automática de sílabas em português
- Suporte a vogais acentuadas
- Tratamento de grupos consonantais perfeitos (pr, br, tr, dr, cr, gr, fr, vr, pl, bl, cl, gl, fl)
- Sistema de exceções para palavras irregulares
- Interface de linha de comando

## Como usar

### Como biblioteca

```haskell
import Syllable.Core

-- Separar uma palavra em sílabas
case syllabify "palavra" of
    Right syllables -> print syllables
    Left error -> putStrLn $ "Erro: " ++ error
```

### Como executável

```bash
cabal run syllable-exe -- "palavra"
```

## Estrutura do projeto

- `src/Syllable/Core.hs` - Módulo principal com a lógica de silabação
- `app/Main.hs` - Interface de linha de comando
- `test/Spec.hs` - Testes automatizados

## Compilação e execução

### Compilar o projeto

```bash
cabal build
```

### Executar testes

```bash
cabal test
```

### Executar o programa

```bash
cabal run syllable-exe -- "exemplo"
```

## Exemplos

```bash
$ cabal run syllable-exe -- "casa"
Palavra: casa
Sílabas: ca sa
Total: 2 sílabas

$ cabal run syllable-exe -- "prato"
Palavra: prato
Sílabas: pra to
Total: 2 sílabas
```

## Regras implementadas

1. **V-CV**: Uma vogal seguida de consoante e vogal se separa antes da consoante
2. **V-CCV**: Uma vogal seguida de grupo consonantal perfeito se separa antes do grupo
3. **VC-CV**: Duas consoantes consecutivas se separam, ficando uma em cada sílaba

## Dependências

- base ^>=4.17.2.0
- containers ^>=0.6.7
- hspec ^>=2.10.0 (para testes)
