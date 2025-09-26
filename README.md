# Separador de Sílabas

Um projeto completo para separação automática de sílabas em português, implementado com Haskell no backend e Elm no frontend, utilizando Bulma CSS via Elm-Bulma para uma interface moderna e responsiva.

## Arquitetura

- **Backend (Haskell)**: API REST implementada com Servant, algoritmos de silabificação baseados em regras fonéticas portuguesas, testes automatizados e CLI.
- **Frontend (Elm + Elm Land + Elm-Bulma)**: SPA responsiva, integração REST, componentes visuais Bulma, tratamento de erros, layout adaptativo e UX aprimorada.

## Funcionalidades

- ✅ Separação automática de sílabas baseada em regras fonéticas
- ✅ Suporte a palavras compostas, hiatos e ditongos
- ✅ API REST para integração
- ✅ Interface web moderna, responsiva e acessível (Bulma CSS)
- ✅ Componentes Elm-Bulma para formulário, botões, notificações e layout
- ✅ Tratamento robusto de erros e feedback visual
- ✅ Testes automatizados (backend)

## Como executar

### Pré-requisitos

- Haskell/Cabal (para o backend)
- Node.js v18+ (para o frontend)
- Elm (instalado automaticamente pelo elm-land)

### Executando o projeto completo

1. **Backend:**

   ```bash
   cd backend
   cabal run syllable-exe -- api
   ```

   O backend ficará disponível em `http://localhost:8080`

2. **Frontend:**

   ```bash
   cd frontend
   npx elm-land server
   ```

   O frontend ficará disponível em `http://localhost:1234`

   > **Dica:** O frontend utiliza Elm-Bulma para estilização. Os componentes visuais (formulário, botões, notificações) são implementados com classes Bulma e helpers Elm-Bulma, garantindo responsividade e visual limpo.

### Testando a API

```bash
curl -X POST http://localhost:8080/syllabify \
   -H "Content-Type: application/json" \
   -d '{"text": "tecnologia aero-espacial"}'
```

### Usando a interface web

Acesse `http://localhost:1234` e utilize o formulário para digitar palavras ou frases. O resultado é exibido com separação silábica, usando tags Bulma para destaque visual. Notificações de erro e carregamento são exibidas com cores e layout Bulma.

### Usando a CLI

```bash
cd backend
cabal run syllable-exe -- "tecnologia aero-espacial"
```

## Exemplos de uso

| Palavra       | Sílabas            |
| ------------- | ------------------ |
| tecnologia    | tec-no-lo-gi-a     |
| aero-espacial | a-e-ro-es-pa-cial  |
| hiato         | hi-a-to            |
| computador    | com-pu-ta-dor      |
| silabificação | si-la-bi-fi-ca-ção |
| automação     | au-to-ma-ção       |

## Desenvolvimento

### Backend

```bash
cd backend
cabal build    # Compilar
cabal test     # Executar testes
cabal run      # Executar aplicação
```

### Frontend

```bash
cd frontend
npx elm-land server    # Servidor de desenvolvimento
npx elm-land build     # Build para produção
```

#### Desenvolvimento visual

- O frontend utiliza Elm-Bulma (surprisetalk/elm-bulma) para componentes visuais.
- Para criar novos componentes, use helpers Bulma ou classes Bulma diretamente:

```elm
Html.div [ class "notification is-info" ] [ Html.text "Mensagem" ]
Html.button [ class "button is-primary" ] [ Html.text "Enviar" ]
Html.textarea [ class "textarea" ] []
```

- Notificações, botões e formulários seguem o padrão Bulma para responsividade e acessibilidade.
- Para personalizar, consulte a [documentação Bulma](https://bulma.io/documentation/) e [Elm-Bulma](https://package.elm-lang.org/packages/surprisetalk/elm-bulma/latest/).

## Algoritmo de Silabificação

O algoritmo implementa regras fonéticas portuguesas incluindo:

- **Sílaba aberta/fechada**: Baseada na posição das vogais
- **Encontros consonantais**: Regras para grupos de consoantes
- **Hiatos e ditongos**: Tratamento especial para sequências vocálicas
- **Palavras compostas**: Separação correta em hífen
- **Casos especiais**: Hiato antes de NH, grupos consonantais complexos, exceções fonéticas

## Estrutura do Projeto

```
syllable/
├── backend/           # API Haskell
│   ├── src/Syllable/  # Código fonte
│   ├── test/          # Testes
│   └── syllable.cabal # Configuração Cabal
│   └── dist-newstyle/build/x86_64-linux/ghc-9.10.1/syllable-0.1.0.0/doc/html/syllable/index.html # Haddock
├── frontend/          # Interface Elm
│   ├── src/           # Código Elm
│   ├── elm-land.json  # Configuração Elm Land
│   └── elm.json       # Dependências Elm
└── .github/           # Documentação
```

## Documentação

- [Documentação Haddock do backend (Haskell)](backend/dist-newstyle/build/x86_64-linux/ghc-9.10.1/syllable-0.1.0.0/doc/html/syllable/index.html)

## Contribuição

1. Fork o projeto
2. Crie uma branch para sua feature (`git checkout -b feature/nova-feature`)
3. Commit suas mudanças (`git commit -am 'Adiciona nova feature'`)
4. Push para a branch (`git push origin feature/nova-feature`)
5. Abra um Pull Request

### Dicas para contribuir no frontend

- Use componentes Bulma para manter o padrão visual
- Teste responsividade e acessibilidade
- Documente novos componentes e páginas
- Siga o padrão Elm Land para rotas e páginas

## Licença

Este projeto está sob a licença MIT. Veja o arquivo `LICENSE` para mais detalhes.
