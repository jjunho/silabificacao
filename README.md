# Separador de Sílabas

Um projeto completo para separação automática de sílabas em português, implementado com Haskell no backend e Elm no frontend.

## Arquitetura

- **Backend (Haskell)**: API REST que implementa algoritmos de silabificação baseados em regras fonéticas portuguesas
- **Frontend (Elm)**: Interface web responsiva para interação com a API

## Funcionalidades

- ✅ Separação automática de sílabas baseada em regras fonéticas
- ✅ Suporte a palavras compostas e hiatos
- ✅ API REST para integração
- ✅ Interface web moderna e responsiva
- ✅ Tratamento robusto de erros
- ✅ Testes automatizados

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

### Testando a API

```bash
curl -X POST http://localhost:8080/syllabify \
  -H "Content-Type: application/json" \
  -d '{"text": "tecnologia aero-espacial"}'
```

### Usando a CLI

```bash
cd backend
cabal run syllable-exe -- "tecnologia aero-espacial"
```

## Exemplos de uso

| Palavra | Sílabas |
|---------|---------|
| tecnologia | tec-no-lo-gi-a |
| aero-espacial | a-e-ro-es-pa-cial |
| hiato | hi-a-to |
| computador | com-pu-ta-dor |

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

## Algoritmo de Silabificação

O algoritmo implementa regras fonéticas portuguesas incluindo:

- **Sílaba aberta/fechada**: Baseada na posição das vogais
- **Encontros consonantais**: Regras para grupos de consoantes
- **Hiatos e ditongos**: Tratamento especial para sequências vocálicas
- **Palavras compostas**: Separação correta em hífen

## Estrutura do Projeto

```
syllable/
├── backend/           # API Haskell
│   ├── src/Syllable/  # Código fonte
│   ├── test/          # Testes
│   └── syllable.cabal # Configuração Cabal
├── frontend/          # Interface Elm
│   ├── src/           # Código Elm
│   └── elm-land.json  # Configuração Elm Land
└── .github/           # Documentação
```

## Contribuição

1. Fork o projeto
2. Crie uma branch para sua feature (`git checkout -b feature/nova-feature`)
3. Commit suas mudanças (`git commit -am 'Adiciona nova feature'`)
4. Push para a branch (`git push origin feature/nova-feature`)
5. Abra um Pull Request

## Licença

Este projeto está sob a licença MIT. Veja o arquivo `LICENSE` para mais detalhes.
