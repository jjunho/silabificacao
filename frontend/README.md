# Frontend - Separador de Sílabas
>
> Built with [Elm Land](https://elm.land) 🌈

## Local development

```bash
# Requires Node.js v18+ (https://nodejs.org)
npx elm-land server
```

O frontend será executado em `http://localhost:1234`.

**Nota:** Certifique-se de que o backend Haskell também esteja rodando:

```bash
cd ../backend
cabal run syllable-exe -- api
```

O backend será executado em `http://localhost:8080` e o frontend fará proxy das requisições `/api` para ele.

## Funcionalidades

- Interface web para separação de sílabas
- Suporte a texto multilinha
- Exibição clara dos resultados com sílabas separadas por hífen
- Tratamento de erros de rede e API

## Deploying to production

Elm Land projects are most commonly deployed as static websites.

Please visit [the "Deployment" guide](https://elm.land/guide/deploying) to learn more
about deploying your app for free using Netlify or Vercel.
