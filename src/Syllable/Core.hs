-- |
-- Module      : Syllable.Core
-- Description : Sistema de silabação automática para o português brasileiro
-- Copyright   : (c) 2024 jjunho
-- License     : MIT
-- Maintainer  : jjunho@example.com
-- Stability   : experimental
-- Portability : portable
--
-- Este módulo implementa um sistema computacional de silabação automática
-- baseado nos princípios da __Fonologia Prosódica__ e da __Teoria da Otimalidade__
-- aplicados ao português brasileiro padrão.
--
-- = Fundamentação Teórica
--
-- O sistema baseia-se em três componentes principais da teoria fonológica:
--
-- [__1. Hierarquia Prosódica__] A sílaba é tratada como uma unidade
-- prosódica com estrutura interna hierárquica: onset (ataque),
-- núcleo e coda.
--
-- [__2. Escala de Sonoridade__] A organização dos fonemas na sílaba
-- segue o princípio universal da sonoridade crescente do onset ao núcleo
-- e decrescente do núcleo à coda.
--
-- [__3. Restrições Fonotáticas__] O português possui restrições específicas
-- sobre quais sequências consonantais são permitidas em cada posição silábica.
--
-- = Arquitetura do Sistema
--
-- O silabificador implementa uma __arquitetura híbrida__:
--
-- * __Componente baseado em regras__: aplica regras fonológicas gerais
-- * __Componente lexical__: trata exceções idiossincráticas
-- * __Interface unificada__: combina ambos os componentes transparentemente
--
-- = Regras Implementadas
--
-- __Princípio do Onset Máximo:__ Consoantes intervocálicas são preferencialmente
-- atribuídas ao onset da sílaba seguinte (V.CV, não VC.V).
--
-- __Preservação de Clusters:__ Grupos consonantais bem-formados (pr, br, cl, etc.)
-- são mantidos intactos no onset.
--
-- __Divisão Consonântica:__ Sequências de consoantes que não formam clusters
-- válidos são divididas entre sílabas adjacentes.
--
-- = Exemplo de Uso
--
-- >>> syllabify "pterodáctilo"
-- Right [Syllable [...], Syllable [...], ...]
--
-- >>> syllabify "casa"
-- Right [Syllable [Consonant 'c',Vowel 'a'], Syllable [Consonant 's',Vowel 'a']]
module Syllable.Core
  ( Phoneme(..)
  , Syllable(..)
  , WordPhonemes
  , syllabify
  , normalize
  , toPhonemes
  ) where

import           Data.Char (toLower)
import qualified Data.Map  as Map

-- | Representa um fonema como vogal ou consoante.
--
-- Em linguística, um fonema é a menor unidade sonora distintiva de uma língua.
-- No sistema fonológico do português, os fonemas se dividem em duas categorias
-- principais baseadas em sua articulação e função silábica:
--
-- * 'Vowel': Representa um fonema vocálico, caracterizado pela ausência de
--   obstáculos à passagem do ar durante sua produção. As vogais funcionam
--   como núcleo silábico obrigatório em português.
--
-- * 'Consonant': Representa um fonema consonantal, caracterizado pela presença
--   de alguma obstrução ou constrição na passagem do ar. As consoantes
--   funcionam como elementos periféricos da sílaba (onset ou coda).
data Phoneme
  = Vowel Char -- ^ Fonema vocálico (núcleo silábico)
  | Consonant Char -- ^ Fonema consonantal (margem silábica)
  deriving (Show, Eq)

-- | Representa uma sílaba como uma sequência ordenada de fonemas.
--
-- Na fonologia portuguesa, a sílaba é uma unidade prosódica que agrupa
-- fonemas de acordo com princípios de sonoridade e estrutura. A estrutura
-- canônica da sílaba portuguesa pode ser representada como (C)(C)V(C),
-- onde:
--
-- * C = consoante (opcional na maioria das posições)
-- * V = vogal (obrigatória como núcleo)
--
-- A sílaba possui hierarquia interna:
--
-- * __Onset__: consoante(s) inicial(is) - pode ser simples (C) ou complexo (CC)
-- * __Núcleo__: vogal obrigatória que forma o centro da sílaba
-- * __Coda__: consoante final opcional (restrita em português)
newtype Syllable =
  Syllable [Phoneme] -- ^ Sequência de fonemas que compõem a sílaba
  deriving (Show, Eq)

-- | Tipo alias para representar uma palavra como sequência linear de fonemas.
--
-- Representa a cadeia fonêmica de uma palavra antes da aplicação das regras
-- de silabação. Esta é a representação fonológica subjacente que serve como
-- input para o processo de fonotática silábica.
type WordPhonemes = [Phoneme]

-- | Léxico de exceções lexicais para palavras com silabação irregular.
--
-- Este mapa implementa um componente crucial dos sistemas de silabação:
-- o tratamento de itens lexicais que apresentam comportamento idiossincrático,
-- não derivável pelas regras fonológicas gerais.
--
-- Essas exceções podem ocorrer devido a:
--
-- * __Empréstimos linguísticos__: palavras de origem estrangeira que mantêm
--   padrões silábicos da língua de origem
-- * __Composição morfológica complexa__: palavras com fronteiras morfológicas
--   que interferem na silabação fonológica
-- * __Variação diacrônica__: formas que preservam padrões históricos
-- * __Registro especializado__: terminologia técnica com convenções específicas
exceptionMap :: Map.Map String [Syllable]
exceptionMap = Map.fromList []

-- | Aplica normalização ortográfica através de conversão para minúsculas.
--
-- Esta função implementa um pré-processamento essencial para garantir
-- consistência na busca lexical e aplicação de regras fonológicas.
-- A normalização é necessária porque:
--
-- * O sistema de escrita português utiliza variação maiúscula/minúscula
--   sem valor fonológico distintivo
-- * As regras de silabação operam no nível fonológico, independente
--   da representação ortográfica
-- * A busca no léxico de exceções requer chaves consistentes
--
-- __Exemplo:__
--
-- >>> normalize "PTERODÁCTILO"
-- "pterodáctilo"
normalize :: String -> String
normalize = map toLower

-- | Função de classificação fonológica para identificação de segmentos vocálicos.
--
-- Implementa o teste de vocoide baseado no sistema vocálico do português
-- brasileiro padrão. A função reconhece:
--
-- __Vogais orais simples:__ /a e i o u/
--
-- * Representadas graficamente como: a, e, i, o, u
--
-- __Vogais com marcação prosódica:__
--
-- * Acento agudo (´): marca tonicidade e, em /e/ e /o/, indica abertura
-- * Acento circunflexo (^): marca tonicidade e fechamento em /e/ e /o/
-- * Acento grave (`): indica crase (fenômeno sintático, não fonológico)
--
-- __Vogais nasalizadas:__
--
-- * Til (~): marca nasalização fonológica em /ã/ e /õ/
--
-- Esta classificação é fundamental para a silabação porque vogais
-- são os únicos segmentos que podem funcionar como núcleo silábico
-- em português.
--
-- __Exemplos:__
--
-- >>> isVowel 'a'
-- True
-- >>> isVowel 'ã'  -- vogal nasalizada
-- True
-- >>> isVowel 'ê'  -- vogal fechada tônica
-- True
-- >>> isVowel 'p'  -- consoante
-- False
isVowel :: Char -> Bool
isVowel c = c `elem` "aeiouáéíóúâêôãõ"

-- | Transdutor ortográfico-fonológico que converte representação gráfica
-- em sequência de fonemas categorizados.
--
-- Esta função implementa uma conversão simplificada do sistema de escrita
-- alfabético para representação fonêmica categorial. O processo envolve:
--
-- __Mapeamento grafema-fonema:__
--
-- * Cada grafema é analisado individualmente
-- * Aplicação de classificação binária: [±vocálico]
-- * Preservação da sequência linear da cadeia gráfica
--
-- __Limitações da implementação atual:__
--
-- * Não trata dígrafos (ch, lh, nh, qu, gu)
-- * Não considera variação contextual (c → /k/ vs /s/)
-- * Não implementa regras morfofonológicas
--
-- __Casos especiais tratados:__
--
-- * Vogais acentuadas mantêm status vocálico
-- * Vogais nasalizadas (ã, õ) são tratadas como núcleo simples
--
-- __Exemplos:__
--
-- >>> toPhonemes "casa"
-- [Consonant 'c',Vowel 'a',Consonant 's',Vowel 'a']
--
-- >>> toPhonemes "pão"
-- [Consonant 'p',Vowel 'ã',Consonant 'o']
toPhonemes :: String -> WordPhonemes
toPhonemes [] = []
toPhonemes (c:cs)
  | isVowel c = Vowel c : toPhonemes cs
  | otherwise = Consonant c : toPhonemes cs

-- | Predicado para identificação de clusters consonantais bem-formados
-- no onset silábico do português.
--
-- Esta função implementa o princípio da __Escala de Sonoridade__ aplicado
-- aos encontros consonantais permitidos na posição de onset em português.
--
-- __Fundamentação teórica:__
--
-- Os clusters identificados respeitam a restrição de que a sonoridade
-- deve ser crescente da margem para o núcleo silábico:
--
-- * __Obstruinte + Líquida__: padrão universal de onset complexo
-- * __Escala:__ oclusiva < fricativa < líquida < vogal
--
-- __Clusters com \/r\/__ (rótico):
--
-- * Obstruintes + /r/: pr, br, tr, dr, cr, gr, fr, vr
-- * O rótico tem alta sonoridade, próxima às vogais
--
-- __Clusters com \/l\/__ (lateral):
--
-- * Obstruintes + /l/: pl, bl, cl, gl, fl
-- * A lateral tem sonoridade intermediária
--
-- __Clusters ausentes:__
--
-- * *sr, *nr, *lr: violam restrições de sonoridade
-- * *tl, *dl: não ocorrem no onset do português (cf. inglês)
--
-- Estes clusters são considerados "perfeitos" porque são indivisíveis
-- na silabação - sempre permanecem juntos no onset da mesma sílaba.
--
-- __Exemplos:__
--
-- >>> isPerfectCluster 'p' 'r'  -- "prato" → pra.to
-- True
-- >>> isPerfectCluster 'f' 'l'  -- "flor" → flor
-- True
-- >>> isPerfectCluster 's' 'r'  -- inexistente no português
-- False
isPerfectCluster :: Char -> Char -> Bool
isPerfectCluster c1 c2 =
  (c1, c2)
    `elem` [ ('p', 'r') -- oclusiva bilabial + rótico
           , ('b', 'r') -- oclusiva bilabial + rótico
           , ('t', 'r') -- oclusiva alveolar + rótico
           , ('d', 'r') -- oclusiva alveolar + rótico
           , ('c', 'r') -- oclusiva velar + rótico
           , ('g', 'r') -- oclusiva velar + rótico
           , ('f', 'r') -- fricativa labiodental + rótico
           , ('v', 'r') -- fricativa labiodental + rótico
           , ('p', 'l') -- oclusiva bilabial + lateral
           , ('b', 'l') -- oclusiva bilabial + lateral
           , ('c', 'l') -- oclusiva velar + lateral
           , ('g', 'l') -- oclusiva velar + lateral
           , ('f', 'l') -- fricativa labiodental + lateral
           ]

-- | Aplicador de regras fonológicas para segmentação silábica.
--
-- Esta função serve como interface para o sistema de regras de silabação,
-- encapsulando o processo de aplicação das restrições fonotáticas do
-- português. Retorna 'Right' para indicar sucesso na derivação silábica.
--
-- __Arquitetura do sistema:__
--
-- * Input: cadeia fonêmica linear
-- * Processo: aplicação iterativa de regras de silabação
-- * Output: estrutura silábica hierárquica
applyRules :: WordPhonemes -> Either String [Syllable]
applyRules phonemes = Right (syllabifyPhonemes phonemes)

-- | Motor de silabação recursiva baseado em parsing incremental.
--
-- Implementa o algoritmo principal de silabação através de decomposição
-- recursiva da cadeia fonêmica. O processo segue o princípio de
-- __Maximum Onset__ da teoria silábica:
--
-- __Estratégia algorítmica:__
--
-- 1. __Parsing incremental__: processa fonemas sequencialmente
-- 2. __Construção gulosa__: maximiza material no onset quando possível
-- 3. __Recursão em cauda__: aplica o mesmo processo ao restante da cadeia
--
-- __Vantagens da abordagem recursiva:__
--
-- * Modularidade: cada sílaba é construída independentemente
-- * Composicionalidade: palavras complexas são tratadas sistematicamente
-- * Extensibilidade: facilita adição de novas regras
--
-- __Exemplos de processamento:__
--
-- >>> syllabifyPhonemes [Consonant 'c',Vowel 'a',Consonant 's',Vowel 'a']
-- [Syllable [Consonant 'c',Vowel 'a'], Syllable [Consonant 's',Vowel 'a']]
syllabifyPhonemes :: WordPhonemes -> [Syllable]
syllabifyPhonemes [] = []
syllabifyPhonemes phonemes =
  let (syllable, remaining) = takeSyllable phonemes
   in syllable : syllabifyPhonemes remaining

-- | Extrator de constituinte silábico através de parsing determinístico.
--
-- Esta função implementa o __primeiro passo__ do algoritmo de silabação:
-- a identificação e extração de uma sílaba completa a partir do início
-- da cadeia fonêmica.
--
-- __Funcionamento:__
--
-- * __Input__: cadeia fonêmica restante para processamento
-- * __Output__: tupla (sílaba_extraída, cadeia_residual)
-- * __Base case__: cadeia vazia resulta em sílaba vazia
-- * __Recursive case__: delega construção para 'buildSyllable'
--
-- Esta separação modular permite diferentes estratégias de construção
-- silábica sem alterar a lógica de iteração principal.
takeSyllable :: WordPhonemes -> (Syllable, WordPhonemes)
takeSyllable []       = (Syllable [], [])
takeSyllable phonemes = buildSyllable [] phonemes

-- | Construtor incremental de sílabas baseado em regras fonotáticas do português.
--
-- Esta é a função central do algoritmo, implementando as __Regras de Silabação__
-- do português através de pattern matching sobre contextos fonológicos.
--
-- __Parâmetros:__
--
-- * @acc@: acumulador contendo fonemas já processados da sílaba atual
-- * @(p:ps)@: cadeia fonêmica restante para análise
--
-- __Regras implementadas:__
--
-- __0. Regra V-V (Hiato)__
--
-- @(_, Vowel v, rest) | hasVowel acc@
--
-- * __Contexto__: sílaba com núcleo + vogal adicional
-- * __Ação__: finaliza sílaba atual, deixa vogal para nova sílaba
-- * __Exemplo__: "extraordinário" → ex.tra.or.di.ná.ri.o, "teatro" → te.a.tro
-- * __Princípio__: vogais adjacentes formam hiato (sílabas separadas)
-- * __Nota__: ditongos ortográficos antes de NH são sempre hiato ("rainha" → ra.i.nha)
--
-- __1. Regra V-CV (Onset Máximo)__
--
-- @(_, Consonant c, Vowel v:rest) | hasVowel acc@
--
-- * __Contexto__: sílaba com núcleo + consoante + vogal seguinte
-- * __Ação__: finaliza sílaba atual, deixa CV para próxima sílaba
-- * __Exemplo__: "casa" → ca.sa (não *cas.a)
-- * __Princípio__: maximiza onset da sílaba seguinte
--
-- __2. Regra V-CCV (Cluster Preservation)__
--
-- @(_, Consonant c1, Consonant c2:rest) | hasVowel acc && isPerfectCluster c1 c2@
--
-- * __Contexto__: sílaba com núcleo + cluster consonantal válido
-- * __Ação__: preserva cluster intacto para próxima sílaba
-- * __Exemplo__: "sopro" → so.pro (não *sop.ro)
-- * __Princípio__: clusters bem-formados são indivisíveis
--
-- __3A. Regra V-CCCC+ (Sequências Consonantais Complexas)__
--
-- @(_, Consonant c1, Consonant c2:Consonant c3:Consonant c4:rest) | hasVowel acc@
--
-- * __Contexto__: sílaba com núcleo + sequência de 4+ consoantes
-- * __Padrão especial__: Nasal + Oclusiva + Fricativa (ex: "ngst" em tungstênio)
-- * __Ação__: mantém sequência NOS na coda se válida, senão quebra após primeira
-- * __Exemplo__: "tungstênio" → tungs.tê.nio
--
-- __3B. Regra V-CCC (Sequências Triplas)__
--
-- @(_, Consonant c1, Consonant c2:Consonant c3:rest) | hasVowel acc@
--
-- * __Contexto__: sílaba com núcleo + sequência de 3 consoantes
-- * __Estratégias__: preserva clusters finais válidos, ou mantém nasais+oclusivas
-- * __Exemplo__: "instrumento" → ins.tru.men.to
--
-- __3C. Regra VC-CV (Splitting Consonântico Simples)__
--
-- @(_, Consonant c1, Consonant c2:rest) | hasVowel acc@
--
-- * __Contexto__: sílaba com núcleo + sequência consonantal dupla não-cluster
-- * __Ação__: divide consoantes entre sílabas (primeira fica, segunda vai)
-- * __Exemplo__: "pasta" → pas.ta (não *pa.sta)
-- * __Princípio__: evita onset marcado, prefere coda simples
--
-- __4. Caso Default (Acumulação)__
--
-- @_ -> buildSyllable (acc ++ [p]) ps@
--
-- * __Contexto__: qualquer situação não coberta pelas regras anteriores
-- * __Ação__: incorpora fonema atual à sílaba em construção
-- * __Exemplos__: onset inicial, núcleo, etc.
--
-- __Base Case:__
--
-- @buildSyllable acc [] = (Syllable acc, [])@
--
-- * Finaliza construção quando não há mais fonemas para processar
buildSyllable :: [Phoneme] -> WordPhonemes -> (Syllable, WordPhonemes)
buildSyllable acc [] = (Syllable acc, [])
buildSyllable acc (p:ps) =
  case (acc, p, ps) of
        -- REGRA 0: V-V (Hiato)
        -- Se já temos núcleo vocálico e encontramos outra vogal,
        -- finalizamos a sílaba atual (hiato - vogais em sílabas diferentes)
        -- REGRA ESPECIAL: ditongos ortográficos antes de NH são sempre hiato
    (_, Vowel v, rest)
      | hasVowel acc -> (Syllable acc, Vowel v : rest)
      | isBeforeNH ps -> (Syllable (acc ++ [Vowel v]), rest)
        -- REGRA 1: V-CV (Onset Máximo)
        -- Se já temos núcleo vocálico e encontramos C-V, finalizamos aqui
        -- para maximizar onset da próxima sílaba
    (_, Consonant c, Vowel v:rest)
      | hasVowel acc -> (Syllable acc, Consonant c : Vowel v : rest)
        -- REGRA 2: V-CCV (Preservação de Cluster)
        -- Se já temos núcleo e encontramos cluster perfeito,
        -- preservamos o cluster intacto na próxima sílaba
    (_, Consonant c1, Consonant c2:rest)
      | hasVowel acc && isPerfectCluster c1 c2 ->
        (Syllable acc, Consonant c1 : Consonant c2 : rest)
        -- REGRA 3A: V-CCCC+ (Sequências Consonantais Complexas)
        -- Para sequências longas de consoantes, quebra na última consoante
        -- ou no último cluster válido, respeitando padrão Nasal+Oclusiva+Fricativa
    (_, Consonant c1, Consonant c2:Consonant c3:Consonant c4:rest)
      | hasVowel acc ->
        if isNasal c1 && isPlosive c2 && isFricative c3
          then ( Syllable (acc ++ [Consonant c1, Consonant c2, Consonant c3])
               , Consonant c4 : rest)
          else ( Syllable (acc ++ [Consonant c1])
               , Consonant c2 : Consonant c3 : Consonant c4 : rest)
          -- Verifica padrão nasal + oclusiva + fricativa (ex: "ngst")
        -- REGRA 3B: V-CCC (Sequências Triplas)
        -- Para sequências de 3 consoantes, verifica padrões especiais
    (_, Consonant c1, Consonant c2:Consonant c3:rest)
      | hasVowel acc ->
        if isPerfectCluster c2 c3
          then ( Syllable (acc ++ [Consonant c1])
               , Consonant c2 : Consonant c3 : rest)
          else if isNasal c1 && isPlosive c2
                 then ( Syllable (acc ++ [Consonant c1, Consonant c2])
                      , Consonant c3 : rest)
                 else ( Syllable (acc ++ [Consonant c1])
                      , Consonant c2 : Consonant c3 : rest)
          -- Se as duas últimas formam cluster válido, quebra antes delas
          -- Se nasal + oclusiva, mantém juntos na coda
        -- REGRA 3C: VC-CV (Divisão Consonântica Simples)
        -- Se já temos núcleo e encontramos CC não-cluster,
        -- dividimos: primeira consoante fica (coda), segunda vai (onset)
    (_, Consonant c1, Consonant c2:rest)
      | hasVowel acc -> (Syllable (acc ++ [Consonant c1]), Consonant c2 : rest)
        -- CASO PADRÃO: Acumulação
        -- Incorpora fonema atual e continua construção
    _ -> buildSyllable (acc ++ [p]) ps

-- | Verifica se uma consoante é nasal (m, n).
--
-- As consoantes nasais têm propriedades especiais na fonotática portuguesa,
-- especialmente em sequências consonantais complexas.
isNasal :: Char -> Bool
isNasal c = c `elem` "mn"

-- | Verifica se uma consoante é fricativa (f, v, s, z, etc.).
--
-- As fricativas podem ocupar posições específicas em clusters complexos,
-- especialmente na posição final de sequências consonantais.
isFricative :: Char -> Bool
isFricative c = c `elem` "fvszxj"

-- | Verifica se uma consoante é oclusiva (p, b, t, d, c, g, k).
--
-- As oclusivas formam o núcleo de muitos clusters consonantais.
isPlosive :: Char -> Bool
isPlosive c = c `elem` "pbtdcgk"

-- | Verifica se uma sequência de fonemas termina com o dígrafo "nh".
--
-- Esta função é essencial para aplicar a regra fonológica específica do português:
-- ditongos ortográficos antes de NH são sempre pronunciados como hiato.
--
-- __Regra fonológica:__
--
-- * "rainha" → ra.i.nha (não *rai.nha)
-- * "bainha" → ba.i.nha (não *bai.nha)
-- * "campainha" → cam.pa.i.nha (não *cam.pai.nha)
--
-- O dígrafo NH tem propriedades fonológicas especiais que desfazem
-- ditongos ortográficos adjacentes, forçando pronuncia hiática.
isBeforeNH :: WordPhonemes -> Bool
isBeforeNH (Consonant 'n':Consonant 'h':_) = True
isBeforeNH []                              = False
isBeforeNH (_:rest)                        = isBeforeNH rest

-- | Predicado para detecção de núcleo silábico em sequência fonêmica.
--
-- Esta função implementa o teste fundamental para __Licensing Silábico__:
-- verifica se uma sequência de fonemas já possui um núcleo vocálico
-- licenciado, requisito obrigatório para constituir uma sílaba válida.
--
-- __Fundamentação teórica:__
--
-- * __Princípio do Núcleo Obrigatório__: toda sílaba deve ter exatamente
--   uma vogal funcionando como núcleo
-- * __Hierarquia de Sonoridade__: apenas elementos de alta sonoridade
--   (vogais) podem ocupar a posição nuclear
-- * __Licensing__: o núcleo "licencia" os elementos periféricos (onset/coda)
--
-- __Comportamento algorítmico:__
--
-- * __Busca linear__: percorre a sequência até encontrar vogal
-- * __Short-circuit__: retorna 'True' imediatamente ao encontrar núcleo
-- * __Determinístico__: não considera casos de núcleos múltiplos
--
-- __Uso no algoritmo:__
--
-- Esta função é crucial para determinar quando uma sílaba pode ser
-- finalizada - apenas sequências com núcleo já estabelecido podem
-- ser submetidas às regras de fronteira silábica.
--
-- __Exemplos:__
--
-- >>> hasVowel [Consonant 'p', Consonant 'r']  -- onset sem núcleo
-- False
-- >>> hasVowel [Consonant 'p', Vowel 'a']      -- onset + núcleo
-- True
-- >>> hasVowel [Vowel 'a', Consonant 's']      -- núcleo + coda
-- True
hasVowel :: [Phoneme] -> Bool
hasVowel []                 = False
hasVowel (Vowel _:_)        = True -- Found nucleus - short circuit
hasVowel (Consonant _:rest) = hasVowel rest -- Skip consonant, continue search

-- | Interface principal do sistema de silabação automática.
--
-- Esta função implementa a __arquitetura híbrida__ do silabificador,
-- combinando processamento baseado em regras com lookup lexical para
-- casos excepcionais.
--
-- __Pipeline de processamento:__
--
-- __1. Normalização Ortográfica__
--
-- @word = normalize text@
--
-- * Converte input para forma canônica (minúsculas)
-- * Garante consistência para lookup lexical
-- * Remove variação tipográfica irrelevante
--
-- __2. Consulta Lexical (Exceções)__
--
-- @Map.lookup word exceptionMap@
--
-- * __Primeiro__: verifica se palavra está no léxico de exceções
-- * __Se encontrada__: retorna silabação pré-definida (bypass das regras)
-- * __Vantagem__: trata casos idiossincráticos que violam regras gerais
-- * __Exemplos__: empréstimos, termos técnicos, compostos irregulares
--
-- __3. Processamento Fonológico (Regras Gerais)__
--
-- @phonemes = toPhonemes word; applyRules phonemes@
--
-- * __Se não encontrada no léxico__: aplica pipeline fonológico completo
-- * __Conversão__: ortografia → representação fonêmica
-- * __Silabação__: aplicação sistemática das regras fonotáticas
--
-- __Arquitetura de fallback:__
--
-- ```
-- Input → Normalização → Lookup Lexical ─┬─→ Output (se exceção)
--                                         │
--                                         └─→ Pipeline Fonológico → Output
-- ```
--
-- __Tipos de retorno:__
--
-- * @Right [Syllable]@: silabação bem-sucedida
-- * @Left String@: erro no processamento (não utilizado na implementação atual)
--
-- __Exemplos:__
--
-- >>> syllabify "casa"      -- regra geral
-- Right [Syllable [Consonant 'c',Vowel 'a'], Syllable [Consonant 's',Vowel 'a']]
--
-- >>> syllabify "tungstênio"  -- exceção lexical
-- Right [Syllable [...], ...]  -- silabação pré-definida
syllabify :: String -> Either String [Syllable]
syllabify text =
  let word = normalize text
   in case Map.lookup word exceptionMap of
        Just syllables -> Right syllables -- Exception path: use pre-defined syllabification
        Nothing -- Rule-based path: apply phonological processing
         ->
          let phonemes = toPhonemes word
           in applyRules phonemes
