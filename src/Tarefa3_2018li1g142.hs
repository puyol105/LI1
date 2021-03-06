{-|
Module      : Tarefa3_2018li1g142
Description : Comprimir e descomprimir o Estado do jogo
Copyright   : Marco Matias Pereira Gonçalves <a75480@alunos.uminho.pt>
              Renato Jorge Cruzinha da Silva <a75310@alunos.uminho.pt>

= Introdução:
Nesta tarefa foi nos proposto fazer uma função 'comprime', cujo objetivo é comprimir o 'Estado' atual do jogo e o 
transformá-lo na menor 'String' possível. Para tal, tivemos como objetivo transformar cada parte do jogo 
(mapa, jogadores e disparos) em 'String's pequenas. Subsequencialmente, era também necessária a criação de uma função 
'descomprime' que descodifica a string comprimida num 'Estado' de jogo.

= Objetivos:
O principal foco do nosso grupo perante esta tarefa, foi comprimir o 'Estado' atual de jogo separadamente por variáveis (
neste caso separará o 'Mapa' do jogo, os Jogdaores e os Disparos). 
Sendo assim, para comprimir o 'Mapa', e para ao descomprimir sabermos que estamos a descomprimir uma 'Mapa', todos eles se
iniciam com o carater 'M', assim sempre que a na 'String' final apareça um 'M' significa que está um 'Mapa' à sua frente. Para
distinguir a 'Peca' que representa o 'Mapa' numa certa posição, foi utilizado uma distribuição de três letras representativas 
para cada elemento. Sendo assim definimos que o carater 'v' iria representar um parte 'Vazia', o carater 'i' representaria um 
'Bloco Indestrutivel' e por fim o carater 'd' para um 'Bloco Destrutivel'. O nosso grupo também teve em conta o facto de 
existirem paredes que são sistemáticas e apenas ocupam espaço no resultado final, como é o caso das Paredes limitadoras do 'Mapa'.
Posto isto, segue-se a parte de comprimir os Jogadores. Nesta parte da tarefa usamos um sistema similar ao da parte do 'Mapa'
usando também um carater 'J' para ao descomprimir ser fácil de localizar todas as variáveis pertencentes ao mesmo. Destas 
variáveis apenas a posição não retorna o seu valor, todas as outras irão recorrer ao auxílio do 'show' para se comprimirem. Na 
posição são mostrados os dois valores separados por um espaço.
Para finalizar a compressão falta apenas comprimir os disparos do 'Estado', sendo a estratégia a ser utilizada
a mesma do caso do 'Mapa' e dos '[Jogador]'es. Foi reservado o carater 'D' para assinalar o início de todos os disparos, e o 
carater subsequente para distinguir os varios tipos de disparos, sendo eles separados do seguinte modo: 'l' para os disparos
laser, 'c' para as balas de canhão e 'x' para assinalar os choques.
A estratégia que o nosso grupo acabou por adotar, foi reservar um carater para representar cada parte do 'Estado'. Este 
carater foi utilizado para facilitar a descompressão do nosso programa, que utiliza essas letras como marco para saber o que 
está a descomprimir.

= Conclusão:
Esta tarefa foi um pouco diferente das anteriores, pois, no fundo é um extra ao jogo, sendo que não é necessária a sua
concretização para que o jogo seja funcional. O objetivo foi cumprido com sucesso tendo em conta que é funcional, contudo 
é possível ter uma taxa de compressão de maior sucesso, pois o nosso grupo utliza carateres repetidos, e alguns que não
estão em uso, e como tal existe uma margem para melhorar. Contudo o objetivo foi cumprido.
-}
module Tarefa3_2018li1g142 where

import LI11819
import Tarefa0_2018li1g142


-- * Função Comprime que codifica um estado numa String.

-- | Comprime um 'Estado' para formato textual.
--
-- __NB:__ A função 'show' representa um 'Estado' num formato textual facilmente legível mas extenso.
--
-- __NB:__ Uma boa solução deve representar o 'Estado' dado no mínimo número de caracteres possível.
comprime :: Estado -> String
comprime (Estado m j d)= unlines $ [zipMapa m] ++ zipJogadores j ++ zipDisparos d


-- | Comprime um mapa.
zipMapa :: Mapa -> String
zipMapa map = unwords (['M']:zipMapaChar m)
        where m = retiraParedes map

-- | Transforma um mapa numa numa lista de Strings.
zipMapaChar :: Mapa -> [String]
zipMapaChar = map linhaToChar

-- | Transforma uma linha de Pecas num lista de Char.
linhaToChar :: [Peca] -> String
linhaToChar [] = []
linhaToChar (h:t) = case h of
                    Vazia               -> 'v':linhaToChar t
                    Bloco Indestrutivel -> 'i':linhaToChar t
                    Bloco Destrutivel   -> 'd':linhaToChar t

-- | Remove as paredes exteriores de um mapa.
retiraParedes :: Mapa -> Mapa
retiraParedes = retiraParedesAux . retiraPeU

-- | Remove os extremos de cada linha de peca.
retiraParedesAux :: Mapa -> Mapa
retiraParedesAux = map retiraPeU

-- | Remove a cabeça && ultimo elemento de uma lista.
retiraPeU :: [a] -> [a]
retiraPeU m = drop 1 aux
        where aux = take (length m -1) m


-- | Comprime todos os jogadores.
zipJogadores :: [Jogador] -> [String]
zipJogadores = map zipJogador

-- | Comprime um Jogador Numa String.
zipJogador :: Jogador -> String
zipJogador (Jogador p d v l c) = 
    unwords ["J",zipTupulo p,show d,show v,show l,show c]

-- | Comprime um Tupulo Numa String.
zipTupulo :: (Int,Int) -> String
zipTupulo (x,y) = show x ++ " " ++ show y


-- | Comprime todos os disparos.
zipDisparos :: [Disparo] -> [String]
zipDisparos = map zipDisparo
-- | Comprime um disparo.
zipDisparo :: Disparo -> String
zipDisparo d = case d of
                DisparoLaser  {} -> unwords ["Dl", show (jogadorDisparo d), zipTupulo (posicaoDisparo d), show (direcaoDisparo d)]
                DisparoCanhao {} -> unwords ["Dc", show (jogadorDisparo d), zipTupulo (posicaoDisparo d), show (direcaoDisparo d)]
                DisparoChoque {} -> unwords ["Dx", show (jogadorDisparo d), show (tempoDisparo d)]

-- * Função Descomprime uma String préviamente codificada num Estado.

-- | Descomprime um 'Estado' no formato textual utilizado pela função 'comprime'.
--
-- __NB:__ A função 'comprime' é válida de for possível recuperar o 'Estado' utilizando a função 'descomprime', i.e.:
--
-- prop> descomprime . comprime = id
--
-- __NB:__ Esta propriedade é particularmente válida para a solução pré-definida:
--
-- prop> read . show = id
descomprime :: String -> Estado
descomprime s = Estado m (reverse j) (reverse d)
                where (Estado m j d) = descomprimeAux s

-- | Função Auxiliar da 'descomprime'.
descomprimeAux :: String -> Estado
descomprimeAux s = descomprimeLinhas e $ Estado [] [] []
                where e = lines s

-- | Função que parsa as linhas codificadas.
descomprimeLinhas :: [String] -> Estado -> Estado
descomprimeLinhas [] e = e
descomprimeLinhas (h:t) (Estado m j d) | x == 'M'  = descomprimeLinhas t $ Estado (unZipMap h) j d
                                       | x == 'J'  = descomprimeLinhas t $ Estado m (unZipJogador h : j) d
                                       | x == 'D'  = descomprimeLinhas t $ Estado m j (unZipDisparo h : d)
                                       | otherwise = descomprimeLinhas t $ Estado m j d
                where x = head h

-- | Descomprime o mapa.
unZipMap :: String -> Mapa
unZipMap map = criaParedes $ unZipMapPeca m
              where m = drop 1 (words map)

-- | Transforma uma lista de Strings num Mapa.
unZipMapPeca :: [String] -> Mapa
unZipMapPeca = map charToLinha

-- | Transforma uma String numa lista de Pecas.
charToLinha :: String -> [Peca]
charToLinha [] = []
charToLinha (h:t) = case h of
                    'v' -> Vazia : charToLinha t 
                    'i' -> Bloco Indestrutivel : charToLinha t
                    'd' -> Bloco Destrutivel : charToLinha t

-- | Remove as paredes exteriores de um mapa.
criaParedes :: Mapa -> Mapa
criaParedes map = criaParedesAux m
                where m = addPcimaEBaixo map

-- | Cria os extremos de cada linha 'Bloco Indestrutivel'.
criaParedesAux :: Mapa -> Mapa
criaParedesAux = map addPeU

-- | Adiciona as paredes de cima e baixo.
addPcimaEBaixo :: Mapa -> Mapa
addPcimaEBaixo map = m ++ [replicate x b]
                    where b = Bloco Indestrutivel
                          x = length (head map)
                          m = replicate x b : map

-- | Adicina um bloco Indestrutivel na cabeça && ultimo elemento da lista.
addPeU :: [Peca] -> [Peca]
addPeU l = (b:l) ++ [b]
     where b = Bloco Indestrutivel


-- | Descomprime o Jogador.
unZipJogador :: String -> Jogador
unZipJogador s = Jogador 
                    (read (l !! 1) :: Int,read (l !! 2) :: Int) 
                    (read (l !! 3) :: Direcao) 
                    (read (l !! 4) :: Int) 
                    (read (l !! 5) :: Int) 
                    (read (l !! 6) :: Int)
                where l = words s

-- | Descodifica um linha referente a um disparo.
unZipDisparo :: String -> Disparo
unZipDisparo s = case head l of
                      "Dl" -> DisparoLaser 
                                    (read (l !! 1) :: Int)
                                    (read (l !! 2) :: Int,read (l !! 3) :: Int) 
                                    (read (l !! 4) :: Direcao)
                      "Dc" -> DisparoCanhao 
                                    (read (l !! 1) :: Int)
                                    (read (l !! 2) :: Int,read (l !! 3) :: Int) 
                                    (read (l !! 4) :: Direcao)                                    
                      "Dx" -> DisparoChoque 
                                    (read (l !! 1) :: Int)
                                    (read (l !! 2) :: Int)
                where l = words s

-- | Testes unitários da Tarefa 3.
--
-- Cada teste é um 'Estado'.
testesT3 :: [Estado]
testesT3 = [Estado [[Bloco Indestrutivel,Bloco Indestrutivel,Bloco Indestrutivel,Bloco Indestrutivel,Bloco Indestrutivel,Bloco Indestrutivel],
                    [Bloco Indestrutivel,Vazia,Vazia,Vazia,Vazia,Bloco Indestrutivel],
                    [Bloco Indestrutivel,Vazia,Vazia,Vazia,Vazia,Bloco Indestrutivel],
                    [Bloco Indestrutivel,Vazia,Vazia,Vazia,Vazia,Bloco Indestrutivel],
                    [Bloco Indestrutivel,Vazia,Vazia,Vazia,Vazia,Bloco Indestrutivel],
                    [Bloco Indestrutivel,Vazia,Vazia,Vazia,Vazia,Bloco Indestrutivel],
                    [Bloco Indestrutivel,Bloco Indestrutivel,Bloco Indestrutivel,Bloco Indestrutivel,Bloco Indestrutivel,Bloco Indestrutivel]] 
                    [Jogador (1,1) D 5 1 1] 
                    [DisparoLaser 1 (1,2) D],
            Estado [[Bloco Indestrutivel,Bloco Indestrutivel,Bloco Indestrutivel,Bloco Indestrutivel,Bloco Indestrutivel,Bloco Indestrutivel],
                    [Bloco Indestrutivel,Vazia,Vazia,Vazia,Vazia,Bloco Indestrutivel],
                    [Bloco Indestrutivel,Vazia,Vazia,Vazia,Vazia,Bloco Indestrutivel],
                    [Bloco Indestrutivel,Vazia,Vazia,Vazia,Vazia,Bloco Indestrutivel],
                    [Bloco Indestrutivel,Vazia,Vazia,Vazia,Vazia,Bloco Indestrutivel],
                    [Bloco Indestrutivel,Vazia,Vazia,Vazia,Vazia,Bloco Indestrutivel],
                    [Bloco Indestrutivel,Vazia,Vazia,Vazia,Vazia,Bloco Indestrutivel],
                    [Bloco Indestrutivel,Vazia,Vazia,Vazia,Vazia,Bloco Indestrutivel],
                    [Bloco Indestrutivel,Vazia,Vazia,Vazia,Vazia,Bloco Indestrutivel],
                    [Bloco Indestrutivel,Vazia,Vazia,Vazia,Vazia,Bloco Indestrutivel],
                    [Bloco Indestrutivel,Vazia,Vazia,Vazia,Vazia,Bloco Indestrutivel],
                    [Bloco Indestrutivel,Vazia,Vazia,Vazia,Vazia,Bloco Indestrutivel],
                    [Bloco Indestrutivel,Vazia,Vazia,Vazia,Vazia,Bloco Indestrutivel],
                    [Bloco Indestrutivel,Vazia,Vazia,Vazia,Vazia,Bloco Indestrutivel],
                    [Bloco Indestrutivel,Vazia,Vazia,Vazia,Vazia,Bloco Indestrutivel],
                    [Bloco Indestrutivel,Vazia,Vazia,Vazia,Vazia,Bloco Indestrutivel],
                    [Bloco Indestrutivel,Vazia,Vazia,Vazia,Vazia,Bloco Indestrutivel],
                    [Bloco Indestrutivel,Vazia,Vazia,Vazia,Vazia,Bloco Indestrutivel],
                    [Bloco Indestrutivel,Vazia,Vazia,Vazia,Vazia,Bloco Indestrutivel],
                    [Bloco Indestrutivel,Vazia,Vazia,Vazia,Vazia,Bloco Indestrutivel],
                    [Bloco Indestrutivel,Vazia,Vazia,Vazia,Vazia,Bloco Indestrutivel],
                    [Bloco Indestrutivel,Vazia,Vazia,Vazia,Vazia,Bloco Indestrutivel],
                    [Bloco Indestrutivel,Bloco Indestrutivel,Bloco Indestrutivel,Bloco Indestrutivel,Bloco Indestrutivel,Bloco Indestrutivel]]
                    [Jogador (1,1) D 5 1 1,Jogador (3,3) C 5 1 1]
                    [DisparoLaser 1 (2,3) D],
            Estado [[Bloco Indestrutivel,Bloco Indestrutivel,Bloco Indestrutivel,Bloco Indestrutivel,Bloco Indestrutivel,Bloco Indestrutivel,Bloco Indestrutivel,Bloco Indestrutivel],
                    [Bloco Indestrutivel,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Bloco Indestrutivel],
                    [Bloco Indestrutivel,Bloco Indestrutivel,Bloco Indestrutivel,Bloco Indestrutivel,Bloco Indestrutivel,Vazia,Vazia,Bloco Indestrutivel],
                    [Bloco Indestrutivel,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Bloco Indestrutivel],
                    [Bloco Indestrutivel,Vazia,Vazia,Bloco Indestrutivel,Bloco Indestrutivel,Bloco Indestrutivel,Bloco Indestrutivel,Bloco Indestrutivel],
                    [Bloco Indestrutivel,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Bloco Indestrutivel],
                    [Bloco Indestrutivel,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Bloco Indestrutivel],
                    [Bloco Indestrutivel,Bloco Indestrutivel,Bloco Indestrutivel,Bloco Indestrutivel,Bloco Indestrutivel,Bloco Indestrutivel,Bloco Indestrutivel,Bloco Indestrutivel]]
                    [Jogador (4,1) D 5 1 3, Jogador (5,3) E 5 2 4]
                    [DisparoCanhao 1 (5,2) E,DisparoChoque 0 3],
            Estado [[Bloco Indestrutivel,Bloco Indestrutivel,Bloco Indestrutivel,Bloco Indestrutivel,Bloco Indestrutivel,Bloco Indestrutivel,Bloco Indestrutivel,Bloco Indestrutivel,Bloco Indestrutivel,Bloco Indestrutivel,Bloco Indestrutivel,Bloco Indestrutivel,Bloco Indestrutivel,Bloco Indestrutivel,Bloco Indestrutivel,Bloco Indestrutivel,Bloco Indestrutivel,Bloco Indestrutivel,Bloco Indestrutivel,Bloco Indestrutivel,Bloco Indestrutivel,Bloco Indestrutivel,Bloco Indestrutivel,Bloco Indestrutivel,Bloco Indestrutivel],
                    [Bloco Indestrutivel,Vazia,Vazia,Vazia,Vazia,Vazia,Bloco Destrutivel,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Bloco Destrutivel,Bloco Destrutivel,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Bloco Indestrutivel],
                    [Bloco Indestrutivel,Vazia,Vazia,Vazia,Vazia,Vazia,Bloco Destrutivel,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Bloco Destrutivel,Bloco Destrutivel,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Bloco Indestrutivel],
                    [Bloco Indestrutivel,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Bloco Destrutivel,Bloco Destrutivel,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Bloco Indestrutivel],
                    [Bloco Indestrutivel,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Bloco Destrutivel,Bloco Destrutivel,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Bloco Indestrutivel],
                    [Bloco Indestrutivel,Bloco Indestrutivel,Bloco Indestrutivel,Bloco Indestrutivel,Bloco Indestrutivel,Bloco Indestrutivel,Bloco Indestrutivel,Bloco Indestrutivel,Bloco Indestrutivel,Bloco Indestrutivel,Bloco Indestrutivel,Bloco Indestrutivel,Bloco Indestrutivel,Bloco Indestrutivel,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Bloco Indestrutivel],
                    [Bloco Indestrutivel,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Bloco Indestrutivel,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Bloco Indestrutivel],
                    [Bloco Indestrutivel,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Bloco Indestrutivel,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Bloco Indestrutivel],
                    [Bloco Indestrutivel,Bloco Indestrutivel,Bloco Indestrutivel,Bloco Indestrutivel,Bloco Indestrutivel,Bloco Indestrutivel,Bloco Indestrutivel,Bloco Indestrutivel,Bloco Indestrutivel,Bloco Indestrutivel,Bloco Indestrutivel,Bloco Indestrutivel,Bloco Indestrutivel,Bloco Indestrutivel,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Bloco Indestrutivel],
                    [Bloco Indestrutivel,Vazia,Vazia,Bloco Indestrutivel,Bloco Indestrutivel,Bloco Indestrutivel,Bloco Indestrutivel,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Bloco Indestrutivel],
                    [Bloco Indestrutivel,Vazia,Vazia,Bloco Indestrutivel,Bloco Indestrutivel,Bloco Indestrutivel,Bloco Indestrutivel,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Bloco Indestrutivel],
                    [Bloco Indestrutivel,Vazia,Vazia,Vazia,Vazia,Vazia,Bloco Indestrutivel,Vazia,Vazia,Vazia,Bloco Indestrutivel,Bloco Indestrutivel,Bloco Indestrutivel,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Bloco Indestrutivel],
                    [Bloco Indestrutivel,Vazia,Vazia,Vazia,Vazia,Vazia,Bloco Indestrutivel,Vazia,Vazia,Vazia,Bloco Indestrutivel,Vazia,Vazia,Vazia,Bloco Destrutivel,Bloco Destrutivel,Bloco Destrutivel,Bloco Destrutivel,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Bloco Indestrutivel],
                    [Bloco Indestrutivel,Vazia,Vazia,Bloco Indestrutivel,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Bloco Indestrutivel,Bloco Destrutivel,Bloco Destrutivel,Bloco Destrutivel,Bloco Destrutivel,Bloco Destrutivel,Bloco Destrutivel,Vazia,Vazia,Vazia,Bloco Destrutivel,Vazia,Vazia,Vazia,Bloco Indestrutivel],
                    [Bloco Indestrutivel,Vazia,Vazia,Bloco Indestrutivel,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Bloco Indestrutivel,Bloco Destrutivel,Vazia,Vazia,Bloco Destrutivel,Bloco Destrutivel,Bloco Destrutivel,Bloco Destrutivel,Vazia,Vazia,Bloco Destrutivel,Vazia,Vazia,Vazia,Bloco Indestrutivel],
                    [Bloco Indestrutivel,Vazia,Vazia,Bloco Indestrutivel,Bloco Indestrutivel,Bloco Indestrutivel,Bloco Indestrutivel,Bloco Indestrutivel,Bloco Indestrutivel,Bloco Indestrutivel,Bloco Indestrutivel,Bloco Destrutivel,Bloco Destrutivel,Bloco Destrutivel,Bloco Destrutivel,Bloco Destrutivel,Bloco Destrutivel,Bloco Destrutivel,Vazia,Vazia,Bloco Destrutivel,Vazia,Vazia,Vazia,Bloco Indestrutivel],
                    [Bloco Indestrutivel,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Bloco Indestrutivel],
                    [Bloco Indestrutivel,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Bloco Indestrutivel],
                    [Bloco Indestrutivel,Vazia,Vazia,Bloco Destrutivel,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Bloco Indestrutivel,Bloco Indestrutivel,Bloco Destrutivel,Vazia,Vazia,Vazia,Bloco Destrutivel,Bloco Destrutivel,Bloco Destrutivel,Bloco Destrutivel,Bloco Destrutivel,Vazia,Vazia,Bloco Indestrutivel],
                    [Bloco Indestrutivel,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Bloco Destrutivel,Bloco Indestrutivel,Bloco Indestrutivel,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Bloco Destrutivel,Bloco Destrutivel,Vazia,Vazia,Bloco Indestrutivel],[Bloco Indestrutivel,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Bloco Destrutivel,Bloco Indestrutivel,Bloco Destrutivel,Vazia,Vazia,Vazia,Bloco Destrutivel,Bloco Destrutivel,Bloco Destrutivel,Bloco Destrutivel,Bloco Destrutivel,Vazia,Vazia,Bloco Indestrutivel],
                    [Bloco Indestrutivel,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Bloco Destrutivel,Bloco Indestrutivel,Bloco Destrutivel,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Bloco Indestrutivel],
                    [Bloco Indestrutivel,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Bloco Destrutivel,Bloco Indestrutivel,Bloco Destrutivel,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Bloco Indestrutivel],[Bloco Indestrutivel,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Bloco Destrutivel,Bloco Indestrutivel,Bloco Destrutivel,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Bloco Indestrutivel],
                    [Bloco Indestrutivel,Bloco Indestrutivel,Bloco Indestrutivel,Bloco Indestrutivel,Bloco Indestrutivel,Bloco Indestrutivel,Bloco Indestrutivel,Bloco Indestrutivel,Bloco Indestrutivel,Bloco Indestrutivel,Bloco Indestrutivel,Bloco Indestrutivel,Bloco Indestrutivel,Bloco Indestrutivel,Bloco Indestrutivel,Bloco Indestrutivel,Bloco Indestrutivel,Bloco Indestrutivel,Bloco Indestrutivel,Bloco Indestrutivel,Bloco Indestrutivel,Bloco Indestrutivel,Bloco Indestrutivel,Bloco Indestrutivel,Bloco Indestrutivel]]
                    [Jogador (2,11) D 2 11 0,Jogador (8,18) E 0 2 0,Jogador (16,9) D 1 0 4,Jogador (9,17) B 2 1 1]
                    [DisparoChoque 0 4,DisparoLaser 2 (16,12) D,DisparoCanhao 3 (10,17) B]
            ]
