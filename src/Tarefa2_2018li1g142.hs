-- | Este módulo define funções comuns da Tarefa 2 do trabalho prático.
module Tarefa2_2018li1g142 where

import LI11819
import Tarefa0_2018li1g142
import Tarefa1_2018li1g142

-- * Funções principais da Tarefa 2.

-- | Efetua uma jogada.
jogada :: Int    -- ^ O identificador do 'Jogador' que efetua a jogada.
       -> Jogada -- ^ A 'Jogada' a efetuar.
       -> Estado -- ^ O 'Estado' anterior.
       -> Estado -- ^ O 'Estado' resultante após o jogador efetuar a jogada.
jogada i j e = case j of 
                       Movimenta dir -> if isAlive jog
                                        then if testaDirecao jog dir
                                             then if isParede m jog dir || jogEmChoque jog (indexJog e) || isBlocked i dir e jog
                                                  then e
                                                  else movimenta i e dir
                                             else Estado m (take i js ++ [mudaDir jog dir] ++ drop (i+1) js) ds
                                        else e
                       Dispara Canhao -> if isAlive jog
                                            then canhao i e 
                                            else e
                       Dispara Laser  -> if isAlive jog && temLaser jog
                                            then laser i e
                                            else e
                       Dispara Choque -> if isAlive jog && temChoque jog 
                                            then choque i e
                                            else e
                       where 
                            jog = js !! i
                            m = mapaEstado e
                            js = jogadoresEstado e
                            ds = disparosEstado e

-- | Testa se um jogador tem vidas
isAlive :: Jogador -> Bool
isAlive (Jogador _ _ v _ _) = v > 0

-- | Testa se o jogador tem lasers disponíveis
temLaser :: Jogador -> Bool
temLaser (Jogador _ _ _ l _) = l > 0
-- | Testa se o jogador tem choques disponíveis
temChoque :: Jogador -> Bool
temChoque (Jogador _ _ _ _ c) = c > 0

-- | Testa se a direção que quero jogar é igual à direção do jogador
testaDirecao :: Jogador -> Direcao -> Bool
testaDirecao (Jogador _ d _ _ _) dir = dir == d

-- | Testa se o jogador tem parede para onde quer ir
isParede :: Mapa -> Jogador -> Direcao -> Bool
isParede m (Jogador (x,y) _ _ _ _) d = case d of 
                                           C -> iNotVazio (x-1,y) m && iNotVazio (x-1,y+1) m
                                           D -> iNotVazio (x,y+2) m && iNotVazio (x+1,y+2) m
                                           B -> iNotVazio (x+2,y) m && iNotVazio (x+2,y+1) m
                                           E -> iNotVazio (x,y-1) m && iNotVazio (x+1,y-1) m

-- | Testa se um bloco é não vazio
iNotVazio :: Posicao -> Mapa -> Bool
iNotVazio p m = encontraPosicaoMatriz p m == Bloco Indestrutivel || encontraPosicaoMatriz p m == Bloco Destrutivel

-- | Testa se tem algum jogador vivo a bloquear o movimento
isBlocked :: Int -> Direcao -> Estado -> Jogador -> Bool
isBlocked _ dir e j = case dir of 
                        C -> elem (x-2,y-1) pos || elem (x-2,y) pos || elem (x-2,y+1) pos
                        D -> elem (x-1,y+2) pos || elem (x,y+2) pos || elem (x+1,y+2) pos
                        B -> elem (x+2,y-1) pos || elem (x+2,y) pos || elem (x+2,y+1) pos
                        E -> elem (x-1,y-2) pos || elem (x,y-2) pos || elem (x+1,y-2) pos
                        where pos = jp (jogadoresEstado e)
                              (x,y) = posicaoJogador j

-- | Dá uma lista de posições de uma lista de jogadores vivos
jp :: [Jogador] -> [PosicaoGrelha] 
jp [] = []
jp (h:t) = if isAlive h then posicaoJogador h : jp t
           else jp t 

-- | Testa se um jogador está em choque
jogEmChoque :: Jogador -> [Jogador] -> Bool
jogEmChoque _ [] = False
jogEmChoque j (h:t) = elem pos (areaChoque h) || jogEmChoque j t
                      where pos = posicaoJogador j

-- | Dá uma lista de jogadores que jogaram o choque
indexJog :: Estado -> [Jogador]
indexJog (Estado _ _ []) = []
indexJog (Estado m js (h:t)) = case h of
                                 DisparoChoque i _ -> (js!!i):indexJog (Estado m js t)

-- | Mete todas as posições onde há choque numa lista
areaChoque :: Jogador -> [PosicaoGrelha]
areaChoque j = [(x-3,y-3),(x-3,y-2),(x-3,y-1),(x-3,y),(x-3,y+1),(x-3,y+2),(x-3,y+3),
                (x-2,y-3),(x-2,y-2),(x-2,y-1),(x-2,y),(x-2,y+1),(x-2,y+2),(x-2,y+3),
                (x-1,y-3),(x-1,y-2),(x-1,y-1),(x-1,y),(x-1,y+1),(x-1,y+2),(x-1,y+3),
                (x,y-3),(x,y-2),(x,y-1),(x,y+1),(x,y+2),(x,y+3),
                (x+1,y-3),(x+1,y-2),(x+1,y-1),(x+1,y),(x+1,y+1),(x+1,y+2),(x+1,y+3),
                (x+2,y-3),(x+2,y-2),(x+2,y-1),(x+2,y),(x+2,y+1),(x+2,y+2),(x+2,y+3),
                (x+3,y-3),(x+3,y-2),(x+3,y-1),(x+3,y),(x+3,y+1),(x+3,y+2),(x+3,y+3)]
             where (x,y) = posicaoJogador j

-- | Jogador perde lasers
perdeLaser :: Jogador -> Jogador
perdeLaser (Jogador p d v l c) = Jogador p d v (l-1) c
-- | Jogador perde choques
perdeChoque :: Jogador -> Jogador
perdeChoque (Jogador p d v l c) = Jogador p d v l (c-1)

-- | MOVIMENTA
movimenta :: Int -> Estado -> Direcao -> Estado
movimenta i e dir = if testaDirecao j dir
                    then Estado m (take i js ++ [moveJogador j dir] ++ drop (i+1) js) ds 
                    else Estado m (take i js ++ [mudaDir j dir] ++ drop (i+1) js) ds
                    where j     = jogadoresEstado e !! i
                          js    = jogadoresEstado e
                          ds    = disparosEstado e
                          m     = mapaEstado e

-- | Movimenta o jogador na direção correspondente
moveJogador :: Jogador -> Direcao -> Jogador
moveJogador (Jogador (x,y) d v l c) dir = case dir of
                                    C -> Jogador (x-1,y) d v l c
                                    D -> Jogador (x,y+1) d v l c
                                    B -> Jogador (x+1,y) d v l c
                                    E -> Jogador (x,y-1) d v l c

-- | Muda a direção do jogador caso não esteja na direção do movimento
mudaDir :: Jogador -> Direcao -> Jogador
mudaDir (Jogador p _ v l c) dir = Jogador p dir v l c

-- DISPARA
-- | Dispara Canhão
canhao :: Int -> Estado -> Estado
canhao i e = Estado m js (addTiro d ds)
             where m = mapaEstado e
                   js = jogadoresEstado e
                   ds = disparosEstado e
                   p = posicaoJogador (jogadoresEstado e !! i)
                   dir = direcaoJogador (jogadoresEstado e !! i)
                   d = DisparoCanhao i (posTiro p dir) dir

-- | Dispara Laser
laser :: Int -> Estado -> Estado
laser i e = Estado m (take i js ++ [perdeLaser j] ++ drop (i+1) js) (addTiro d ds)
             where j = jogadoresEstado e !! i
                   m = mapaEstado e
                   js = jogadoresEstado e
                   ds = disparosEstado e
                   p = posicaoJogador (jogadoresEstado e !! i)
                   dir = direcaoJogador (jogadoresEstado e !! i)
                   d = DisparoLaser i (posTiro p dir) dir
                
-- | Dispara Choque
choque :: Int -> Estado -> Estado
choque i e = Estado m (take i js ++ [perdeChoque j] ++ drop (i+1) js) (addTiro d ds)
             where  
                j = jogadoresEstado e !! i
                m = mapaEstado e
                js = jogadoresEstado e
                ds = disparosEstado e
                d = DisparoChoque i 5 -- 5 ticks que demora o choque

-- | Adiciona um disparo a uma lista de disparos
addTiro :: Disparo -> [Disparo] -> [Disparo]
addTiro d l = l ++ [d]

-- | Adiciona o tiro no sítio certo
posTiro :: PosicaoGrelha -> Direcao -> PosicaoGrelha
posTiro (x,y) dir = case dir of
                    C -> (x-1,y)
                    D -> (x,y+1)
                    B -> (x+1,y)
                    E -> (x,y-1)

-- * Testes

-- | Testes unitários da Tarefa 2.
--
-- Cada teste é um triplo (/identificador do 'Jogador'/,/'Jogada' a efetuar/,/'Estado' anterior/).
testesT2 :: [(Int,Jogada,Estado)]
testesT2 = [(0, Movimenta C, Estado (mapaInicial (6,6)) [Jogador (1,1) C 1 1 1] []), -- Testa um jogador ir contra uma parede
            (1, Movimenta C, Estado (mapaInicial (6,6)) [Jogador (1,1) C 1 1 1, Jogador (3,2) C 1 1 1] []), -- Testa um confronto entre tanques
            (0, Movimenta C, Estado (mapaInicial (6,6)) [Jogador (2,1) C 1 1 1] []), -- Testa um movimento
            (0, Movimenta C, Estado (mapaInicial (6,6)) [Jogador (2,1) D 1 1 1] []), -- Testa uma mudança de diração
            (1, Movimenta C, Estado (mapaInicial (6,6)) [Jogador (1,1) B 1 1 1, Jogador (3,3) C 1 1 1] []), -- Testa um movimento paralelo
            (1, Movimenta C, Estado (mapaInicial (6,6)) [Jogador (1,1) B 1 1 1, Jogador (3,2) C 1 1 1] [DisparoChoque 0 3]), -- Testa se um jogador se move em choque
            (0, Movimenta B, Estado (mapaInicial (6,6)) [Jogador (1,1) B 1 1 1, Jogador (3,2) C 1 1 1] [DisparoChoque 0 3, DisparoChoque 1 3]), -- Testa dois choques
            (0, Movimenta B, Estado (mapaInicial (6,6)) [Jogador (1,1) C 1 1 1, Jogador (3,2) C 1 1 1] [DisparoChoque 1 3]), -- Testa uma rotação em choque
            (0, Dispara Canhao, Estado (mapaInicial (6,6)) [Jogador (1,1) B 1 1 1, Jogador (3,3) C 1 1 1] []), -- Testa um disparo de canhão em zona livre
            (0, Movimenta B, Estado (mapaInicial (6,6)) [Jogador (1,1) B 1 1 1, Jogador (3,3) C 1 1 1] [DisparoChoque 0 3]), -- Testa movimento com um jogador a jogar o choque
            (0, Movimenta D, Estado (mapaInicial (6,6)) [Jogador (1,1) B 1 1 1, Jogador (3,3) C 1 1 1] [DisparoChoque 0 3]), -- Testa uma mudança de direção com um jogador que jogou o choque
            (0, Dispara Laser, Estado (mapaInicial (6,6)) [Jogador (1,1) B 1 1 1, Jogador (3,2) C 1 1 1] []), -- Testa um disparo de laser
            (0, Dispara Laser, Estado (mapaInicial (6,6)) [Jogador (1,1) B 1 0 1, Jogador (3,2) C 1 1 1] []), -- Testa um disparo de laser sem lasers
            (0, Movimenta B, Estado (mapaInicial (6,6)) [Jogador (1,1) B 0 1 1, Jogador (3,3) C 1 1 1] []), -- Testa se um jogador se mexe sem vidas
            (0, Dispara Choque, Estado (mapaInicial (6,6)) [Jogador (1,1) B 1 1 1, Jogador (3,2) C 1 1 1] []), -- Testa um disparo de choque
            (0, Dispara Canhao, Estado (mapaInicial (6,6)) [Jogador (1,1) B 1 1 1, Jogador (3,2) C 1 1 1] [DisparoChoque 1 3]), -- Testa um tiro de canhão quando está afetado por um choque
            (0, Movimenta B, Estado (mapaInicial (6,6)) [Jogador (1,1) B 1 1 1, Jogador (3,2) C 0 1 1] []), -- Testa se um jogador pode ir contra um morto
            (0, Movimenta E, Estado (mapaInicial (6,6)) [Jogador (1,1) D 1 0 0, Jogador (1,3) D 1 0 0, Jogador (3,1) D 0 0 0, Jogador (3,3) D 1 0 0] []), -- Teste dos docentes que deu errado.
            (0, Movimenta B, Estado (mapaInicial (6,6)) [Jogador (2,1) B 1 0 0, Jogador (3,2) D 0 0 0] []), -- Testa se um jogador pode andar numa área de um jogador morto
            (0, Movimenta B, Estado (mapaInicial (6,6)) [Jogador (1,1) B 1 0 0, Jogador (3,3) D 1 0 0] [DisparoChoque 0 3, DisparoChoque 1 3])] -- Testa se um choque colide com outro
