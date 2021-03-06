-- | Este módulo define funções comuns da Tarefa 4 do trabalho prático.
module Tarefa4_2018li1g142 where

import LI11819
import Tarefa0_2018li1g142
import Tarefa1_2018li1g142
import Tarefa2_2018li1g142

-- * Testes
-- | Testes unitários da Tarefa 4.
--
-- Cada teste é um 'Estado'.
testesT4 :: [Estado]
testesT4 = [Estado (mapaInicial (6,6)) [Jogador (1,1) C 1 1 1] [DisparoChoque 0 3], -- Testa a decrementação de um choque
            Estado (mapaInicial (6,6)) [Jogador (1,1) C 1 1 1] [DisparoChoque 0 0], -- Testa o fim de um choque
            Estado (mapaInicial (6,6)) [Jogador (1,1) B 1 1 1] [DisparoCanhao 0 (4,1) B], -- Testa se uma bala desaparece ao esbarrar numa parede indestrutivel
            Estado (mapaInicial (8,8)) [Jogador (1,1) B 1 1 1, Jogador (4,4) E 1 1 1] [DisparoCanhao 0 (4,1) B, DisparoCanhao 1 (4,1) E], -- Testa duas balas em direções de 90º no mesmo ponto
            Estado (mapaInicial (8,8)) [Jogador (1,1) B 1 1 1, Jogador (4,1) C 1 1 1] [DisparoCanhao 1 (2,1) C], -- Testa a morte de um jogador
            Estado (mapaInicial (8,8)) [Jogador (1,1) B 2 1 1, Jogador (4,1) C 1 1 1] [DisparoCanhao 1 (2,1) C], -- Testa um jogador a perder vida
            Estado (mapaInicial (8,8)) [Jogador (1,1) B 1 1 1, Jogador (4,2) C 1 1 1, Jogador (1,3) B 2 1 1] [DisparoCanhao 1 (2,2) C], -- Testa dois jogadores a perder vida num único tiro
            Estado (mapaInicial (8,8)) [Jogador (4,1) B 0 1 1, Jogador (4,4) E 1 1 1] [DisparoCanhao 1 (4,2) E], -- Testa um tiro num jogador morto
            Estado (mapaInicial (8,8)) [Jogador (1,1) B 0 1 1, Jogador (4,1) C 1 1 1] [DisparoCanhao 0 (2,1) D, DisparoChoque 1 3], -- Testa um canhão numa área de choque
            Estado (mapaInicial (8,8)) [Jogador (1,1) B 1 1 1, Jogador (3,1) B 2 1 1, Jogador (5,1) C 1 1 1] [DisparoLaser 2 (4,1) C], -- Testa a morte e a perda de vida de dois jogadores
            Estado (mapaInicial (8,8)) [Jogador (1,1) B 1 1 1, Jogador (4,4) E 1 1 1] [DisparoCanhao 0 (4,1) B, DisparoLaser 1 (4,3) E], -- Testa a destruição de uma bala de canhão por um laser.
            Estado (mapaInicial (6,6)) [Jogador (1,1) B 1 1 1, Jogador (3,3) E 1 1 1] [DisparoChoque 0 3, DisparoLaser 1 (3,2) E], -- Testa um laser a passar por um choque
            Estado (mapaInicial (6,6)) [Jogador (1,1) B 1 1 1, Jogador (3,3) B 1 1 1] [DisparoChoque 0 3, DisparoChoque 1 3], -- Testa a sobreposicao de dois choques
            Estado (mapaInicial (8,8)) [Jogador (1,1) B 1 1 1, Jogador (3,3) E 1 1 1] [DisparoLaser 0 (2,1) B, DisparoLaser 1 (3,2) E], -- Testa a sobreposicao de dois laser
            Estado (mapaInicial (8,8)) [Jogador (1,1) B 1 1 1, Jogador (5,5) C 1 1 1] [DisparoCanhao 0 (4,1) B, DisparoCanhao 1 (3,1) C]] -- Testa dois canhoes que passaram um pelo outro 

-- * Funções principais da Tarefa 4.

-- | Avança o 'Estado' do jogo um 'Tick' de tempo.
--
-- __NB:__ Apenas os 'Disparo's afetam o 'Estado' do jogo com o passar do tempo.
--
-- __NB:__ Deve chamar as funções 'tickChoques', 'tickCanhoes' e 'tickLasers' pela ordem definida.
tick :: Estado -- ^ O 'Estado' anterior.
     -> Estado -- ^ O 'Estado' após um 'Tick'.
tick = tickChoques . tickCanhoes . tickLasers

-- | Avança o 'Estado' do jogo um 'Tick' de tempo, considerando apenas os efeitos dos tiros de 'Laser' disparados.
tickLasers :: Estado -> Estado
tickLasers e = exeLaser e (listaLasers ds)
             where ds = disparosEstado e

-- | Avança o 'Estado' do jogo um 'Tick' de tempo, considerando apenas os efeitos das balas de 'Canhao' disparadas.
tickCanhoes :: Estado -> Estado 
tickCanhoes e = addOutros (exeCanhao e (listaCanhoes ds)) (listaLasers ds) (listaChoques ds)
              where ds = disparosEstado e

-- | Avança o 'Estado' do jogo um 'Tick' de tempo, considerando apenas os efeitos dos campos de 'Choque' disparados.
tickChoques :: Estado -> Estado
tickChoques e@(Estado m js ds) = if listaChoques ds /= []
                                 then Estado m js (listaLasers ds ++ listaCanhoes ds ++ exeChoque (listaChoques ds))
                                 else e

-- CHOQUE
-- | Executa os choques.
exeChoque :: [Disparo] -> [Disparo]
exeChoque [] = []
exeChoque (h:t) = if fimChoque h then exeChoque t
                                 else decrementaChoque h : exeChoque t

-- | Testa se um choque vai acabar.
fimChoque :: Disparo -> Bool
fimChoque (DisparoChoque _ t) = t == 0

-- | Decrementa o tempo do choque.
decrementaChoque :: Disparo -> Disparo
decrementaChoque (DisparoChoque i t) = DisparoChoque i (t-1)   

-- CANHÃO
-- | Executa os canhões.
exeCanhao :: Estado -> [Disparo] -> Estado
exeCanhao e [] = e
exeCanhao e ds@(h:t) = case v of
                        0 -> exeCanhao (Estado m js (incrementaCanhao h:t)) t
                        1 -> exeCanhao (Estado m js (removeBalas h ds)) (removeBalas h ds)
                        2 -> exeCanhao (Estado (mudaMapa h m) js t) t
                        3 -> exeCanhao (Estado m (lostVidas h js) t) t 
                        where v   = porqueExplode e h
                              m   = mapaEstado e
                              js  = jogadoresEstado e

-- | Devolve o motivo da explusão.
porqueExplode :: Estado -> Disparo -> Int
porqueExplode e d | confrontoBalas d (drop 1 (listaCanhoes ds)) = 1
                  | temParede d m                               = 2
                  | temJogador js d                             = 3
                  | otherwise                                   = 0
                  where ds = disparosEstado e
                        js = jogadoresEstado e
                        m  = mapaEstado e

-- | Adiciona os outros tiros à lista com os canhões efetuados.
addOutros :: Estado -> [Disparo] -> [Disparo] -> Estado
addOutros e dlas dcho = Estado m js (dlas ++ dcan ++ dcho)
                      where m    = mapaEstado e
                            js   = jogadoresEstado e
                            dcan = disparosEstado e  

-- | Avança com a bala do canhão no caso de ele poder avançar.
incrementaCanhao :: Disparo -> Disparo
incrementaCanhao (DisparoCanhao i (x,y) dir) = case dir of 
                                             C -> DisparoCanhao i (x-1,y) dir
                                             D -> DisparoCanhao i (x,y+1) dir
                                             B -> DisparoCanhao i (x+1,y) dir
                                             E -> DisparoCanhao i (x,y-1) dir

-- | Remove as balas que estão na mesma posição do disparo.
removeBalas :: Disparo -> [Disparo] -> [Disparo]
removeBalas _ [] = []
removeBalas d (h:t) = if p1 == p2 then removeBalas d t 
                                  else h:removeBalas d t 
                    where p1 = posicaoDisparo d 
                          p2 = posicaoDisparo h

-- | Testa se duas balas de canhão estão na mesma posição.
confrontoBalas :: Disparo -> [Disparo] -> Bool
confrontoBalas _ [] = False
confrontoBalas d (h:t) = (d1 == d2) || confrontoBalas d t
                       where d1 = posicaoDisparo d
                             d2 = posicaoDisparo h

-- | Testa se tem jogadores vivos a ocupar a posição da frente da bala.
temJogador :: [Jogador] -> Disparo -> Bool
temJogador [] _ = False
temJogador (h:t) d@(DisparoCanhao _ (x,y) dir) = case dir of
                                             C -> isAlive h && elem (x-1,y) (allPosJog h) || temJogador t d
                                             D -> isAlive h && elem (x,y+1) (allPosJog h) || temJogador t d
                                             B -> isAlive h && elem (x+1,y) (allPosJog h) || temJogador t d
                                             E -> isAlive h && elem (x,y-1) (allPosJog h) || temJogador t d

-- | Testa se tem parede para onde o disparo segue.
temParede :: Disparo -> Mapa -> Bool
temParede (DisparoCanhao _ (x,y) dir) m = case dir of
                                            C -> iNotVazio (x,y) m || iNotVazio (x,y+1) m
                                            D -> iNotVazio (x,y+1) m || iNotVazio (x+1,y+1) m
                                            B -> iNotVazio (x+1,y) m || iNotVazio (x+1,y+1) m
                                            E -> iNotVazio (x,y) m || iNotVazio (x+1,y) m

-- | Devolve uma lista com todas as PosicaoGrelha que um jogador ocupa.
allPosJog :: Jogador -> [PosicaoGrelha]
allPosJog j = [(x-1,y-1),(x-1,y),(x-1,y+1),
               (x,y-1),(x,y),(x,y+1),
               (x+1,y-1),(x+1,y),(x+1,y+1)]
               where (x,y) = posicaoJogador j

-- | Devolve os jogadores com as vidas perdidas.
lostVidas :: Disparo -> [Jogador] -> [Jogador]
lostVidas _ [] = []
lostVidas d (h:t) = if elem pos (allPosJog h) && isAlive h then perdeVida h:lostVidas d t
                    else h:lostVidas d t
                    where pos = posicaoDisparo d

-- | Jogador perde uma vida.
perdeVida :: Jogador -> Jogador
perdeVida (Jogador p d v l c) = Jogador p d (v-1) l c

-- | Efetua mudanças no mapa.
mudaMapa :: Disparo -> Mapa -> Mapa
mudaMapa d m = case dir of
                    C | temDestrutivel (x,y) m -> if temDestrutivel (x,y+1) m 
                                                  then destroiParede (x,y) (destroiParede (x,y+1) m) 
                                                  else destroiParede (x,y) m
                      | temDestrutivel (x,y+1) m -> destroiParede (x,y+1) m
                      | otherwise -> m
                    D | temDestrutivel (x,y+1) m -> if temDestrutivel (x+1,y+1) m 
                                                    then destroiParede (x,y+1) (destroiParede (x+1,y+1) m) 
                                                    else destroiParede (x,y+1) m
                      | temDestrutivel (x+1,y+1) m -> destroiParede (x+1,y+1) m
                      | otherwise -> m
                    B | temDestrutivel (x+1,y) m -> if temDestrutivel (x+1,y+1) m 
                                                    then destroiParede (x+1,y) (destroiParede (x+1,y+1) m) 
                                                    else destroiParede (x+1,y) m
                      | temDestrutivel (x+1,y+1) m -> destroiParede (x+1,y+1) m
                      | otherwise -> m
                    E | temDestrutivel (x,y) m -> if temDestrutivel (x+1,y) m 
                                                  then destroiParede (x,y) (destroiParede (x+1,y) m) 
                                                  else destroiParede (x,y) m
                      | temDestrutivel (x+1,y) m -> destroiParede (x+1,y) m
                      | otherwise -> m
                    where dir   = direcaoDisparo d
                          (x,y) = posicaoDisparo d

-- | Testa se tem uma parede destrutivel à frente da bala.
temDestrutivel :: Posicao -> Mapa -> Bool
temDestrutivel p m = encontraPosicaoMatriz p m == Bloco Destrutivel

-- | Destroi uma parede destrutivel com um tiro.
destroiParede :: Posicao -> Mapa -> Mapa
destroiParede p = atualizaPosicaoMatriz p Vazia

-- LASER
-- | Executa os lasers
exeLaser :: Estado -> [Disparo] -> Estado 
exeLaser e [] = e
exeLaser e (h:t) = exeLaser (Estado (destroiMapa (expansaoMapa h m) m) (perdemVidas h (expansao h m) js 0) (retiraCanhoes (expansao h m) (listaCanhoes ds) ++ listaChoques ds)) t
                 where m  = mapaEstado e
                       js = jogadoresEstado e
                       ds = disparosEstado e

-- | Retira canhões que passam nos lasers.
retiraCanhoes :: [PosicaoGrelha] -> [Disparo] -> [Disparo]
retiraCanhoes _ [] = []
retiraCanhoes [] l = l
retiraCanhoes pg (h:t) = if pos `elem` pg then retiraCanhoes pg t
                                        else h:retiraCanhoes pg t
                       where pos = posicaoDisparo h

-- | Devolve os jogadores com as vidas perdidas por um único laser.
perdemVidas :: Disparo -> [PosicaoGrelha] -> [Jogador] -> Int -> [Jogador]
perdemVidas _ _ [] _ = []
perdemVidas _ [] l _ = l
perdemVidas d pg (h:t) i = if (isAlive h && not (quemJogou d i)) && deveSofrer pg h 
                           then perdeVida h:perdemVidas d pg t (i+1) 
                           else h:perdemVidas d pg t (i+1)

-- | Testa se um jogador está no espaço do laser
deveSofrer :: [PosicaoGrelha] -> Jogador -> Bool
deveSofrer [] _ = False
deveSofrer (h:t) j = elem h (allPosJog j) || deveSofrer t j

-- | Indica se foi o jogador atual a jogar
quemJogou :: Disparo -> Int -> Bool
quemJogou d n = n == i
              where i = jogadorDisparo d

-- | Devolve uma lista com todas as posições das peças do mapa por onde passa o laser.
expansaoMapa :: Disparo -> Mapa -> [PosicaoGrelha]
expansaoMapa (DisparoLaser i (x,y) dir) m = case dir of
                                        C -> if temIndestrutivel (x,y) m || temIndestrutivel (x,y+1) m
                                             then [(x,y),(x,y+1)]
                                             else (x,y):(x,y+1):expansaoMapa (DisparoLaser i (x-1,y) dir) m
                                        D -> if temIndestrutivel (x,y+1) m || temIndestrutivel (x+1,y+1) m
                                             then [(x,y+1),(x+1,y+1)]
                                             else (x,y+1):(x+1,y+1):expansaoMapa (DisparoLaser i (x,y+1) dir) m
                                        B -> if temIndestrutivel (x+1,y) m || temIndestrutivel (x+1,y+1) m
                                             then [(x+1,y),(x+1,y+1)]
                                             else (x+1,y):(x+1,y+1):expansaoMapa (DisparoLaser i (x+1,y) dir) m
                                        E -> if temIndestrutivel (x,y) m || temIndestrutivel (x+1,y) m
                                             then [(x,y),(x+1,y)]
                                             else (x,y):(x+1,y):expansaoMapa (DisparoLaser i (x,y-1) dir) m

-- | Destroi todas os 'Blocos Destrutiveis' por onde passa o laser
destroiMapa :: [PosicaoGrelha] -> Mapa -> Mapa
destroiMapa [] m = m
destroiMapa (h:t) m = if temIndestrutivel h m then destroiMapa t m 
                                              else destroiMapa t (destroiParede h m) 

-- | Devolve uma lista com todas as 'PosicaoGrelha' por onde passa o laser.
expansao :: Disparo -> Mapa -> [PosicaoGrelha]
expansao (DisparoLaser i (x,y) dir) m = case dir of
                                        C -> if temIndestrutivel (x,y) m || temIndestrutivel (x,y+1) m
                                             then [(x,y)]
                                             else (x,y):expansao (DisparoLaser i (x-1,y) dir) m
                                        D -> if temIndestrutivel (x,y+1) m || temIndestrutivel (x+1,y+1) m
                                             then [(x,y)]
                                             else (x,y):expansao (DisparoLaser i (x,y+1) dir) m
                                        B -> if temIndestrutivel (x+1,y) m || temIndestrutivel (x+1,y+1) m
                                             then [(x,y)]
                                             else (x,y):expansao (DisparoLaser i (x+1,y) dir) m
                                        E -> if temIndestrutivel (x,y) m || temIndestrutivel (x+1,y) m
                                             then [(x,y)]
                                             else (x,y):expansao (DisparoLaser i (x,y-1) dir) m

-- | Testa se tem uma parede inestrutivel à frente da bala.
temIndestrutivel :: Posicao -> Mapa -> Bool
temIndestrutivel p m = encontraPosicaoMatriz p m == Bloco Indestrutivel

-- Listas de disparos separadas
-- | Devolve uma lista apenas com lasers
listaLasers :: [Disparo] -> [Disparo]
listaLasers [] = []
listaLasers (h:t) = case h of
                    DisparoLaser {} -> h:listaLasers t
                    otherwise -> listaLasers t

-- | Devolve uma lista apenas com choques
listaChoques :: [Disparo] -> [Disparo]
listaChoques [] = []
listaChoques (h:t) = case h of
                    DisparoChoque {} -> h:listaChoques t
                    otherwise -> listaChoques t

-- | Devolve uma lista apenas com tiros de canhão
listaCanhoes :: [Disparo] -> [Disparo]
listaCanhoes [] = []
listaCanhoes (h:t) = case h of
                    DisparoCanhao {} -> h:listaCanhoes t
                    otherwise -> listaCanhoes t
