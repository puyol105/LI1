{-|
Module      : Tarefa3_2018li1g142
Description : Comprimir e descomprimir o Estado do jogo
Copyright   : Marco Matias Pereira Gonçalves <a75480@alunos.uminho.pt>
              Renato Jorge Cruzinha da Silva <a75310@alunos.uminho.pt>

= Introdução:
Esta tarefa consta em fazer um ro'bot' que consiga, sem a ajuda de um humano, jogar de um modo desafiante o jogo 'Tanks'. 
Existem muitos pontos importantes a ter em consideração, nomeadamente o facto de o jogo ter no máximo 200 ticks, ao qual 
findados, as restantes vidas do jogador contarão para a pontuação final do mesmo. Sendo assim, o nosso grupo privilegiou 
a utilização de uma postura defensiva, de modo a obter o máximo de vidas possíveis após o fim do jogo.
Contudo, as vidas finais não são o único meio de pontuação, sendo que um jogador pontua sempre que tira uma vida a um
adversário. Tendo em conta a última afirmação, o nosso grupo utilizou os lasers como principal recurso ofensivo, de modo a que 
atinjam, sempre que utilizados, um adversário.

= Objetivos:
Para resolver esta tarefa, adotamos um estilo de jogo defensivo ao nosso ro'bot'. Ou seja, tentar chegar ao fim dos 
200 ticks de jogo com o maior número de vidas possível. Para isso metemos o nosso ro'bot' a disparar um canhão em todas 
as direções, de modo que as balas dos nossos canhões atinjam as balas que vão de encontro ao ro'bot' destruindo as mesmas.
Para isso, o ro'bot' efetua uma sequência de jogadas do seguinte modo: Se jogou uma bala de canhão, então efetua uma rotação
de 90º à sua direita, se não dispara uma bala de canhão. Tendo isto em conta, o nosso ro'bot' a cada sequência de duas jogadas
efetua um 'Just (Dispara Canhao)' e um 'Just (Movimenta *90º à direita*)'. Como os raios laser também causam a perda de vida
ao oponente, e a sua utilização é limitada a 3, o nosso ro'bot' apenas utiliza um quando é certo que irá atingir um oponente.
Ou seja, apenas utiliza um laser quando na direção atual do ro'bot' tem um ponto central de um outro jogador. Isto porque se 
esse mesmo jogador se movimentar para outra posição na jogada que se segue, o laser irá atingi-lo impreterivelmente. Não 
obstante que enquanto o jogador possuir raios laser o seu principal objetivo é jogá-los primeiro, e apenas seguidamente as
balas de canhão, precisamente por os lasers sempre que forem jogados terem a caracteristica de que serão certeiros.

= Conclusão:
Esta tarefa finaliza o trabalho, implementando um ro'bot' capaz de jogar sozinho. É uma tarefa que distingue várias maneiras
de jogar, e privilegia a melhor estratégia. O ro'bot' que o nosso grupo construiu é capaz de jogar um jogo inteiro sem qualquer 
auxílio humano e como tal cumpre os requisitos da mesma. 
-}
module Tarefa6_2018li1g142 where

import LI11819
import Tarefa0_2018li1g142
import Tarefa1_2018li1g142
import Tarefa2_2018li1g142
import Tarefa4_2018li1g142

-- * Funções principais da Tarefa 6.

-- | Define um ro'bot' capaz de jogar autonomamente o jogo.
bot :: Int          -- ^ O identificador do 'Jogador' associado ao ro'bot'.
    -> Estado       -- ^ O 'Estado' para o qual o ro'bot' deve tomar uma decisão.
    -> Maybe Jogada -- ^ Uma possível 'Jogada' a efetuar pelo ro'bot'.
bot i e = if isAlive (jogBot i e) then play i e
                                  else Nothing
                                  
-- | O ro'bot' efetua uma 'Jogada'.
play :: Int -> Estado -> Maybe Jogada
play i e = if jogaLaser i e && temLaser (jogBot i e) then Just (Dispara Laser)  
           else if jogouCanhao i e (botCan i e) || isIndestrutivel (jogBot i e) m 
                then Just (Movimenta (rodaDir (direcaoJogador (jogBot i e))))
                else Just (Dispara Canhao)
         where m = mapaEstado e

-- | Roda a direção 90º no sentido hórario
rodaDir :: Direcao -> Direcao
rodaDir dir = case dir of
                    C -> D
                    D -> B
                    B -> E
                    E -> C

-- | Testa se joga o canhão ou muda de direção.
jogouCanhao :: Int -> Estado -> [Disparo] -> Bool
jogouCanhao i e [] = False
jogouCanhao i e (h:t) = direcaoJogador (jogBot i e) == direcaoDisparo h 

-- | Lista de canhões do 'bot'
botCan :: Int -> Estado -> [Disparo]
botCan i (Estado m js []) = []
botCan i (Estado m js (h:t)) = if jogadorDisparo h == i then h : botCan i (Estado m js t)
                               else botCan i (Estado m js t)

-- | Testa se joga o laser
jogaLaser :: Int -> Estado -> Bool
jogaLaser i e = not (null (filter (`elem` pontosFrente (jogBot i e) m) (posiJogs js)))
                where m  = mapaEstado e
                      js = jogadoresEstado e

-- | Lista de 'PosicaoGrelha' de todos os jogadores vivos
posiJogs :: [Jogador] -> [PosicaoGrelha]
posiJogs [] = []
posiJogs (h:t) = if isAlive h then posicaoJogador h : posiJogs t
                              else posiJogs t

-- | Dá o jogador associado ao bot
jogBot :: Int -> Estado -> Jogador
jogBot i (Estado m (h:t) ds) = if i == 0 then h else jogBot (i-1) (Estado m t ds)

-- | Area de ['PosicaoGrelha'] com os pontos centrais do jogador
pontosFrente :: Jogador -> Mapa -> [PosicaoGrelha]
pontosFrente j@(Jogador (x,y) dir v l c) m = case dir of
                                            C -> if isIndestrutivel j m 
                                                 then []
                                                 else (x-1,y):pontosFrente (Jogador (x-1,y) dir v l c) m
                                            D -> if isIndestrutivel j m 
                                                 then []
                                                 else (x,y+1):pontosFrente (Jogador (x,y+1) dir v l c) m
                                            B -> if isIndestrutivel j m 
                                                 then []
                                                 else (x+1,y):pontosFrente (Jogador (x+1,y) dir v l c) m
                                            E -> if isIndestrutivel j m 
                                                 then []
                                                 else (x,y-1):pontosFrente (Jogador (x,y-1) dir v l c) m

-- | Teste se uma 'PosicaoGrelha' seguinte tem parede indestrutivel.
isIndestrutivel :: Jogador -> Mapa -> Bool
isIndestrutivel (Jogador (x,y) dir _ _ _) m = case dir of
                                            C -> temIndestrutivel (x-1,y) m || temIndestrutivel (x-1,y+1) m
                                            D -> temIndestrutivel (x,y+2) m || temIndestrutivel (x+1,y+2) m
                                            B -> temIndestrutivel (x+2,y) m || temIndestrutivel (x+2,y+1) m
                                            E -> temIndestrutivel (x,y-1) m || temIndestrutivel (x+1,y-1) m
