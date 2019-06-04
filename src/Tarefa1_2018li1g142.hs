-- | Este módulo define funções comuns da Tarefa 1 do trabalho prático.
module Tarefa1_2018li1g142 where

import LI11819
import Tarefa0_2018li1g142

-- * Funções principais da Tarefa 1.

-- | Aplica uma 'Instrucao' num 'Editor'.
--
--    * 'Move' - move numa dada 'Direcao'.
--
--    * 'MudaTetromino' - seleciona a 'Peca' seguinte (usar a ordem léxica na estrutura de dados),
--       sem alterar os outros parâmetros.
--
--    * 'MudaParede' - muda o tipo de 'Parede'.
--
--    * 'Desenha' - altera o 'Mapa' para incluir o 'Tetromino' atual, sem alterar os outros parâmetros.
--
-- __NB:__ Deve assumir que o cursor e tetrominós cabem __sempre__ dentro das bordas do 'Mapa'.
instrucao :: Instrucao -- ^ A 'Instrucao' a aplicar.
          -> Editor    -- ^ O 'Editor' anterior.
          -> Editor    -- ^ O 'Editor' resultante após aplicar a 'Instrucao'.
instrucao i e = case i of
                    Roda          -> Editor pos (newDirecao d) t par m 
                    MudaTetromino -> Editor pos d (nextTetr t) par m 
                    MudaParede    -> Editor pos d t (nextPar par) m
                    Desenha       -> Editor pos d t par (desenhaTetr e)
                    Move C        -> if iFronteira e C then Editor pos d t par (newUpLine m)
                                                       else Editor (decY pos) d t par m
                    Move D        -> if iFronteira e D then Editor (incX pos) d t par (newLineD m)
                                                       else Editor (incX pos) d t par m
                    Move B        -> if iFronteira e B then Editor (incY pos) d t par (newLineB m)
                                                       else Editor (incY pos) d t par m
                    Move E        -> if iFronteira e E then Editor pos d t par (newLineE m)
                                                       else Editor (decX pos) d t par m
                    where pos = posicaoEditor e
                          d   = direcaoEditor e
                          t   = tetrominoEditor e
                          par = paredeEditor e
                          m   = mapaEditor e

-- | Roda a Direção 90º para a direita.
newDirecao :: Direcao -> Direcao
newDirecao d = case d of
                    C -> D
                    D -> B
                    B -> E
                    E -> C

-- | Muda o Tetromino para o proximo.
nextTetr :: Tetromino -> Tetromino
nextTetr t = case t of
                  I -> J
                  J -> L
                  L -> O
                  O -> S
                  S -> T
                  T -> Z
                  Z -> I

-- | Troca o tipo de uma parede.
nextPar :: Parede -> Parede
nextPar p = if p == Indestrutivel then Destrutivel
                                  else Indestrutivel

-- | Testa se o tetrominó esta na borda do mapa.
iFronteira :: Editor -> Direcao -> Bool
iFronteira e d = case d of
                    C -> py <= 1
                    E -> px <= 1 
                    D -> px + length h - 1 >= x
                    B -> py + length tet - 1 >= y
                    
                    where tet@(h:_) = tetrominoParaMatriz (tetrominoEditor e)
                          (px,py) = posicaoEditor e
                          m = mapaEditor e
                          x = (-2) + length (head m)
                          y = (-2) + length m

-- | Recebe uma lista de coordenadas de um tetromino e uma direção e aplica o movimento da direção.
moveDir :: [Posicao] -> Direcao -> [Posicao]
moveDir l d = case d of
                    C -> map decY l
                    D -> map incX l
                    B -> map incY l
                    E -> map decX l
-- | Incrementa X numa posição.
incX :: Posicao -> Posicao
incX (x,y) = (x+1, y)
-- | Incrementa Y numa posição.
incY :: Posicao -> Posicao
incY (x,y) = (x, y+1)
-- | Decrementa X numa posição.
decX :: Posicao -> Posicao
decX (x,y) = (x-1, y)
-- | Decrementa Y numa posição.
decY :: Posicao -> Posicao
decY (x,y) = (x, y-1)

-- | Adiciona uma linha no lado direito da matriz.
newLineD = rodaMatriz . newUpLine . rodaMatrizI

-- | Adiciona uma linha na parte de baixo da matriz.
newLineB = inverteMatrizV . newUpLine . inverteMatrizV

-- | Adiciona uma linha no lado esquerdo da Matriz.
newLineE = rodaMatrizI . newUpLine . rodaMatriz

-- | Adiciona uma linha no topo da matriz.
newUpLine :: Mapa -> Mapa
newUpLine (h:t) = [h] ++ [genLineH (length h)] ++ t

-- | Gera uma linha horizontal para introuzir na matriz.
genLineH :: Int -> [Peca]
genLineH x = [b] ++ replicate (x-2) Vazia ++ [b] 
                where b = Bloco Indestrutivel

-- | Recebe um editor e devolve o mapa povoado.
desenhaTetr :: Editor -> Mapa
desenhaTetr e = povoaMap (mapaEditor e) (desenhaTetrList e) (paredeEditor e)

-- | Devolve um mapa povoado recebendo uma lista de posições.
povoaMap :: Mapa -> [Posicao] -> Parede -> Mapa
povoaMap m [] _ = m
povoaMap m (h:t) p = if ePosicaoMatrizValida (swap h) m 
                        then atualizaPosicaoMatriz (swap h) (Bloco p) (povoaMap m t p)
                        else povoaMap m t p

-- | Função auxiliar de 'povoaMap'.
swap :: (a,b) -> (b,a)
swap (x,y) = (y,x)

-- | Posições nas quais é possivel por blocos segundo aquele tetrominio.
desenhaTetrList :: Editor -> [Posicao]
desenhaTetrList e = posBlocos p (tetrToBool t d) []
                where p = posicaoEditor e
                      t = tetrominoEditor e
                      d = direcaoEditor e

-- | Devolve matriz boleana correspondente ao tetromino direcionado.
tetrToBool :: Tetromino -> Direcao -> Matriz Bool
tetrToBool t d = case d of 
                      C -> tet
                      D -> rodaMatriz tet
                      B -> rodaMatriz (rodaMatriz tet)
                      E -> rodaMatrizI tet
                 where tet = tetrominoParaMatriz t

-- | Devolve a lista das posicoes que podem ser necessarias a trocar no mapa.
posBlocos :: Posicao -> Matriz Bool -> [Posicao] -> [Posicao]
posBlocos _ [] l = l
posBlocos _ [[]] l = l
posBlocos (x,y) (h:t) l = posBlocos (x,y+1) t (posBlocosLinha (x,y) h l)

-- | Auxiliar da posBlocas que tratas a linhas da  matriz.
posBlocosLinha :: Posicao -> [Bool] -> [Posicao] -> [Posicao]
posBlocosLinha _ [] l = l
posBlocosLinha (x,y) (h:t) l = if h 
                            then posBlocosLinha (x+1,y) t ((x,y):l)
                            else posBlocosLinha (x+1,y) t l

-- | Aplica uma sequência de 'Instrucoes' num 'Editor'.
--
-- __NB:__ Deve chamar a função 'instrucao'.
instrucoes :: Instrucoes -- ^ As 'Instrucoes' a aplicar.
           -> Editor     -- ^ O 'Editor' anterior.
           -> Editor     -- ^ O 'Editor' resultante após aplicar as 'Instrucoes'.
instrucoes t e = foldl (flip instrucao) e t

-- | Cria um 'Mapa' inicial com 'Parede's nas bordas e o resto vazio.
mapaInicial :: Dimensao -- ^ A 'Dimensao' do 'Mapa' a criar.
            -> Mapa     -- ^ O 'Mapa' resultante com a 'Dimensao' dada.
mapaInicial (y,x) = [wall] ++ replicate (y-2) auxWall ++ [wall]
                    where p = Bloco Indestrutivel
                          v = Vazia   
                          auxWall = [p] ++ replicate (x-2) v ++ [p]
                          wall = replicate x p

-- | Cria um 'Editor' inicial.
--
-- ... __NB:__ Deve chamar as funções 'mapaInicial', 'dimensaoInicial', e 'posicaoInicial'.
editorInicial :: Instrucoes  -- ^ Uma sequência de 'Instrucoes' de forma a poder calcular a  'dimensaoInicial' e a 'posicaoInicial'.
              -> Editor      -- ^ O 'Editor' inicial, usando a 'Peca' 'I' 'Indestrutivel' voltada para 'C'.
editorInicial _ = Editor (1,1) C I Indestrutivel (mapaInicial (6,6))
-- test ignorei posicao inicial e dimensao inicial
-- | Constrói um 'Mapa' dada uma sequência de 'Instrucoes'.
--
-- ... __NB:__ Deve chamar as funções 'Instrucoes' e 'editorInicial'.
constroi :: Instrucoes -- ^ Uma sequência de 'Instrucoes' dadas a um 'Editor' para construir um 'Mapa'.
         -> Mapa       -- ^ O 'Mapa' resultante.
constroi is = mapaEditor  $ instrucoes is (editorInicial is)


-- | Testes unitários da Tarefa 1.
--
-- Cada teste é uma sequência de 'Instrucoes'.
testesT1 :: [Instrucoes] 
testesT1 = [[],
            [Roda],
            [Move D],
            [Move E],
            [Move C],
            [Move B],
            [Desenha],
            [Move C, Move C],
            [Move C,Move C,Move C,Move D,Move D,Move B,MudaTetromino,MudaTetromino,MudaParede,Move D,Move E,Move E,Move C,Move C],
            [Move C,Move C,Move C,Move D,Move D,Move B,Desenha],
            [Move C,Move C,Move C,Move D,Move D,Move B,Desenha,MudaTetromino,MudaTetromino,MudaParede,Move D,Move E,Move E,Move C,Move C,Desenha],
            [Move C,Move C,Move C,Desenha,MudaTetromino,Move D,Move D,Move D, Move D, Move D, Move D,Desenha],
            [MudaTetromino,MudaParede,Move D],
            [MudaTetromino,MudaParede,Desenha],
            [MudaTetromino,MudaParede,Move D,Desenha],
            [MudaTetromino,MudaTetromino,MudaTetromino,MudaTetromino,MudaTetromino,MudaTetromino,Desenha],
            [MudaTetromino,MudaTetromino,MudaTetromino,MudaTetromino,MudaTetromino,Desenha],
            [MudaTetromino,MudaTetromino,MudaTetromino,MudaTetromino,Desenha],
            [MudaTetromino,MudaTetromino,MudaTetromino,Desenha],
            [MudaTetromino,MudaTetromino,Desenha],
            [MudaTetromino,Desenha],
            [Desenha,Move C,Move C,Move E,Move B,Move B,Move D,Move D,Move C,MudaTetromino,Move C,Roda,MudaParede,Desenha,Move E,Move C,Move E,MudaTetromino,Move C,Move E,Move E,Move C,Desenha,Move C,Move D,Move D,MudaParede,Move D,Move E,Desenha,Move D,MudaParede,Desenha,Move D,Desenha,Move C,Move E,Move C,Roda,MudaTetromino,Move B,Desenha,Move E,Move C,Move D,Move D,Roda,Move D,Desenha,Move D,Move B,MudaParede,Move E,Move E,Move B,Desenha,Move B,Move D,Move B,Move B,Move B,Move E,Move E,Move E,Move E,Move B,Desenha],
            [Move C,Move C,Move C,Move C,Move D,Move D,Move D,Move D,Move D,Move D,Move D,Move B,Move B,Move B,Move B,Move B,Move B,Move B,Move B,Move E,Move E,Move E,Move E,Move E,Move E,Move E,Move E,Move E,Move E,Move C,Move D,Move C,Move C,Move C,Move E,Move D,Move C,Move C,MudaTetromino,MudaTetromino,Move D,Move E,MudaTetromino,Move D,Move D,MudaParede,Move E,Move C,Desenha,Move D,Desenha,Move D,Desenha,Move B,Move B,Desenha,Move E,Move E,Desenha,Move B,Move B,Desenha,Move D,Move D,Desenha,Move B,Move B,Desenha,Move E,Move E,Desenha,Move D,Move D,Move D,Move D,Move D,Move D,Desenha,Move D,Move D,Desenha,Move C,Move C,Desenha,Move E,Move E,Desenha,Move C,Move C,Desenha,Move D,Move D,Desenha,Move C,Move C,Desenha,Move E,Move E,Desenha,Move C,Move C,Move C,Move B,Move D,Move D,Move D,Move D,Move D,Move E,Move E,Move E,Move E,Move E,Move E,Move E,Move E,Move E,Move E,Move E,Move E,Move E,Move E,Move B,Move D,Move D,Move D,Move B,MudaParede,Move B,Move D,Desenha,Move B,Move B,Move B,Move B,Move C,Move B,Desenha,Move D,Move D,Move D,Move D,Move D,Move D,Desenha,Move C,Move C,Move C,Move C,Desenha]
            ]
