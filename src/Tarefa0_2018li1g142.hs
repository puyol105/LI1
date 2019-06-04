-- | Este módulo define funções genéricas sobre vetores e matrizes, que serão úteis na resolução do trabalho prático.
module Tarefa0_2018li1g142 where

import LI11819
import Data.List

-- * Funções não-recursivas.

-- | Um 'Vetor' é uma 'Posicao' em relação à origem.
type Vetor = Posicao 
-- ^ <<http://oi64.tinypic.com/mhvk2x.jpg vetor>>

-- ** Funções sobre vetores

-- *** Funções gerais sobre 'Vetor'es.

-- | Soma dois 'Vetor'es.
somaVetores :: Vetor -> Vetor -> Vetor
somaVetores (a,b) (x,y) = (a+x, b+y)

-- | Subtrai dois 'Vetor'es.
subtraiVetores :: Vetor -> Vetor -> Vetor
subtraiVetores (a,b) (x,y) = (a-x, b-y)

-- | Multiplica um escalar por um 'Vetor'.
multiplicaVetor :: Int -> Vetor -> Vetor
multiplicaVetor x (a,b) = (a*x, b*x)

-- | Roda um 'Vetor' 90º no sentido dos ponteiros do relógio, alterando a sua direção sem alterar o seu comprimento (distância à origem).
--
-- <<http://oi65.tinypic.com/2j5o268.jpg rodaVetor>>x
rodaVetor :: Vetor -> Vetor
rodaVetor (x,y) = (y,-x)

-- | Espelha um 'Vetor' na horizontal (sendo o espelho o eixo vertical).
--
-- <<http://oi63.tinypic.com/jhfx94.jpg inverteVetorH>>
inverteVetorH :: Vetor -> Vetor
inverteVetorH (a,b) = (a, -b)

-- | Espelha um 'Vetor' na vertical (sendo o espelho o eixo horizontal).
--
-- <<http://oi68.tinypic.com/2n7fqxy.jpg inverteVetorV>>
inverteVetorV :: Vetor -> Vetor
inverteVetorV (a,b) = (-a, b)

-- *** Funções do trabalho sobre 'Vetor'es.

-- | Devolve um 'Vetor' unitário (de comprimento 1) com a 'Direcao' dada.
direcaoParaVetor :: Direcao -> Vetor
direcaoParaVetor d = case d of
                          C -> (-1,0)
                          D -> (0, 1)
                          E -> (0,-1)
                          B -> (1, 0)

-- ** Funções sobre listas

-- *** Funções gerais sobre listas.
--
-- Funções não disponíveis no 'Prelude', mas com grande utilidade.

-- | Verifica se o indice pertence à lista.
eIndiceListaValido :: Int -> [a] -> Bool
eIndiceListaValido x l | length l > x && x >= 0 = True
                       | otherwise = False

-- ** Funções sobre matrizes.

-- *** Funções gerais sobre matrizes.

-- | Uma matriz é um conjunto de elementos a duas dimensões.
--
-- Em notação matemática, é geralmente representada por:
--
-- <<https://upload.wikimedia.org/wikipedia/commons/d/d8/Matriz_organizacao.png matriz>>
type Matriz a = [[a]]

-- | Calcula a dimensão de uma matriz.
--  TODO
-- __NB:__ Note que não existem matrizes de dimensão /m * 0/ ou /0 * n/, e que qualquer matriz vazia deve ter dimensão /0 * 0/.
dimensaoMatriz :: Matriz a -> Dimensao
dimensaoMatriz [] = (0,0)
dimensaoMatriz [[]] = (0,0)
dimensaoMatriz ([]:_) = (0,0) 
dimensaoMatriz m | null (head m) = (0,0)
                 | otherwise = (length m, length (head m))

-- | Verifica se a posição pertence à matriz.
ePosicaoMatrizValida :: Posicao -> Matriz a -> Bool
ePosicaoMatrizValida (x,y) m | (x<0) || (y<0) = False
                             | (x <= (fst (dimensaoMatriz m)-1)) && (y <= (snd (dimensaoMatriz m)-1)) = True
                             | otherwise = False
                   
-- | Verifica se a posição está numa borda da matriz.
eBordaMatriz :: Posicao -> Matriz a -> Bool
eBordaMatriz (x,y) m | x == 0 = True
                     | y == 0 = True
                     | x == (length m-1) = True
                     | y == (length (head m)-1) = True
                     | otherwise = False

-- *** Funções do trabalho sobre matrizes.

-- | Converte um 'Tetromino' (orientado para cima) numa 'Matriz' de 'Bool'.
--
-- <<http://oi68.tinypic.com/m8elc9.jpg tetrominos>>
tetrominoParaMatriz :: Tetromino -> Matriz Bool
tetrominoParaMatriz t = case t of
                        I -> replicate 4 [False,True,False,False]
                        J -> j
                        L -> inverteMatrizH j
                        O -> replicate 2 [True,True]
                        S -> s
                        T -> replicate 3 False:replicate 3 True:[[False,True,False]]
                        Z -> inverteMatrizH s
                        where j = replicate 2 [False,True,False] ++ [[True,True,False]]
                              s = [False,True,True]:[True,True,False]:[replicate 3 False]

-- * Funções recursivas.

-- ** Funções sobre listas.
--
-- Funções não disponíveis no 'Prelude', mas com grande utilidade.

-- | Devolve o elemento num dado índice de uma lista. Só pode ser utilizada sabendo que o indice existe.
encontraIndiceLista :: Int -> [a] -> a
encontraIndiceLista x l = l !! x

-- | Modifica um elemento num dado índice.
--
-- __NB:__ Devolve a própria lista se o elemento não existir.
atualizaIndiceLista :: Int -> a -> [a] -> [a]
atualizaIndiceLista x a l | length l > x = take x l ++ a:drop (x+1) l
                          | otherwise = l

-- ** Funções sobre matrizes.
-- | Roda uma 'Matriz' 90º no sentido inverso dos ponteiros do relógio.
rodaMatrizI :: Matriz a -> Matriz a 
rodaMatrizI = reverse . transpose

-- | Roda uma 'Matriz' 90º no sentido dos ponteiros do relógio.
-- <<http://oi68.tinypic.com/21deluw.jpg rodaMatriz>>
rodaMatriz :: Matriz a -> Matriz a 
rodaMatriz = map reverse . transpose

-- | Inverte uma 'Matriz' na horizontal.
--
-- <<http://oi64.tinypic.com/iwhm5u.jpg inverteMatrizH>>
inverteMatrizH :: Matriz a -> Matriz a
inverteMatrizH = map reverse

-- | Inverte uma 'Matriz' na vertical.
--
-- <<http://oi64.tinypic.com/11l563p.jpg inverteMatrizV>>
inverteMatrizV :: Matriz a -> Matriz a
inverteMatrizV = reverse

-- | Cria uma nova 'Matriz' com o mesmo elemento.
criaMatriz :: Dimensao -> a -> Matriz a
criaMatriz (y,x) a = replicate y l
                    where l = replicate x a

-- | Devolve o elemento numa dada 'Posicao' de uma 'Matriz'.
encontraPosicaoMatriz :: Posicao -> Matriz a -> a
encontraPosicaoMatriz p@(x,y) m | ePosicaoMatrizValida p m = m !! x !! y
                                | otherwise = head (head m)

-- | Modifica um elemento numa dada 'Posicao'
--
-- __NB:__ Devolve a própria 'Matriz' se o elemento não existir.
atualizaPosicaoMatriz :: Posicao -> a -> Matriz a -> Matriz a
atualizaPosicaoMatriz p@(x,y) a m | ePosicaoMatrizValida p m = take x m ++ (atualizaIndiceLista y a (m !! x):drop (x+1) m)
                                  | otherwise = m
