{-|
Module      : Tarefa5_2018li1g142
Description : Criação gráfica do jogo.
Copyright   : Marco Matias Pereira Gonçalves <a75480@alunos.uminho.pt>
              Renato Jorge Cruzinha da Silva <a75310@alunos.uminho.pt>

= Introdução:
Nesta tarefa foi nos proposto que fosse efetuada a componente gráfica do jogo 'Tanks'. O necessário era criar usando as bibliotecas
Gloss.

= Objetivos:
Criar todos os componentes gráficos desta tarefa, incluindo todas as imagens, menus de jogo e toda a interação existente entre
a máquina e o utilizador. Nesta tarefa é também possível fazer adições de objetivos que não sejam obrigatórios da UC. Foi nesta
arefa que o nosso grupo teve maior dificuldade, e como tal tem muitos aspetos a melhores e a concretizar.

= Conclusão:
Como esta tarefa foi a mais que mais dificuldades nos apresentou, não está completamente resolvida, e a que consideramos que
obtivemos menos resultados de aprendizagem em comparação com todas as outras tarefas.
-}

module Tarefa5_2018li1g142 where

import LI11819
import Tarefa0_2018li1g142
import Tarefa1_2018li1g142
import Tarefa2_2018li1g142
import Tarefa4_2018li1g142
import Graphics.Gloss       
import Graphics.Gloss.Data.Picture  
import Graphics.Gloss.Interface.Pure.Game
import Data.List
-- | Imagens do Jogo
data Pics = Pics
    { p1      :: Picture  -- ^ Imagems do jogador 1
    , p2      :: Picture  -- ^ Imagems do jogador 1
    , p3      :: Picture  -- ^ Imagems do jogador 1
    , p4      :: Picture  -- ^ Imagems do jogador 1
    , vaz     :: Picture  -- ^ Bloco Vazio
    , des     :: Picture  -- ^ Bloco Destrutivel
    , ind     :: Picture  -- ^ Bloco Indestrutivel
    , laserP  :: Picture  -- ^ Imagem do Laser
    , canhaoP :: Picture  -- ^ Imagem do canhao
    , choqueP :: Picture  -- ^ imagem do choque
    , menu0   :: Picture  -- ^ Escolher o numero de Jogadores
    , menu1   :: Picture  -- ^ 1º Menu 
    , menu2   :: Picture  -- ^ 2º Menu
    , menu3   :: Picture  -- ^ Menu Escolher Mapa
    , menuP   :: Picture  -- ^ Menu Pausa
    , escala  :: Float    -- ^ Escala das imgens
    }
    deriving (Show,Eq)

-- | Controlos do Jogo
data Keys = Keys
    { p1Up      :: Event  -- ^ Chave que movimenta o jogador 1 para Cima
    , p1Down    :: Event  -- ^ Chave que movimenta o jogador 1 para Baixo
    , p1Left    :: Event  -- ^ Chave que movimenta o jogador 1 para Esquerda
    , p1Rigth   :: Event  -- ^ Chave que movimenta o jogador 1 para Direita
    , p1Canhao  :: Event  -- ^ Chave que o jogador 1 dispara o canhao
    , p1Laser   :: Event  -- ^ Chave que o jogador 1 dispara o laser
    , p1Choque  :: Event  -- ^ Chave que o jogador 1 dispara o choque
    , p2Up      :: Event  -- ^ Chave que movimenta o jogador 2 para Cima
    , p2Down    :: Event  -- ^ Chave que movimenta o jogador 2 para Baixo
    , p2Left    :: Event  -- ^ Chave que movimenta o jogador 2 para Esquerda
    , p2Rigth   :: Event  -- ^ Chave que movimenta o jogador 2 para Direita
    , p2Canhao  :: Event  -- ^ Chave que o jogador 2 dispara o canhao
    , p2Laser   :: Event  -- ^ Chave que o jogador 2 dispara o laser
    , p2Choque  :: Event  -- ^ Chave que o jogador 2 dispara o choque
    , p3Up      :: Event  -- ^ Chave que movimenta o jogador 3 para Cima
    , p3Down    :: Event  -- ^ Chave que movimenta o jogador 3 para Baixo
    , p3Left    :: Event  -- ^ Chave que movimenta o jogador 3 para Esquerda
    , p3Rigth   :: Event  -- ^ Chave que movimenta o jogador 3 para Direita
    , p3Canhao  :: Event  -- ^ Chave que o jogador 3 dispara o canhao
    , p3Laser   :: Event  -- ^ Chave que o jogador 3 dispara o laser
    , p3Choque  :: Event  -- ^ Chave que o jogador 3 dispara o choque
    , p4Up      :: Event  -- ^ Chave que movimenta o jogador 4 para Cima
    , p4Down    :: Event  -- ^ Chave que movimenta o jogador 4 para Baixo
    , p4Left    :: Event  -- ^ Chave que movimenta o jogador 4 para Esquerda
    , p4Rigth   :: Event  -- ^ Chave que movimenta o jogador 4 para Direita
    , p4Canhao  :: Event  -- ^ Chave que o jogador 4 dispara o canhao
    , p4Laser   :: Event  -- ^ Chave que o jogador 4 dispara o laser
    , p4Choque  :: Event  -- ^ Chave que o jogador 4 dispara o choque
    , pausaJ    :: Event  -- ^ Pausa no Jogo
    }
    deriving (Show,Eq)
-- | Keys Deafult
keysDefaultJogo = Keys (EventKey (Char 'w')            Down (Modifiers Down Down Down) (0,0))
                       (EventKey (Char 's')            Down (Modifiers Down Down Down) (0,0))
                       (EventKey (Char 'a')            Down (Modifiers Down Down Down) (0,0))
                       (EventKey (Char 'd')            Down (Modifiers Down Down Down) (0,0))
                       (EventKey (Char '1')            Down (Modifiers Down Down Down) (0,0))
                       (EventKey (Char '2')            Down (Modifiers Down Down Down) (0,0))
                       (EventKey (Char '3')            Down (Modifiers Down Down Down) (0,0))
                       (EventKey (Char 't')            Down (Modifiers Down Down Down) (0,0))
                       (EventKey (Char 'g')            Down (Modifiers Down Down Down) (0,0))
                       (EventKey (Char 'f')            Down (Modifiers Down Down Down) (0,0))
                       (EventKey (Char 'h')            Down (Modifiers Down Down Down) (0,0))
                       (EventKey (Char '4')            Down (Modifiers Down Down Down) (0,0))
                       (EventKey (Char '5')            Down (Modifiers Down Down Down) (0,0))
                       (EventKey (Char '6')            Down (Modifiers Down Down Down) (0,0))
                       (EventKey (Char 'i')            Down (Modifiers Down Down Down) (0,0))
                       (EventKey (Char 'k')            Down (Modifiers Down Down Down) (0,0))
                       (EventKey (Char 'j')            Down (Modifiers Down Down Down) (0,0))
                       (EventKey (Char 'l')            Down (Modifiers Down Down Down) (0,0))
                       (EventKey (Char '7')            Down (Modifiers Down Down Down) (0,0))
                       (EventKey (Char '8')            Down (Modifiers Down Down Down) (0,0))
                       (EventKey (Char '9')            Down (Modifiers Down Down Down) (0,0))
                       (EventKey (SpecialKey KeyUp)    Down (Modifiers Down Down Down) (0,0))
                       (EventKey (SpecialKey KeyDown)  Down (Modifiers Down Down Down) (0,0))
                       (EventKey (SpecialKey KeyLeft)  Down (Modifiers Down Down Down) (0,0))
                       (EventKey (SpecialKey KeyRight) Down (Modifiers Down Down Down) (0,0))
                       (EventKey (Char ',')            Down (Modifiers Down Down Down) (0,0))
                       (EventKey (Char '.')            Down (Modifiers Down Down Down) (0,0))
                       (EventKey (Char '-')            Down (Modifiers Down Down Down) (0,0))
                       (EventKey (SpecialKey KeyF1)    Down (Modifiers Down Down Down) (0,0))

-- | Banco de comandos do editor
data KeysEd = KeysEd
    { up      :: Event -- ^ Move Para cima
    , down    :: Event -- ^ Move Para baixo
    , left    :: Event -- ^ Move Para esquerda
    , rigth   :: Event -- ^ Move Para direita
    , roda    :: Event -- ^ Roda Tertromino
    , mudaT   :: Event -- ^ Muda Tetromino
    , mudaP   :: Event -- ^ Muda parede
    , desenha :: Event -- ^ Desenha Tertromino
    , restart :: Event -- ^ Recomeçar a desenhar
    , sair    :: Event -- ^ Sair do menu editor
    }
    deriving (Show,Eq)
-- | KeysEd Deafult
keysDefaultEd = KeysEd (EventKey (SpecialKey KeyUp)    Down (Modifiers Down Down Down) (0,0))
                (EventKey (SpecialKey KeyDown)  Down (Modifiers Down Down Down) (0,0))
                (EventKey (SpecialKey KeyLeft)  Down (Modifiers Down Down Down) (0,0))
                (EventKey (SpecialKey KeyRight) Down (Modifiers Down Down Down) (0,0))
                (EventKey (Char 'r')            Down (Modifiers Down Down Down) (0,0))
                (EventKey (Char 't')            Down (Modifiers Down Down Down) (0,0))
                (EventKey (Char 'p')            Down (Modifiers Down Down Down) (0,0))
                (EventKey (Char 'd')            Down (Modifiers Down Down Down) (0,0))
                (EventKey (SpecialKey KeyF5)    Down (Modifiers Down Down Down) (0,0))
                (EventKey (SpecialKey KeyF1)    Down (Modifiers Down Down Down) (0,0))

data KeyGen = KeyGen
    { um     :: Event
    , dois   :: Event
    , tres   :: Event
    , quatro :: Event
    , cinco  :: Event 
    }
    deriving (Show,Eq)

-- | Controlos iniciais default para escolhas nos menus
keysDefaultMenu = KeyGen (EventKey (Char '1') Down (Modifiers Up Up Up) (0,0))
                         (EventKey (Char '2') Down (Modifiers Up Up Up) (0,0))
                         (EventKey (Char '3') Down (Modifiers Up Up Up) (0,0))
                         (EventKey (Char '4') Down (Modifiers Up Up Up) (0,0))
                         (EventKey (Char '5') Down (Modifiers Up Up Up) (0,0))

data KeyBank = KeyBank
    { keyGamePlay :: Keys       -- ^ Banco de controladores do Jogo
    , keyEditor   :: KeysEd     -- ^ Banco de chaves do Editor
    , keyGeneral  :: KeyGen     -- ^ Chaves que serao usadas para configurar o jogo
    }
    deriving (Show,Eq)

data Sitio = Sitio
    { pausaS         :: Bool  -- ^ Jogo esta em pausa
    , jogo           :: Bool  -- ^ Jogo começou
    , jogadores      :: Bool  -- ^ O numero de jogadores esta defenido
    , mapaDefenido   :: Bool  -- ^ O mapa esta escolhido
    , escolherMapa   :: Bool  -- ^ Menu escolher Mapa
    , editarMapa     :: Bool  -- ^ Menu CriarMapa
    , editarComandos :: Bool  -- ^ Menu Editar Controlos
    , escolheNovo    :: Bool  -- ^ Está á espera do novo evento para editar os controlos
    , antigoEvent    :: Event -- ^ Evento que vai ser substituido
    , proibioTrocar  :: Bool  -- ^ Evento pode ou nao ser substituido 
    }
    deriving (Show,Eq)

-- | Valores inicias do Sitio
sitioDefault = Sitio False False False False False False False False (EventKey (Char ' ') Down (Modifiers Down Down Down) (0,0)) False

data World = World
    { estado :: Estado   -- ^ Estado do Jogo
    , keys   :: KeyBank  -- ^ Banco de commandos
    , pics   :: Pics     -- ^ Banco de Imagens
    , editor :: Editor   -- ^ Editor para criar o Estado Inicial
    , sitio  :: Sitio    -- ^ Identifica onde estou
    }
    deriving (Show,Eq)

comparaEvent :: Event -> Event -> Bool
comparaEvent e@(EventKey k1 _ _ _) (EventKey k2 _ _ _) = if (isDown e && k1 == k2) then True
                                                                                   else False
comparaEvent _ _ = False

isDown :: Event -> Bool
isDown e = x == "Down"
         where x = head $ drop 3 $ take 4 $ words $ show e

-- | Testa se o evento pretence aos controladores do menu do jogo
isCommandMenu :: Event -> KeyGen -> Int
isCommandMenu e k
    | comparaEvent e (um k     ) = 1 
    | comparaEvent e (dois k   ) = 2 
    | comparaEvent e (tres k   ) = 3 
    | comparaEvent e (quatro k ) = 4
    | comparaEvent e (cinco k  ) = 5
    | otherwise = 0
-- | Testa se o evento pretence aos controladores do menu editor
isCommandEditor :: Event -> KeysEd -> Int
isCommandEditor e k
    | comparaEvent e (up k     ) = 1
    | comparaEvent e (down k   ) = 2
    | comparaEvent e (left k   ) = 3
    | comparaEvent e (rigth k  ) = 4
    | comparaEvent e (roda k   ) = 5
    | comparaEvent e (mudaT k  ) = 6
    | comparaEvent e (mudaP k  ) = 7
    | comparaEvent e (desenha k) = 8
    | comparaEvent e (restart k) = 9
    | comparaEvent e (sair k   ) = 10
    | otherwise = 0
-- | Testa se o evento pretence as keys do jogo
isCommand :: Event -> Keys -> Int
isCommand e k
    | comparaEvent e (pausaJ k  ) = -1
    | comparaEvent e (p1Up k    ) = 1 
    | comparaEvent e (p1Down k  ) = 2 
    | comparaEvent e (p1Left k  ) = 3 
    | comparaEvent e (p1Rigth k ) = 4 
    | comparaEvent e (p1Canhao k) = 5 
    | comparaEvent e (p1Laser k ) = 6 
    | comparaEvent e (p1Choque k) = 7 
    | comparaEvent e (p2Up k    ) = 8 
    | comparaEvent e (p2Down k  ) = 9 
    | comparaEvent e (p2Left k  ) = 10
    | comparaEvent e (p2Rigth k ) = 11
    | comparaEvent e (p2Canhao k) = 12
    | comparaEvent e (p2Laser k ) = 13
    | comparaEvent e (p2Choque k) = 14
    | comparaEvent e (p3Up k    ) = 15
    | comparaEvent e (p3Down k  ) = 16
    | comparaEvent e (p3Left k  ) = 17
    | comparaEvent e (p3Rigth k ) = 18
    | comparaEvent e (p3Canhao k) = 19
    | comparaEvent e (p3Laser k ) = 20
    | comparaEvent e (p3Choque k) = 21
    | comparaEvent e (p4Up k    ) = 22
    | comparaEvent e (p4Down k  ) = 23
    | comparaEvent e (p4Left k  ) = 24
    | comparaEvent e (p4Rigth k ) = 25
    | comparaEvent e (p4Canhao k) = 26
    | comparaEvent e (p4Laser k ) = 27
    | comparaEvent e (p4Choque k) = 28
    | otherwise = 0

-- | Funcao que adiciona os Jogadores condiçoes 
reageMenu0 :: Event -> World -> World 
reageMenu0 e w@(World es ks p ed (Sitio pa jg js md esm em ec edc ev po)) 
    = case i of
           1 -> World (Estado m [onePlayer] d)                 ks p ed s
           2 -> World (Estado m [onePlayer,onePlayer] d)       ks p ed s
           3 -> World (Estado m (take 3 (repeat onePlayer)) d) ks p ed s
           4 -> World (Estado m (take 4 (repeat onePlayer)) d) ks p ed s
           otherwise -> w
    where k = keyGeneral ks
          i = isCommandMenu e k
          Estado m j d = es
          onePlayer = Jogador (0,0) D 6 3 3
          s = Sitio pa jg True md esm em ec edc ev po

-- | Função que reage ao Menu1   condiçoes 
reageMenu1 :: Event -> World -> World
reageMenu1 e w@(World es ks p ed (Sitio pa jg js md esm em ec edc ev po)) 
    = case i of
           1 -> World es ks p ed $ Sitio pa jg js md True em ec edc ev po
           2 -> World es ks p ed $ Sitio pa jg False md esm em ec edc ev po
           3 -> World es ks p ed $ Sitio pa jg js md esm em True False ev False
           otherwise -> w
    where k = keyGeneral ks
          i = isCommandMenu e k

-- | Funcao que reage ao menu2 condicoes
reageMenu2 :: Event -> World -> World
reageMenu2 e w@(World es ks p ed s@(Sitio pa jg js md esm em ec edc ev po))
    = case i of
           1 -> World (meteJogadores ed es) ks p ed $ Sitio pa True js md esm em ec edc ev po
           2 -> World es ks p ed $ Sitio pa jg js False True em ec edc ev po
           3 -> World es ks p ed $ Sitio pa jg False md esm em ec edc ev po
           4 -> World es ks p ed $ Sitio pa jg js md esm em True False ev False
           otherwise -> w
    where k = keyGeneral ks    
          i = isCommandMenu e k

-- | Mapas Default
mapa1 = [[Bloco Indestrutivel,Bloco Indestrutivel,Bloco Indestrutivel,Bloco Indestrutivel,Bloco Indestrutivel,Bloco Indestrutivel,Bloco Indestrutivel,Bloco Indestrutivel,Bloco Indestrutivel,Bloco Indestrutivel,Bloco Indestrutivel,Bloco Indestrutivel,Bloco Indestrutivel,Bloco Indestrutivel,Bloco Indestrutivel,Bloco Indestrutivel,Bloco Indestrutivel,Bloco Indestrutivel],[Bloco Indestrutivel,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Bloco Indestrutivel],[Bloco Indestrutivel,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Bloco Indestrutivel],[Bloco Indestrutivel,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Bloco Indestrutivel],[Bloco Indestrutivel,Vazia,Vazia,Vazia,Bloco Destrutivel,Bloco Destrutivel,Bloco Destrutivel,Bloco Destrutivel,Vazia,Vazia,Bloco Destrutivel,Bloco Destrutivel,Bloco Destrutivel,Bloco Destrutivel,Vazia,Vazia,Vazia,Bloco Indestrutivel],[Bloco Indestrutivel,Vazia,Vazia,Vazia,Bloco Destrutivel,Bloco Indestrutivel,Bloco Indestrutivel,Bloco Destrutivel,Vazia,Vazia,Bloco Destrutivel,Bloco Indestrutivel,Bloco Indestrutivel,Bloco Destrutivel,Vazia,Vazia,Vazia,Bloco Indestrutivel],[Bloco Indestrutivel,Vazia,Vazia,Vazia,Bloco Destrutivel,Bloco Indestrutivel,Bloco Indestrutivel,Bloco Destrutivel,Vazia,Vazia,Bloco Destrutivel,Bloco Indestrutivel,Bloco Indestrutivel,Bloco Destrutivel,Vazia,Vazia,Vazia,Bloco Indestrutivel],[Bloco Indestrutivel,Vazia,Vazia,Vazia,Bloco Destrutivel,Bloco Destrutivel,Bloco Destrutivel,Bloco Destrutivel,Vazia,Vazia,Bloco Destrutivel,Bloco Destrutivel,Bloco Destrutivel,Bloco Destrutivel,Vazia,Vazia,Vazia,Bloco Indestrutivel],[Bloco Indestrutivel,Vazia,Vazia,Vazia,Bloco Destrutivel,Bloco Destrutivel,Bloco Destrutivel,Bloco Destrutivel,Vazia,Vazia,Bloco Destrutivel,Bloco Destrutivel,Bloco Destrutivel,Bloco Destrutivel,Vazia,Vazia,Vazia,Bloco Indestrutivel],[Bloco Indestrutivel,Vazia,Vazia,Vazia,Bloco Destrutivel,Bloco Indestrutivel,Bloco Indestrutivel,Bloco Destrutivel,Vazia,Vazia,Bloco Destrutivel,Bloco Indestrutivel,Bloco Indestrutivel,Bloco Destrutivel,Vazia,Vazia,Vazia,Bloco Indestrutivel],[Bloco Indestrutivel,Vazia,Vazia,Vazia,Bloco Destrutivel,Bloco Indestrutivel,Bloco Indestrutivel,Bloco Destrutivel,Vazia,Vazia,Bloco Destrutivel,Bloco Indestrutivel,Bloco Indestrutivel,Bloco Destrutivel,Vazia,Vazia,Vazia,Bloco Indestrutivel],[Bloco Indestrutivel,Vazia,Vazia,Vazia,Bloco Destrutivel,Bloco Destrutivel,Bloco Destrutivel,Bloco Destrutivel,Vazia,Vazia,Bloco Destrutivel,Bloco Destrutivel,Bloco Destrutivel,Bloco Destrutivel,Vazia,Vazia,Vazia,Bloco Indestrutivel],[Bloco Indestrutivel,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Bloco Indestrutivel],[Bloco Indestrutivel,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Bloco Indestrutivel],[Bloco Indestrutivel,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Bloco Indestrutivel],[Bloco Indestrutivel,Bloco Indestrutivel,Bloco Indestrutivel,Bloco Indestrutivel,Bloco Indestrutivel,Bloco Indestrutivel,Bloco Indestrutivel,Bloco Indestrutivel,Bloco Indestrutivel,Bloco Indestrutivel,Bloco Indestrutivel,Bloco Indestrutivel,Bloco Indestrutivel,Bloco Indestrutivel,Bloco Indestrutivel,Bloco Indestrutivel,Bloco Indestrutivel,Bloco Indestrutivel]]
mapa2 = [[Bloco Indestrutivel,Bloco Indestrutivel,Bloco Indestrutivel,Bloco Indestrutivel,Bloco Indestrutivel,Bloco Indestrutivel,Bloco Indestrutivel,Bloco Indestrutivel,Bloco Indestrutivel,Bloco Indestrutivel],[Bloco Indestrutivel,Vazia,Vazia,Vazia,Vazia,Vazia,Bloco Destrutivel,Bloco Destrutivel,Vazia,Bloco Indestrutivel],[Bloco Indestrutivel,Vazia,Vazia,Vazia,Bloco Destrutivel,Bloco Destrutivel,Bloco Destrutivel,Bloco Destrutivel,Vazia,Bloco Indestrutivel],[Bloco Indestrutivel,Vazia,Vazia,Vazia,Bloco Destrutivel,Bloco Indestrutivel,Bloco Indestrutivel,Vazia,Vazia,Bloco Indestrutivel],[Bloco Indestrutivel,Vazia,Vazia,Bloco Indestrutivel,Bloco Destrutivel,Bloco Indestrutivel,Bloco Indestrutivel,Bloco Destrutivel,Vazia,Bloco Indestrutivel],[Bloco Indestrutivel,Bloco Destrutivel,Bloco Destrutivel,Bloco Indestrutivel,Bloco Destrutivel,Bloco Destrutivel,Vazia,Vazia,Vazia,Bloco Indestrutivel],[Bloco Indestrutivel,Bloco Destrutivel,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Bloco Indestrutivel],[Bloco Indestrutivel,Vazia,Vazia,Vazia,Vazia,Bloco Destrutivel,Vazia,Vazia,Vazia,Bloco Indestrutivel],[Bloco Indestrutivel,Vazia,Bloco Indestrutivel,Bloco Indestrutivel,Vazia,Bloco Destrutivel,Bloco Destrutivel,Bloco Destrutivel,Vazia,Bloco Indestrutivel],[Bloco Indestrutivel,Vazia,Bloco Indestrutivel,Bloco Indestrutivel,Vazia,Bloco Indestrutivel,Vazia,Vazia,Vazia,Bloco Indestrutivel],[Bloco Indestrutivel,Vazia,Vazia,Vazia,Vazia,Bloco Indestrutivel,Vazia,Vazia,Vazia,Bloco Indestrutivel],[Bloco Indestrutivel,Vazia,Vazia,Vazia,Vazia,Bloco Indestrutivel,Vazia,Vazia,Vazia,Bloco Indestrutivel],[Bloco Indestrutivel,Vazia,Vazia,Vazia,Vazia,Bloco Indestrutivel,Vazia,Vazia,Vazia,Bloco Indestrutivel],[Bloco Indestrutivel,Bloco Indestrutivel,Bloco Indestrutivel,Bloco Indestrutivel,Bloco Indestrutivel,Bloco Indestrutivel,Bloco Indestrutivel,Bloco Indestrutivel,Bloco Indestrutivel,Bloco Indestrutivel]]
mapa3 = [[Bloco Indestrutivel,Bloco Indestrutivel,Bloco Indestrutivel,Bloco Indestrutivel,Bloco Indestrutivel,Bloco Indestrutivel],[Bloco Indestrutivel,Vazia,Bloco Indestrutivel,Vazia,Vazia,Bloco Indestrutivel],[Bloco Indestrutivel,Vazia,Bloco Indestrutivel,Vazia,Vazia,Bloco Indestrutivel],[Bloco Indestrutivel,Bloco Indestrutivel,Bloco Indestrutivel,Vazia,Vazia,Bloco Indestrutivel],[Bloco Indestrutivel,Vazia,Vazia,Vazia,Vazia,Bloco Indestrutivel],[Bloco Indestrutivel,Bloco Indestrutivel,Bloco Indestrutivel,Bloco Indestrutivel,Bloco Indestrutivel,Bloco Indestrutivel]]
mapa4 = [[Bloco Indestrutivel,Bloco Indestrutivel,Bloco Indestrutivel,Bloco Indestrutivel,Bloco Indestrutivel,Bloco Indestrutivel,Bloco Indestrutivel,Bloco Indestrutivel,Bloco Indestrutivel,Bloco Indestrutivel,Bloco Indestrutivel,Bloco Indestrutivel,Bloco Indestrutivel,Bloco Indestrutivel,Bloco Indestrutivel,Bloco Indestrutivel,Bloco Indestrutivel,Bloco Indestrutivel,Bloco Indestrutivel,Bloco Indestrutivel,Bloco Indestrutivel,Bloco Indestrutivel,Bloco Indestrutivel],[Bloco Indestrutivel,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Bloco Indestrutivel],[Bloco Indestrutivel,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Bloco Indestrutivel,Bloco Indestrutivel,Bloco Indestrutivel,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Bloco Indestrutivel],[Bloco Indestrutivel,Vazia,Vazia,Bloco Indestrutivel,Bloco Indestrutivel,Bloco Indestrutivel,Bloco Indestrutivel,Vazia,Vazia,Bloco Indestrutivel,Bloco Indestrutivel,Bloco Indestrutivel,Bloco Indestrutivel,Bloco Indestrutivel,Vazia,Vazia,Bloco Indestrutivel,Bloco Indestrutivel,Bloco Indestrutivel,Bloco Indestrutivel,Vazia,Vazia,Bloco Indestrutivel],[Bloco Indestrutivel,Vazia,Vazia,Bloco Indestrutivel,Bloco Indestrutivel,Bloco Indestrutivel,Bloco Indestrutivel,Vazia,Vazia,Bloco Indestrutivel,Bloco Indestrutivel,Bloco Indestrutivel,Bloco Indestrutivel,Bloco Indestrutivel,Vazia,Vazia,Bloco Indestrutivel,Bloco Indestrutivel,Bloco Indestrutivel,Bloco Indestrutivel,Vazia,Vazia,Bloco Indestrutivel],[Bloco Indestrutivel,Vazia,Vazia,Bloco Destrutivel,Bloco Destrutivel,Vazia,Vazia,Vazia,Vazia,Vazia,Bloco Indestrutivel,Bloco Indestrutivel,Bloco Indestrutivel,Vazia,Vazia,Vazia,Vazia,Vazia,Bloco Destrutivel,Bloco Destrutivel,Vazia,Vazia,Bloco Indestrutivel],[Bloco Indestrutivel,Vazia,Vazia,Bloco Destrutivel,Vazia,Vazia,Vazia,Bloco Destrutivel,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Bloco Destrutivel,Vazia,Vazia,Vazia,Bloco Destrutivel,Vazia,Vazia,Bloco Indestrutivel],[Bloco Indestrutivel,Vazia,Vazia,Bloco Destrutivel,Vazia,Vazia,Vazia,Bloco Destrutivel,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Bloco Destrutivel,Vazia,Vazia,Vazia,Bloco Destrutivel,Vazia,Vazia,Bloco Indestrutivel],[Bloco Indestrutivel,Bloco Indestrutivel,Bloco Indestrutivel,Vazia,Vazia,Vazia,Bloco Destrutivel,Bloco Destrutivel,Bloco Indestrutivel,Bloco Indestrutivel,Bloco Destrutivel,Bloco Destrutivel,Bloco Destrutivel,Bloco Indestrutivel,Bloco Indestrutivel,Bloco Destrutivel,Bloco Destrutivel,Vazia,Vazia,Vazia,Bloco Indestrutivel,Bloco Indestrutivel,Bloco Indestrutivel],[Bloco Indestrutivel,Bloco Indestrutivel,Bloco Indestrutivel,Vazia,Vazia,Vazia,Bloco Indestrutivel,Bloco Indestrutivel,Bloco Indestrutivel,Bloco Indestrutivel,Bloco Destrutivel,Bloco Destrutivel,Bloco Destrutivel,Bloco Indestrutivel,Bloco Indestrutivel,Bloco Indestrutivel,Bloco Indestrutivel,Vazia,Vazia,Vazia,Bloco Indestrutivel,Bloco Indestrutivel,Bloco Indestrutivel],[Bloco Indestrutivel,Bloco Indestrutivel,Bloco Indestrutivel,Vazia,Vazia,Vazia,Bloco Destrutivel,Bloco Destrutivel,Bloco Indestrutivel,Bloco Indestrutivel,Bloco Destrutivel,Bloco Destrutivel,Bloco Destrutivel,Bloco Indestrutivel,Bloco Indestrutivel,Bloco Destrutivel,Bloco Destrutivel,Vazia,Vazia,Vazia,Bloco Indestrutivel,Bloco Indestrutivel,Bloco Indestrutivel],[Bloco Indestrutivel,Vazia,Vazia,Bloco Destrutivel,Vazia,Vazia,Vazia,Bloco Destrutivel,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Bloco Destrutivel,Vazia,Vazia,Vazia,Bloco Destrutivel,Vazia,Vazia,Bloco Indestrutivel],[Bloco Indestrutivel,Vazia,Vazia,Bloco Destrutivel,Vazia,Vazia,Vazia,Bloco Destrutivel,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Bloco Destrutivel,Vazia,Vazia,Vazia,Bloco Destrutivel,Vazia,Vazia,Bloco Indestrutivel],[Bloco Indestrutivel,Vazia,Vazia,Bloco Destrutivel,Bloco Destrutivel,Vazia,Vazia,Vazia,Vazia,Vazia,Bloco Indestrutivel,Bloco Indestrutivel,Bloco Indestrutivel,Vazia,Vazia,Vazia,Vazia,Vazia,Bloco Destrutivel,Bloco Destrutivel,Vazia,Vazia,Bloco Indestrutivel],[Bloco Indestrutivel,Vazia,Vazia,Bloco Indestrutivel,Bloco Indestrutivel,Bloco Indestrutivel,Bloco Indestrutivel,Vazia,Vazia,Bloco Indestrutivel,Bloco Indestrutivel,Bloco Indestrutivel,Bloco Indestrutivel,Bloco Indestrutivel,Vazia,Vazia,Bloco Indestrutivel,Bloco Indestrutivel,Bloco Indestrutivel,Bloco Indestrutivel,Vazia,Vazia,Bloco Indestrutivel],[Bloco Indestrutivel,Vazia,Vazia,Bloco Indestrutivel,Bloco Indestrutivel,Bloco Indestrutivel,Bloco Indestrutivel,Vazia,Vazia,Bloco Indestrutivel,Bloco Indestrutivel,Bloco Indestrutivel,Bloco Indestrutivel,Bloco Indestrutivel,Vazia,Vazia,Bloco Indestrutivel,Bloco Indestrutivel,Bloco Indestrutivel,Bloco Indestrutivel,Vazia,Vazia,Bloco Indestrutivel],[Bloco Indestrutivel,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Bloco Indestrutivel,Bloco Indestrutivel,Bloco Indestrutivel,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Bloco Indestrutivel],[Bloco Indestrutivel,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Bloco Indestrutivel],[Bloco Indestrutivel,Bloco Indestrutivel,Bloco Indestrutivel,Bloco Indestrutivel,Bloco Indestrutivel,Bloco Indestrutivel,Bloco Indestrutivel,Bloco Indestrutivel,Bloco Indestrutivel,Bloco Indestrutivel,Bloco Indestrutivel,Bloco Indestrutivel,Bloco Indestrutivel,Bloco Indestrutivel,Bloco Indestrutivel,Bloco Indestrutivel,Bloco Indestrutivel,Bloco Indestrutivel,Bloco Indestrutivel,Bloco Indestrutivel,Bloco Indestrutivel,Bloco Indestrutivel,Bloco Indestrutivel]]

-- | Função que reage ao Menu3 (Escolher Mapa) 
reageMenu3 :: Event -> World -> World
reageMenu3 e w@(World es ks p ed@(Editor pos dir t par m) (Sitio pa jg js md esm em ec edc ev po)) 
    = case i of
           1 -> World es ks p (Editor pos dir t par mapa1) $ Sitio pa jg js True False em ec edc ev po
           2 -> World es ks p (Editor pos dir t par mapa2) $ Sitio pa jg js True False em ec edc ev po
           3 -> World es ks p (Editor pos dir t par mapa3) $ Sitio pa jg js True False em ec edc ev po
           4 -> World es ks p (Editor pos dir t par mapa4) $ Sitio pa jg js True False em ec edc ev po
           5 -> World es ks p ed $ Sitio pa jg js md esm True ec edc ev po
           otherwise -> w
    where k = keyGeneral ks
          i = isCommandMenu e k

-- | Funcao auxiiliar que compara o comando recebido com os que a aplicação tem guardados devolvendo um tupulo com o identificador do jogador e com a jogada a executar
reageJogo :: Event -> World -> World
reageJogo e w@(World es ks p ed s@(Sitio pa jg js md esm em ec edc ev po))
    = case i of 
           (-1) -> World es ks p ed $ Sitio True jg js md esm em ec edc ev po
           1    -> World (jogada 0 (Movimenta C)    es) ks p ed s 
           2    -> World (jogada 0 (Movimenta B)    es) ks p ed s 
           3    -> World (jogada 0 (Movimenta E)    es) ks p ed s 
           4    -> World (jogada 0 (Movimenta D)    es) ks p ed s 
           5    -> World (jogada 0 (Dispara Canhao) es) ks p ed s 
           6    -> World (jogada 0 (Dispara Laser ) es) ks p ed s 
           7    -> World (jogada 0 (Dispara Choque) es) ks p ed s 
           8    -> World (jogada 1 (Movimenta C)    es) ks p ed s 
           9    -> World (jogada 1 (Movimenta B)    es) ks p ed s 
           10   -> World (jogada 1 (Movimenta E)    es) ks p ed s 
           11   -> World (jogada 1 (Movimenta D)    es) ks p ed s 
           12   -> World (jogada 1 (Dispara Canhao) es) ks p ed s 
           13   -> World (jogada 1 (Dispara Laser ) es) ks p ed s 
           14   -> World (jogada 1 (Dispara Choque) es) ks p ed s 
           15   -> World (jogada 2 (Movimenta C)    es) ks p ed s 
           16   -> World (jogada 2 (Movimenta B)    es) ks p ed s 
           17   -> World (jogada 2 (Movimenta E)    es) ks p ed s 
           18   -> World (jogada 2 (Movimenta D)    es) ks p ed s 
           19   -> World (jogada 2 (Dispara Canhao) es) ks p ed s 
           20   -> World (jogada 2 (Dispara Laser ) es) ks p ed s 
           21   -> World (jogada 2 (Dispara Choque) es) ks p ed s 
           22   -> World (jogada 3 (Movimenta C)    es) ks p ed s 
           23   -> World (jogada 3 (Movimenta B)    es) ks p ed s 
           24   -> World (jogada 3 (Movimenta E)    es) ks p ed s 
           25   -> World (jogada 3 (Movimenta D)    es) ks p ed s 
           26   -> World (jogada 3 (Dispara Canhao) es) ks p ed s 
           27   -> World (jogada 3 (Dispara Laser ) es) ks p ed s 
           28   -> World (jogada 3 (Dispara Choque) es) ks p ed s 
           otherwise -> w
    where i = isCommand e $ keyGamePlay ks

-- | Reage á pausa do jogo
reagePausa :: Event -> World -> World
reagePausa e w@(World es ks p ed s@(Sitio pa jg js md esm em ec edc ev po))
    = case i of
           1 -> World es ks p ed $ Sitio False jg js md esm em ec edc ev po
           2 -> w -- TODO
           3 -> World es ks p ed $ Sitio pa jg js md esm em True False ev False
           4 -> World es ks p ed $ Sitio False False js md esm em ec edc ev po
           otherwise -> w
    where k = keyGeneral ks
          i = isCommandMenu e k

-- | Funcao que reage ao editor
reageEditor :: Event -> World -> World
reageEditor e w@(World es ks p ed s@(Sitio pa jg js md esm em ec edc ev po))
    = case i of
           1  -> World es ks p (instrucao (Move C)      ed) s
           2  -> World es ks p (instrucao (Move B)      ed) s
           3  -> World es ks p (instrucao (Move E)      ed) s
           4  -> World es ks p (instrucao (Move D)      ed) s
           5  -> World es ks p (instrucao Roda          ed) s
           6  -> World es ks p (instrucao MudaTetromino ed) s
           7  -> World es ks p (instrucao MudaParede    ed) s
           8  -> World es ks p (instrucao Desenha       ed) s
           9  -> World es ks p (editorInicial []) s
           10 -> World es ks p ed (Sitio pa jg js True False False ec edc ev po)
           otherwise -> w
    where k = keyEditor ks
          i = isCommandEditor e k

-- | Escolhe qual o comando a ser trocado
reageEditarC :: Event -> World -> World
reageEditarC e w@(World es ks p ed (Sitio pa jg js md esm em ec edc ev po))  
    = if (isDown e)
        then if (isCommand e k > 0)
            then World es ks p ed (Sitio pa jg js md esm em ec True e po)
            else if (isCommand e k == -1)
                then World es ks p ed (Sitio pa jg js md esm em False False ev False)
                else w
        else w
    where k = keyGamePlay ks

-- | Reage á nova tecla
reageTrocaC :: Event -> World -> World
reageTrocaC e w@(World es ks p ed (Sitio pa jg js md esm em ec edc ev po)) 
    = if(isDown e)
        then if (i == 0)
            then World es (trocaChave e ev ks) p ed (Sitio pa jg js md esm em ec False ev False)
            else if(i > 0) 
                then World es ks p ed (Sitio pa jg js md esm em ec edc ev True)
                else World es ks p ed (Sitio pa jg js md esm em False False ev False)
        else w
    where k = keyGamePlay ks
          i = isCommand e k

-- | Troca o comando utilizando a 'auxTroca'
trocaChave :: Event -> Event -> KeyBank -> KeyBank
trocaChave e ev (KeyBank kj ke kg) = KeyBank (auxTroca e (isCommand ev kj) kj) ke kg

-- | Devolve novas Keys
auxTroca :: Event -> Int -> Keys -> Keys
auxTroca e i k@(Keys p1C p1B p1E p1D p1Ca p1La p1Ch p2C p2B p2E p2D p2Ca p2La p2Ch p3C p3B p3E p3D p3Ca p3La p3Ch p4C p4B p4E p4D p4Ca p4La p4Ch pause)
    = case i of
           0  -> k
           1  -> Keys e p1B p1E p1D p1Ca p1La p1Ch p2C p2B p2E p2D p2Ca p2La p2Ch p3C p3B p3E p3D p3Ca p3La p3Ch p4C p4B p4E p4D p4Ca p4La p4Ch pause
           2  -> Keys p1C e p1E p1D p1Ca p1La p1Ch p2C p2B p2E p2D p2Ca p2La p2Ch p3C p3B p3E p3D p3Ca p3La p3Ch p4C p4B p4E p4D p4Ca p4La p4Ch pause
           3  -> Keys p1C p1B e p1D p1Ca p1La p1Ch p2C p2B p2E p2D p2Ca p2La p2Ch p3C p3B p3E p3D p3Ca p3La p3Ch p4C p4B p4E p4D p4Ca p4La p4Ch pause
           4  -> Keys p1C p1B p1E e p1Ca p1La p1Ch p2C p2B p2E p2D p2Ca p2La p2Ch p3C p3B p3E p3D p3Ca p3La p3Ch p4C p4B p4E p4D p4Ca p4La p4Ch pause
           5  -> Keys p1C p1B p1E p1D e p1La p1Ch p2C p2B p2E p2D p2Ca p2La p2Ch p3C p3B p3E p3D p3Ca p3La p3Ch p4C p4B p4E p4D p4Ca p4La p4Ch pause
           6  -> Keys p1C p1B p1E p1D p1Ca e p1Ch p2C p2B p2E p2D p2Ca p2La p2Ch p3C p3B p3E p3D p3Ca p3La p3Ch p4C p4B p4E p4D p4Ca p4La p4Ch pause
           7  -> Keys p1C p1B p1E p1D p1Ca p1La e p2C p2B p2E p2D p2Ca p2La p2Ch p3C p3B p3E p3D p3Ca p3La p3Ch p4C p4B p4E p4D p4Ca p4La p4Ch pause
           8  -> Keys p1C p1B p1E p1D p1Ca p1La p1Ch e p2B p2E p2D p2Ca p2La p2Ch p3C p3B p3E p3D p3Ca p3La p3Ch p4C p4B p4E p4D p4Ca p4La p4Ch pause
           9  -> Keys p1C p1B p1E p1D p1Ca p1La p1Ch p2C e p2E p2D p2Ca p2La p2Ch p3C p3B p3E p3D p3Ca p3La p3Ch p4C p4B p4E p4D p4Ca p4La p4Ch pause
           10 -> Keys p1C p1B p1E p1D p1Ca p1La p1Ch p2C p2B e p2D p2Ca p2La p2Ch p3C p3B p3E p3D p3Ca p3La p3Ch p4C p4B p4E p4D p4Ca p4La p4Ch pause
           11 -> Keys p1C p1B p1E p1D p1Ca p1La p1Ch p2C p2B p2E e p2Ca p2La p2Ch p3C p3B p3E p3D p3Ca p3La p3Ch p4C p4B p4E p4D p4Ca p4La p4Ch pause
           12 -> Keys p1C p1B p1E p1D p1Ca p1La p1Ch p2C p2B p2E p2D e p2La p2Ch p3C p3B p3E p3D p3Ca p3La p3Ch p4C p4B p4E p4D p4Ca p4La p4Ch pause
           13 -> Keys p1C p1B p1E p1D p1Ca p1La p1Ch p2C p2B p2E p2D p2Ca e p2Ch p3C p3B p3E p3D p3Ca p3La p3Ch p4C p4B p4E p4D p4Ca p4La p4Ch pause
           14 -> Keys p1C p1B p1E p1D p1Ca p1La p1Ch p2C p2B p2E p2D p2Ca p2La e p3C p3B p3E p3D p3Ca p3La p3Ch p4C p4B p4E p4D p4Ca p4La p4Ch pause
           15 -> Keys p1C p1B p1E p1D p1Ca p1La p1Ch p2C p2B p2E p2D p2Ca p2La p2Ch e p3B p3E p3D p3Ca p3La p3Ch p4C p4B p4E p4D p4Ca p4La p4Ch pause
           16 -> Keys p1C p1B p1E p1D p1Ca p1La p1Ch p2C p2B p2E p2D p2Ca p2La p2Ch p3C e p3E p3D p3Ca p3La p3Ch p4C p4B p4E p4D p4Ca p4La p4Ch pause
           17 -> Keys p1C p1B p1E p1D p1Ca p1La p1Ch p2C p2B p2E p2D p2Ca p2La p2Ch p3C p3B e p3D p3Ca p3La p3Ch p4C p4B p4E p4D p4Ca p4La p4Ch pause
           18 -> Keys p1C p1B p1E p1D p1Ca p1La p1Ch p2C p2B p2E p2D p2Ca p2La p2Ch p3C p3B p3E e p3Ca p3La p3Ch p4C p4B p4E p4D p4Ca p4La p4Ch pause
           19 -> Keys p1C p1B p1E p1D p1Ca p1La p1Ch p2C p2B p2E p2D p2Ca p2La p2Ch p3C p3B p3E p3D e p3La p3Ch p4C p4B p4E p4D p4Ca p4La p4Ch pause
           20 -> Keys p1C p1B p1E p1D p1Ca p1La p1Ch p2C p2B p2E p2D p2Ca p2La p2Ch p3C p3B p3E p3D p3Ca e p3Ch p4C p4B p4E p4D p4Ca p4La p4Ch pause
           21 -> Keys p1C p1B p1E p1D p1Ca p1La p1Ch p2C p2B p2E p2D p2Ca p2La p2Ch p3C p3B p3E p3D p3Ca p3La e p4C p4B p4E p4D p4Ca p4La p4Ch pause
           22 -> Keys p1C p1B p1E p1D p1Ca p1La p1Ch p2C p2B p2E p2D p2Ca p2La p2Ch p3C p3B p3E p3D p3Ca p3La p3Ch e p4B p4E p4D p4Ca p4La p4Ch pause
           23 -> Keys p1C p1B p1E p1D p1Ca p1La p1Ch p2C p2B p2E p2D p2Ca p2La p2Ch p3C p3B p3E p3D p3Ca p3La p3Ch p4C e p4E p4D p4Ca p4La p4Ch pause
           24 -> Keys p1C p1B p1E p1D p1Ca p1La p1Ch p2C p2B p2E p2D p2Ca p2La p2Ch p3C p3B p3E p3D p3Ca p3La p3Ch p4C p4B e p4D p4Ca p4La p4Ch pause
           25 -> Keys p1C p1B p1E p1D p1Ca p1La p1Ch p2C p2B p2E p2D p2Ca p2La p2Ch p3C p3B p3E p3D p3Ca p3La p3Ch p4C p4B p4E e p4Ca p4La p4Ch pause
           26 -> Keys p1C p1B p1E p1D p1Ca p1La p1Ch p2C p2B p2E p2D p2Ca p2La p2Ch p3C p3B p3E p3D p3Ca p3La p3Ch p4C p4B p4E p4D e p4La p4Ch pause
           27 -> Keys p1C p1B p1E p1D p1Ca p1La p1Ch p2C p2B p2E p2D p2Ca p2La p2Ch p3C p3B p3E p3D p3Ca p3La p3Ch p4C p4B p4E p4D p4Ca e p4Ch pause
           28 -> Keys p1C p1B p1E p1D p1Ca p1La p1Ch p2C p2B p2E p2D p2Ca p2La p2Ch p3C p3B p3E p3D p3Ca p3La p3Ch p4C p4B p4E p4D p4Ca p4La e pause

-- | Função utilizada para reagir aos commandos
reageEvento :: Event -> World -> World 
reageEvento e w
    = case i of 
        0 -> reageMenu0   e w
        1 -> reageMenu1   e w
        2 -> reageMenu2   e w
        3 -> reageMenu3   e w
        4 -> reageJogo    e w
        5 -> reagePausa   e w
        6 -> reageEditor  e w
        7 -> reageEditarC e w
        8 -> reageTrocaC  e w
        9 -> reageTrocaC  e w
        otherwise -> w
    where i = sitioToInt $ sitio w


-- | Função que recebe um Sitio e retorna um int para escerever um menu ou reagir
-- | 0 -> m0
-- | 1 -> m1
-- | 2 -> m2
-- | 3 -> m3 
-- | 4 -> Jogar
-- | 5 -> Pausa
-- | 6 -> Editor
-- | 7 -> Editar Controlos
-- | 8 -> Espera Pelo proximo evento para substituir os controlos
-- | 9 -> Controlo cedido já esta em uso1
sitioToInt :: Sitio -> Int
sitioToInt (Sitio pause game players mapDefined escolherMap editaMap editaComands escolhido _ proibido) 
    = if(not editaComands) 
        then if(players)
            then if(mapDefined)
                then if(game)
                    then if(pause)
                        then 5
                        else 4
                    else 2
                else if(escolherMap) 
                    then if(editaMap)
                        then 6
                        else 3
                    else 1
            else 0
        else if(escolhido)
            then if(proibido) 
                then 9
                else 8
            else 7

-- | Função utilizada para Desenhar o mapa
desenhaEstado :: World -> Picture
desenhaEstado w
    = case i of 
        0 -> desenhaMenu0         w
        1 -> desenhaMenu1         w
        2 -> desenhaMenu2         w
        3 -> desenhaMenu3         w
        4 -> desenhaJogo          w
        5 -> desenhaPausa         w
        6 -> desenhaEditor        w
        7 -> desenhaEditarC       w
        8 -> desenhaEditarCEspera w
        9 -> desenhaEditarCErrado w
    where i = sitioToInt $ sitio  w

desenhaMenu0 :: World -> Picture
desenhaMenu0 = menu0 . pics

desenhaMenu1 :: World -> Picture
desenhaMenu1 = menu1 . pics

desenhaMenu2 :: World -> Picture
desenhaMenu2 = menu2 . pics

desenhaMenu3 :: World -> Picture
desenhaMenu3 = menu3 . pics
-- TODO
desenhaJogo :: World -> Picture
desenhaJogo w = desenhaMapa w

desenhaPausa :: World -> Picture
desenhaPausa = menuP . pics
-- TODO
desenhaEditor :: World -> Picture
desenhaEditor = desenhaMapa
-- TODO
desenhaEditarC :: World -> Picture
desenhaEditarC = vaz . pics
-- TODO
desenhaEditarCEspera :: World -> Picture
desenhaEditarCEspera = des . pics
-- TODO
desenhaEditarCErrado :: World -> Picture
desenhaEditarCErrado = ind . pics

estadoInicial :: Pics -> World
estadoInicial pics = World e keysDefault pics (editorInicial []) sitioDefault
              where e = Estado [[]] [] []
                    keysDefault = KeyBank keysDefaultJogo keysDefaultEd keysDefaultMenu 
                    
-- | Função que reage ao tempo
reageTempo :: Float -> World -> World
reageTempo x w@(World es ks p ed s@(Sitio pa jg js md esm em ec edc ev po)) 
    = if ((not pa) && jg) then World (tick es) ks p ed s
                          else w

-- | Frame rate
fr :: Int
fr = 8

-- | dm indica todas as informações sobre a janela do jogo.
dm :: Display
dm = InWindow "Tanks Grupo 142" (800, 600) (0, 0)

-- | Função principal da Tarefa 5.
--
-- __NB:__ Esta Tarefa é completamente livre. Deve utilizar a biblioteca <http://hackage.haskell.org/package/gloss gloss> para animar o jogo, e reutilizar __de forma completa__ as funções das tarefas anteriores.
main :: IO ()
main = do
       p1  <- (loadBMP "../img/tank1.bmp")
       p2  <- (loadBMP "../img/tank2.bmp")
       p3  <- (loadBMP "../img/tank3.bmp")
       p4  <- (loadBMP "../img/tank4.bmp")
       vaz <- (loadBMP "../img/grass.bmp")
       des <- (loadBMP "../img/Destrutivel.bmp")
       ind <- (loadBMP "../img/Indestrutivel.bmp")
       m0  <- (loadBMP "../img/menu0.bmp")
       m1  <- (loadBMP "../img/menu1.bmp")
       m2  <- (loadBMP "../img/menu2.bmp")
       m3  <- (loadBMP "../img/menu3.bmp")
       mP  <- (loadBMP "../img/menuP.bmp")
       play dm              -- janela onde irá correr o jogo
            (greyN 0.5)     -- côr do fundo da janela
            fr              -- frame rate
            (estadoInicial (escalar $ Pics p1 p2 p3 p4 vaz des ind p1 p1 p1 m0 m1 m2 m3 mP 100.0))
            desenhaEstado   -- desenha o estado do jogo
            reageEvento     -- reage a um evento
            reageTempo      -- reage ao passar do tempo

-- | Redimensiona uma imagem
escalar :: Pics -> Pics
escalar p = Pics p1S p2S p3S p4S vazS desS indS caS laS chS m0S m1S m2S m3S mPS sca
        where p1S = scale 0.025 0.05 (p1 p)
              p2S = scale 0.025 0.05 (p2 p)
              p3S = scale 0.025 0.05 (p3 p)
              p4S = scale 0.025 0.05 (p4 p)
              blo = 4/9 
              vazS = scale blo blo $ vaz p
              desS = scale blo blo $ des p
              indS = scale blo blo $ ind p
              caS = scale 0.007 0.007 (laserP p)
              laS = scale 0.007 0.007 (canhaoP p)
              chS = scale 0.007 0.007 (choqueP p)
              m0S = menu0 p
              m1S = menu1 p
              m2S = menu2 p
              m3S = menu3 p
              mPS = menuP p 
              sca = escala p             

meteJogadores :: Editor -> Estado -> Estado
meteJogadores ed es = case i of
                           0 -> jogador0 ed es 
                           1 -> jogador1 ed es
                           2 -> jogador2 ed es
                           3 -> jogador3 ed es
                           4 -> jogador4 ed es
                    where i = length (jogadoresEstado es) 

-- | Sem jogadores
jogador0 :: Editor -> Estado -> Estado
jogador0 ed@(Editor _ _ _ _ map) (Estado _ [] ds) = Estado map [] ds

-- | Sabe que recebe um jogador
jogador1 :: Editor -> Estado -> Estado
jogador1 ed@(Editor _ _ _ _ map) (Estado _ [Jogador _ d1 v1 l1 c1] ds) = Estado map [Jogador (1,1) d1 v1 l1 c1] ds

-- | Sabe que recebe 2 jogadores
jogador2 :: Editor -> Estado -> Estado
jogador2 ed@(Editor _ _ _ _ map) (Estado _ [Jogador _ d1 v1 l1 c1, Jogador _ d2 v2 l2 c2] ds) = Estado map [Jogador (1,1) d1 v1 l1 c1,Jogador (1, length (head map)-3) d2 v2 l2 c2] ds

-- | Sabe que recebe 3 jogadores
jogador3 :: Editor -> Estado -> Estado
jogador3 ed@(Editor _ _ _ _ map) (Estado _ [Jogador _ d1 v1 l1 c1, Jogador _ d2 v2 l2 c2, Jogador _ d3 v3 l3 c3] ds) = Estado map [Jogador (1,1) d1 v1 l1 c1,Jogador (1, length (head map)-3) d2 v2 l2 c2,Jogador (length map -3,length (head map)-3) d3 v3 l3 c3] ds

-- | Sabe que recebe 4 jogadores
jogador4 :: Editor -> Estado -> Estado
jogador4 ed@(Editor _ _ _ _ map) (Estado _ [Jogador _ d1 v1 l1 c1, Jogador _ d2 v2 l2 c2, Jogador _ d3 v3 l3 c3, Jogador _ d4 v4 l4 c4] ds) = Estado map [Jogador (1,1) d1 v1 l1 c1,Jogador (1, length (head map)-3) d2 v2 l2 c2,Jogador (length map -3,length (head map)-3) d3 v3 l3 c3,Jogador (length map -3,1) d4 v4 l4 c4] ds

tamanho :: Int -> Float
tamanho x = fromIntegral(div 600 x)

desenhaMapa :: World -> Picture
desenhaMapa w = Translate (-400.0) (-300.0) (scale 0.5 0.5 (pictures d))
              where m = criaMatrizPic w
                    d = desenhaMatriz w (0,0) m

{- | Imagens do Jogo
data Pics = Pics
    { p1      :: Picture  -- ^ Imagems do jogador 1
    , p2      :: Picture  -- ^ Imagems do jogador 1
    , p3      :: Picture  -- ^ Imagems do jogador 1
    , p4      :: Picture  -- ^ Imagems do jogador 1
    , vaz     :: Picture  -- ^ Bloco Vazio
    , des     :: Picture  -- ^ Bloco Destrutivel
    , ind     :: Picture  -- ^ Bloco Indestrutivel
    , laserP  :: Picture  -- ^ Imagem do Laser
    , canhaoP :: Picture  -- ^ Imagem do canhao
    , choqueP :: Picture  -- ^ imagem do choque
    , menu0   :: Picture  -- ^ Escolher o numero de Jogadores
    , menu1   :: Picture  -- ^ 1º Menu 
    , menu2   :: Picture  -- ^ 2º Menu
    , menu3   :: Picture  -- ^ Menu Escolher Mapa
    , menuP   :: Picture  -- ^ Menu Pausa
    , escala  :: Float    -- ^ Escala das imgens
    }
    deriving (Show,Eq)-}
desenhaMatriz :: World -> Posicao -> Matriz Picture -> [Picture]
desenhaMatriz _ _ [] = []
desenhaMatriz _ _ [[]] = []
desenhaMatriz w (x,y) (h:t) = desenhaMatrizLinha w (x,y) h ++ desenhaMatriz w (x,y+1) t

desenhaMatrizLinha :: World -> Posicao -> [Picture] -> [Picture]
desenhaMatrizLinha _ _ [] = []
desenhaMatrizLinha w (x,y) (h:t) = Translate ((fromIntegral x)*siz) ((fromIntegral y)*siz) h : desenhaMatrizLinha w (x+1,y) t
                                 where siz = escala $ pics w

-- | Cria uma Matriz de pictures
-- | Atualiza a Matriz com paredes Destrutiveis e Indestrutiveis
criaMatrizPic :: World -> Matriz Picture
criaMatrizPic w = trocaLista pInd matriz lpInd
                   where pInd = ind $ pics w                    -- ^ picture Indestrutivel
                         pDes = des $ pics w                    -- ^ picture Destutivel
                         p = vaz $ pics w                       -- ^ picture Vazio
                         m = mapaEstado $ estado w              -- ^ Mapa do estado
                         mapa = criaMatriz (dimensaoMatriz m) p -- ^ Mapa de picture Vazio
                         lpInd = listaDeParedesIn (0,0) m []    -- ^ Lista de Paredes Indestrutiveis
                         lpDes = listaDeParedesDes (0,0) m []   -- ^ Lista de Paredes Destrutiveis
                         matriz = trocaLista pDes mapa lpDes    

-- | Troca numa matriz uma lista de posicoes para um elemnto dado
trocaLista :: a -> Matriz a -> [Posicao] -> Matriz a
trocaLista _ m [] = m
trocaLista p m (h:t) = trocaLista p (atualizaPosicaoMatriz (swap h) p m) t

-- | Devolve a lista das posicoes que podem ser necessarias a trocar no mapa.
listaDeParedesIn :: Posicao -> Mapa -> [Posicao] -> [Posicao]
listaDeParedesIn _ [] l = l
listaDeParedesIn _ [[]] l = l
listaDeParedesIn (x,y) (h:t) l = listaDeParedesIn (x,y+1) t (listaDeParedesInLinha (x,y) h l)

-- | Auxiliar da posBlocas que tratas a linhas da  matriz.
listaDeParedesInLinha :: Posicao -> [Peca] -> [Posicao] -> [Posicao]
listaDeParedesInLinha _ [] l = l
listaDeParedesInLinha (x,y) (h:t) l = if (h == Bloco Indestrutivel)
                            then listaDeParedesInLinha (x+1,y) t ((x,y):l)
                            else listaDeParedesInLinha (x+1,y) t l

-- | Devolve a lista das posicoes que podem ser necessarias a trocar no mapa.
listaDeParedesDes :: Posicao -> Matriz Peca -> [Posicao] -> [Posicao]
listaDeParedesDes _ [] l = l
listaDeParedesDes _ [[]] l = l
listaDeParedesDes (x,y) (h:t) l = listaDeParedesDes (x,y+1) t (listaDeParedesDesLinha (x,y) h l)

-- | Auxiliar da posBlocas que tratas a linhas da  matriz.
listaDeParedesDesLinha :: Posicao -> [Peca] -> [Posicao] -> [Posicao]
listaDeParedesDesLinha _ [] l = l
listaDeParedesDesLinha (x,y) (h:t) l = if (h == Bloco Destrutivel)
                            then listaDeParedesDesLinha (x+1,y) t ((x,y):l)
                            else listaDeParedesDesLinha (x+1,y) t l