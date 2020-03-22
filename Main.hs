module Main where

import Graphics.Gloss
import Graphics.Gloss.Data.ViewPort
import Graphics.Gloss.Interface.Pure.Game 
width, height, offset :: Int
width = 800
height = 600
offset = 100

background :: Color
background = white

halfWidth :: Float
halfWidth = fromIntegral width / 2

halfHeight :: Float
halfHeight = fromIntegral height / 2

window :: Display
window = InWindow "Canhao" (width, height) (offset, offset)

-- | Cor da borda.
wallColor :: Color
wallColor = blue

-- | Borda de cima.
{-
topWall :: Picture
topWall = translate 0 halfHeight
        $ color wallColor 
        $ rectangleSolid (fromIntegral width) 10

rightWall :: Picture
rightWall = translate (fromIntegral width / 2) 0
          $ color wallColor 
          $ rectangleSolid 10 $ fromIntegral height
leftWall :: Picture
leftWall = translate (-fromIntegral width / 2) 0
          $ color wallColor 
          $ rectangleSolid 10 $ fromIntegral height

walls :: Picture
walls = pictures [leftWall, rightWall, topWall]  
-}

renderTxt :: Color -> String -> Picture
renderTxt cor texto = translate (-150) 150 (scale 0.30 3 (color cor (text texto) ))


-- | Mensagem atual a ser mostrada.
curMsg :: Int -> Bool -> Picture
curMsg  0   paused = pauseMsg paused
curMsg (-1) paused = lostMsg
curMsg  _   paused = winMsg

winMsg    = renderTxt green "You won! (r = new game)"
lostMsg   = renderTxt red   "Git gud! (r = new game)"
 
pauseMsg True  = renderTxt blue "Press p to play!"
pauseMsg False = renderTxt blue ""

-- | Coordenada do centro da bola.
type Position = (Float, Float)

-- Propriedades da bola

-- | Raio da bola.
ballSize :: Float
ballSize = 10

-- | Cor da bola.
ballColor :: Color
ballColor = red

ball :: Position -> Picture
ball (x,y) = color ballColor $ circleSolid ballSize

moveBall :: Float -> Position -> Position -> Position
moveBall segundos (x, y) (vx, vy) = (x', y')
    where
        x' = x * vx * segundos
        y' = y * vy * segundos

-- Propriedades dos aviões

avioesEmFila :: Int
avioesEmFila = 5

-- | Tamanho dos avioes
tamanhoAviao :: (Float, Float)
tamanhoAviao = (20, 10)

aHalfWidth :: Float
aHalfWidth  = (1 + fst tamanhoAviao) / 2

aHalfHeight :: Float
aHalfHeight = (1 + snd tamanhoAviao) / 2

-- | Informação dos aviões.
data AviaoInfo = Aviao
  { aviaoPos :: Position -- ^ (x, y) coordenada do avião.
  , aviaoCol :: Color    -- ^ cor do avião.
  }

-- | Lista dos blocos atuais.
type Avioes = [AviaoInfo]

temAviao :: Avioes -> Bool
temAviao [] = False
temAviao aviao = True

-- | Desenha os avioes.
{-drawBlocks :: Avioes -> Picture
drawBlocks avioes = pictures $ translate (drawBlock)
  where
    drawBlock (Aviao (x, y) col) = undefined
    block                        = {aviaoPos = (x,y) , aviaoCol = col}
    (w, h)                       = tamanhoAviao
-}
genAviao :: Int -> AviaoInfo
genAviao n = Aviao { aviaoPos = pos, aviaoCol = orange }
  where pos  = (fromIntegral bx, fromIntegral by)
      
        bx   = (-250) + (y,x) * 35 
        by   = 100 - (y,x) * 40 
        (y, x) = divMod n avioesEmFila 

-- JOGADOR
-- Informações do jogador

-- | Cor do jogador.
playerColor :: Color
playerColor = green

-- | Tamanho do jogador.
playerWidth :: Float
playerWidth = 50

halfPlayerWidth :: Float
halfPlayerWidth = playerWidth / 2

playerHeight :: Float
playerHeight = 10

-- | Posição do jogador no eixo y
playerY :: Float
playerY = -250

mkPlayer :: Float -> Picture
mkPlayer x = translate 0 (-250) $ color playerColor $ polygon [(50,50)]


movePlayer :: Float -> Float -> Float -> Float
movePlayer seconds x v  | (rightWallCollision x) &&  (v>0) == True = x - 1 
                        | (leftWallCollision x)  &&  (v<0) == True = x + 1
                        | otherwise = x + v * seconds

-- | Verifica se o jogador atingiu a parede da esquerda.
leftWallCollision :: Float -> Bool
leftWallCollision x | x - halfPlayerWidth <= -halfWidth + 5  = True
                    | otherwise                              = False

-- | Verifica se o jogador atingiu a parede da direita.
rightWallCollision :: Float -> Bool
rightWallCollision x | x + halfPlayerWidth >= halfWidth + 5   = True
                     | otherwise                              = False

-- | Verifica se o jogador atingiu a parede.
paddleWallCollision :: Float -> Bool
paddleWallCollision x | rightWallCollision x == True || leftWallCollision x == True = True
                      | otherwise                                                   = False


-- | Função para if-then-else.
funIf :: Bool -> a -> a -> a
funIf b x y = if b then x else y

-- | Multiplica elementos de duas tuplas.
mulTuple :: Num a => (a, a) -> (a, a) -> (a, a)
mulTuple (x1,x2) (y1,y2) = (x1*y1, x2*y2)

-- | Verifica se tem interseção entre duas faixas de valores.
overlap :: Ord a => (a, a) -> (a, a) -> Bool
overlap (xmin, xmax) (ymin, ymax) = xmin <= ymax && ymin <= xmax

-- | Cria uma faixa de valores centrado em x e com raio r.
range :: Num a => a -> a -> (a, a)
range x r = (x - r, x + r)
{-
blockCollision :: Position -> Position -> Avioes -> Position
blockCollision v (xball, yball) bs = foldl changeVel v bs
  where
    changeVel (vx, vy) (Aviao (xb, yb) c) | hitCornerH xb && overlapY yb = (-vx,  vy)
                                          | hitCornerV yb && overlapX xb = ( vx, -vy)
                                          | otherwise                    = ( vx,  vy)
  
    hitCornerH xb = ( 0.8 * aHalfWidth,  aHalfWidth)
    hitCornerV yb = ( (−aHalfHeight),  (− 0.8) * aHalfHeight)
    overlapY   yb = overlap yballRange $ range yb aHalfHeight
    overlapX   xb = overlap xballRange $ range xb aHalfWidth
    xballRange    = range xball ballSize
    yballRange    = range yball (-ballSize)
-}
-- | Remove blocos atingidos.
removeAvioes :: Avioes -> Position -> Avioes
removeAvioes bs (xball, yball)  = filter hit bs
  where 
    hit (Aviao (xb, yb) c) = overlapBallX (range xb aHalfWidth) 
                             && overlapBallY (range yb aHalfHeight)
    
    xballRange             = range xball ballSize
    yballRange             = range yball ballSize
    overlapBallX           = overlap xballRange
    overlapBallY           = overlap yballRange


-- Informações do jogo

-- | Estado do jogo
data GameStatus = Game
  { ballLoc   :: Position -- ^ (x, y) coordenada da bola.
  , ballVel   :: Position -- ^ (x, y) velocidade da bola.
  , playerLoc :: Float    -- ^ Posição horizontal do jogador.
  , playerVel :: Float    -- ^ Velocidade do jogador.
  , playerAcc :: Float    -- ^ Aceleração do jogador.
  , isPaused  :: Bool     -- ^ Indicador do status de pausa.
  , avioes    :: Avioes   -- ^ Lista de blocos na tela.
  , gameStat  :: Int      -- ^ Status do jogo: 0 - em jogo, 1 - vitória, -1 - derrota.
  }


-- | Estado inicial do jogo.
initialState :: GameStatus
initialState = Game
  { ballLoc   = (0, -100)
  , ballVel   = (25, -150)
  , playerLoc = 0
  , playerVel = 0
  , playerAcc = 150
  , isPaused  = True
  , avioes    = map genAviao [0..59]
  , gameStat = 0
  }

-- | Converte o estado do jogo em uma imagem de tela.
render :: GameStatus -> Picture
render game = pictures [ballPic,playerPic, aviaoPic, msgPic]
  where
    ballPic   = ball ballLoc game
    playerPic = mkPlayer playerLoc
    aviaoPic =  pictures $ foldl (\x y -> x ++
        [uncurry translate (aviaoPos y) (color (aviaoCol y) (uncurry rectangleSolid tamanhoAviao))])
        [] (avioes game)
 
    msgPic    = renderTxt blue "Press p to play"
    
-- | Atualiza o estado da bola.
updateBall :: Float -> GameStatus -> GameStatus
updateBall seconds game = game { ballLoc = moveBall }
  where pos = ballLoc game
        v   = ballVel game

-- | Atualiza o estado do jogador.
updatePlayer :: Float -> GameStatus -> GameStatus
updatePlayer seconds game = game { playerLoc = movePlayer seconds x v }
  where x = playerLoc game
        v = playerVel game

-- | Atualiza posição da bola de acordo com colisões nos blocos e remove blocos.
updateAvioes :: GameStatus -> GameStatus
updateAvioes game = game {  avioes = avioes' }
  where
    -- atualiza a velocidade da bola ao atingir blocos
    --ballVel' = blockCollision
    avioes'  = removeAvioes avioes 

-- | Atualiza o estado do jogo.
update :: Float -> GameStatus -> GameStatus
update seconds game | ((isPaused game) == True) = game
                    | (temAviao avioes)  == False = game {gameStat =1 }
                    | otherwise = updateAvioes game
             

-- | Responde aos e ventos de teclas.
handleKeys :: Event -> GameStatus -> GameStatus
-- Tecla 'r' retorna ao estaod inicial.
handleKeys (EventKey (Char 'r') Down _ _)            game = initialState
-- Tecla 'p' pausa e despausa o jogo.
handleKeys (EventKey (Char 'p') Down _ _)            game = invPause game 
-- Tecla '←' move para esquerda.
handleKeys (EventKey (SpecialKey KeyLeft) Down _ _)  game = decVel game
-- Soltar a tecla '←' para o jogador.
handleKeys (EventKey (SpecialKey KeyLeft) Up _ _)    game = incVel game
-- Tecla '→' move o jogador para a direita.
handleKeys (EventKey (SpecialKey KeyRight) Down _ _) game = incVel game
-- Soltar a tecla '→' para o jogador.
handleKeys (EventKey (SpecialKey KeyRight) Up _ _)   game = decVel game
-- Qualquer outra tecla é ignorada.
handleKeys _ game = game

-- | Incrementa a velocidade do jogador.
incVel :: GameStatus -> GameStatus
incVel game = game { playerVel = playerVel game + playerAcc game}

-- | Decrementa a velocidade do jogador.
decVel :: GameStatus -> GameStatus
decVel game = game { playerVel = playerVel game - playerAcc game}

-- | Inverte o estado de pausa do jogo.
invPause :: GameStatus -> GameStatus
invPause game = game { isPaused = not $ isPaused game}

fps :: Int
fps = 60

main :: IO()
main = play window background fps initialState render handleKeys update
