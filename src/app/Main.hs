module Main(main) where
import Graphics.Gloss
import Graphics.Gloss.Interface.Pure.Game
import Snake

window :: Display
window = InWindow "Snake Game" (width, height) (offset, offset)

background :: Color
background = black

render :: SnakeGame -> Picture
render game = pictures $ [ fillRectangle black (16, 0) (640, 20)
                                , fillRectangle black (16, 24) (640, 20)
                                , fillRectangle black (0, 12) (20, 480)
                                , fillRectangle black (32, 12) (20, 480) ] ++
                                  fmap (convertToPicture black) snake ++ 
                                  fmap (convertToPicture blue) [food] ++
                                  gameOverPicture
                where
                    snake = getSnake game 
                    food = getFood game
                    convertToPicture :: Color -> (Int, Int) -> Picture
                    convertToPicture color' (x, y) = fillRectangle color' (toFloat (x, y)) (20, 20)
                    fillRectangle color' (tx, ty) (w, h) =  color color' $ 
                                                    scale 1 (-1) $ 
                                                    translate (tx * 20 - 320) (ty * 20 - 240) $ 
                                                    rectangleSolid w h
                    toFloat (x, y) = (fromIntegral x, fromIntegral y)
                    gameOverPicture =   if (gameOver game) 
                                then [  color blue $ 
                                        translate (-200) (0) $ 
                                        scale 0.5 0.5 $ 
                                        text "GAME OVER"
                                     ,  color blue $ 
                                        translate (-175) (-50) $ 
                                        scale 0.2 0.2 $ 
                                        text "Press SPACE to try again." ] 
                                else []

handleKeys :: Event -> SnakeGame -> SnakeGame
handleKeys (EventKey (SpecialKey KeyLeft) Down _ _) snakeGame = (changeDirection snakeGame LEFT)
handleKeys (EventKey (SpecialKey KeyRight) Down _ _) snakeGame = (changeDirection snakeGame RIGHT)
handleKeys (EventKey (SpecialKey KeyUp) Down _ _) snakeGame = (changeDirection snakeGame UP)
handleKeys (EventKey (SpecialKey KeyDown) Down _ _) snakeGame = (changeDirection snakeGame DOWN)
handleKeys (EventKey (SpecialKey KeySpace) Down _ _) snakeGame = if (gameOver snakeGame)
        then initialState False
        else snakeGame

handleKeys _ snakeGame = snakeGame

update :: Float -> SnakeGame -> SnakeGame
update time snakeGame = snakeGame

main :: IO ()
main = play window background 10 (initialState True) render handleKeys update