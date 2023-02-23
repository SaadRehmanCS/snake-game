module Main(main) where
import Graphics.Gloss
import Graphics.Gloss.Interface.Pure.Game
import Snake

offset :: Int
offset = 100

window :: Display
window = InWindow "Snake Game" (width, height) (offset, offset)

background :: Color
background = black

render :: SnakeGame -> Picture
render game = pictures $ [ fillRectangle blue (gameBorderWidthHalf, 0) (borderWidth, blockSize)
                                , fillRectangle blue (gameBorderWidthHalf, fromIntegral gameBorderHeight) (borderWidth, blockSize)
                                , fillRectangle blue (0, gameBorderHeightHalf) (blockSize, borderHeight)
                                , fillRectangle blue (fromIntegral gameBorderWidth, gameBorderHeightHalf) (blockSize, borderHeight) ] ++
                                  fmap (convertToPicture green) snake ++ 
                                  fmap (convertToPicture blue) [food] ++
                                  gameOverPicture
                where
                        gameBorderHeightHalf = fromIntegral (gameBorderHeight `div` 2)
                        gameBorderWidthHalf = fromIntegral (gameBorderWidth `div` 2)
                        borderWidth = fromIntegral width-blockSize
                        borderHeight = fromIntegral height-blockSize
                        snake = getSnake game 
                        food = getFood game
                        convertToPicture :: Color -> (Int, Int) -> Picture
                        convertToPicture color' (x, y) = fillRectangle color' (toFloat (x, y)) (20, 20)
                        fillRectangle color' (tx, ty) (w, h) =  color color' $ 
                                                        scale 1 (-1) $ 
                                                        translate (tx * blockSize - (borderWidth / 2)) (ty * blockSize - (borderHeight / 2)) $ 
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
                                                text "Press SPACE to play again."
                                        , color red $ 
                                                translate (-90) (-100) $ 
                                                scale 0.3 0.3 $ 
                                                text ("Score: "  ++ show (score game))]
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
update time snakeGame = if (gameOver snakeGame)
        then (snakeGame)
        else (Game newSnake newFood newGameOver curDirection newSeed newScore)
        where
                newGameOver = checkGameOver snakeGame
                curSnake = getSnake snakeGame
                curFood = getFood snakeGame
                newSnake = move snakeGame
                seed = randSeed snakeGame
                (isFoodEaten, newScore) = foodIsEaten snakeGame
                (genFood, newSeed) = generateFood snakeGame
                newFood = if isFoodEaten
                        then genFood
                        else curFood
                curDirection = direction snakeGame

main :: IO ()
main = play window background 10 (initialState True) render handleKeys update
