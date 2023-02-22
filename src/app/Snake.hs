module Snake where
import Data.Map as Map

data CurDir = UP | DOWN | LEFT | RIGHT deriving Show

width, height, offset :: Int
width = 600
height = 600
offset = 100
snakeLength = 20

type Food = (Int, Int)
type Snake = [Food]

data SnakeGame = Game {
    getSnake :: Snake,
    getFood :: Food,
    gameOver :: Bool,
    direction :: CurDir
} deriving Show

initialState :: Bool -> SnakeGame
initialState gameOverState = Game {
    getSnake = [(startX, startY), (startX, startY-snakeLength)],
    getFood = (3,3),
    gameOver = gameOverState,
    direction = DOWN
} where
    startX = fromIntegral (width `div` 2)
    startY = fromIntegral (height `div` 2)


changeDirection :: SnakeGame -> CurDir -> SnakeGame
changeDirection (Game s f g direction) newDir = (Game s f g newDir)
