module Snake where
import Data.Map as Map
import System.Random

data CurDir = UP | DOWN | LEFT | RIGHT deriving (Show, Eq)

gameBorderHeight = 24
gameBorderWidth = 32

blockSize :: Float
blockSize = 20

width, height :: Int
width = gameBorderWidth * 20 + (round blockSize)
height = gameBorderHeight * 20 + (round blockSize)

type Food = (Int, Int)
type Snake = [Food]

data SnakeGame = Game {
    getSnake :: Snake,
    getFood :: Food,
    gameOver :: Bool,
    direction :: CurDir,
    randSeed :: StdGen,
    score :: Int
} deriving Show

initialState :: Bool -> SnakeGame
initialState gameOverState = Game {
    getSnake = [(startX, startY), (startX, startY-1), (startX, startY-2)],
    getFood = (10,3),
    gameOver = gameOverState,
    direction = DOWN,
    randSeed = mkStdGen 100,
    score = 0
} where
    startX = 15
    startY = 15

move :: SnakeGame -> Snake
move snakeGame = 
    if isFoodEaten then newHead : snake else newHead : init snake
    where
    (isFoodEaten, _) = (foodIsEaten snakeGame)
    curDir = direction snakeGame
    snake = (getSnake snakeGame)
    newHead = 
        -- (0, -1) is up
        if curDir == UP then (xHead, yHead - 1)
        -- (0, 1) is down
        else if curDir == DOWN then (xHead, yHead + 1)
        -- (-1, 0) is left
        else if curDir == LEFT then (xHead - 1, yHead)
        -- (0, 1) is right
        else if curDir == RIGHT then (xHead + 1, yHead)
        -- this last case shouldn't happen
        else head snake

    (xHead, yHead) = head snake

changeDirection :: SnakeGame -> CurDir -> SnakeGame
changeDirection (Game s f g direction r sc) newDir = (Game s f g newDir r sc)

foodIsEaten :: SnakeGame -> (Bool, Int)
foodIsEaten snakeGame = if (head (getSnake snakeGame)) == (getFood snakeGame)
    then (True, score snakeGame + 1)
    else (False, score snakeGame)

checkGameOver :: SnakeGame -> Bool
checkGameOver snakeGame = ((head $ getSnake snakeGame) `elem` (tail $ getSnake snakeGame)) ||
    (fst (head $ getSnake snakeGame) <= 0|| fst (head $ getSnake snakeGame) >= gameBorderWidth) ||
    (snd (head $ getSnake snakeGame) <= 0 || snd (head $ getSnake snakeGame) >= gameBorderHeight)

generateFood :: SnakeGame -> (Food, StdGen)
generateFood snakeGame = ((foodX, foodY), stdGen3)
    where
        (foodX, stdGen2) = ((randomR (1, gameBorderWidth-1) (randSeed snakeGame)))
        (foodY, stdGen3) = ((randomR (1, gameBorderHeight-1) stdGen2))
