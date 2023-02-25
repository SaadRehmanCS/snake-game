module Snake where
import Data.Map as Map
import System.Random

-- Inititalize global variables
data CurDir = UP | DOWN | LEFT | RIGHT deriving (Show, Eq)

gameBorderHeight = 24
gameBorderWidth = 32

blockSize :: Float
blockSize = 20

width, height :: Int
width = gameBorderWidth * 20 + (round blockSize)
height = gameBorderHeight * 20 + (round blockSize)

-- Initialize data types/structures
type Food = (Int, Int)
type Snake = [Food]

data SnakeGame = Game {
    getSnake :: Snake,
    getFood :: Food,
    gameOver :: Bool,
    direction :: CurDir,
    randSeed :: StdGen,
    score :: Int,
    highScore :: Int
} deriving Show

-- Initial state of the program upon starting
initialState :: Bool -> Int -> SnakeGame
initialState gameOverState newHighScore = Game {
    getSnake = [(startX, startY), (startX, startY-1), (startX, startY-2)],
    getFood = (10,3),
    gameOver = gameOverState,
    direction = DOWN,
    randSeed = mkStdGen 100,
    score = 0,
    highScore = newHighScore
} where
    startX = 15
    startY = 15

-- Update the state of the snake
-- Increment the head and delete the tail depending on if it eats food
move :: SnakeGame -> Snake
move (Game snake food gameOver curDir randSeed score highScore) = 
    if isFoodEaten then newHead : snake else newHead : init snake
    where
    (isFoodEaten, _) = (foodIsEaten (Game snake food gameOver curDir randSeed score highScore))
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

-- Change the state of direction using the provided new direction 
changeDirection :: SnakeGame -> CurDir -> SnakeGame
changeDirection (Game s f g direction r sc highScore) newDir = (Game s f g newDir r sc highScore)

-- Checks if snake eats food based on coordinates of the head and food
-- Returns the Bool and score
foodIsEaten :: SnakeGame -> (Bool, Int)
foodIsEaten (Game snake food gameOver curDir randSeed score highScore) = if (head snake) == food
    then (True, score + 1)
    else (False, score)

-- Checks if game is over based on if the head crosses the boundaries or intersects with body
checkGameOver :: SnakeGame -> Bool
checkGameOver (Game snake food gameOver curDir randSeed score highScore) =
    ((head snake) `elem` (tail snake)) ||
    (headX <= 0|| headX >= gameBorderWidth) ||
    (headY <= 0 || headY >= gameBorderHeight)
    where
        (headX, headY) = head snake

-- Generates new food and checks that food spawns in valid position
-- Returns the new food and a new random seed
generateFood :: SnakeGame -> (Food, StdGen)
generateFood (Game snake food gameOver direction randSeed score highScore) = if (foodX, foodY) `elem` snake
    then generateFood (Game snake food gameOver direction stdGen3 score highScore)
    else ((foodX, foodY), stdGen3)
    where
        (foodX, stdGen2) = ((randomR (1, gameBorderWidth-1) randSeed))
        (foodY, stdGen3) = ((randomR (1, gameBorderHeight-1) stdGen2))
