import System.IO
import Data.List
import Control.Monad

data Cell = Alive | Dead
            deriving(Ord,Eq)

instance Show Cell where
    show Alive = "A"
    show Dead  = " "

data Coordinate = Coordinate {x :: Int, y :: Int} 
                    deriving(Eq,Ord, Show)

type Board = [[Cell]]


data GameState = GameState {
    aliveCells :: [Coordinate],
    rate       :: Int
}

        
run :: GameState -> IO ()
run game = do
            showBoard game
            putStrLn "Next Generation?"
            getChar
            let GameState newAlives rate = crank game
            run (GameState newAlives rate)
            return ()

         
        

initialGameState :: GameState
initialGameState = GameState {
                        aliveCells = [],
                        rate       = 1
                    }

boardWidth :: Int
boardWidth = 10

boardHeight :: Int
boardHeight = 10

showBoard :: GameState -> IO ()
showBoard (GameState alives _ ) = display
        [[alive x y | x <- [0..boardWidth] ] | y <- [0..boardHeight]]
        where
            alive x y 
                | Coordinate x y `elem` alives = Alive
                | otherwise                    = Dead

crank :: GameState -> GameState
crank state@(GameState alives rate) = (GameState newAlives rate)
    where 
        newAlives = 
            [Coordinate x y | x <- [0..boardWidth],y <-[0..boardHeight],
                                    liveOn (Coordinate x y) state]
toString :: Board -> [String]
toString board = [show row | row <- board]

display :: Board -> IO ()
display = (mapM_ putStrLn).toString

liveOn :: Coordinate -> GameState -> Bool
liveOn coord (GameState alives _) = 
    (coord `elem` alives && aliveNeighbours coord == 2 )
    || aliveNeighbours coord == 3
    where
    aliveNeighbours = length.(filter (`elem` alives)).neighbours 

neighbours :: Coordinate -> [Coordinate]
neighbours (Coordinate x y) = 
                [Coordinate i j| i <- (pm x), j <- (pm y), (x /=i || y /=j),
                                onBoard i, onBoard j]
            where 
                pm z = map (+z) [-1,0,1]
                onBoard m = 0 <= m && m <= boardWidth

resurrect :: Coordinate -> GameState -> GameState
resurrect (cell) (GameState alives rate) = GameState (cell : alives) rate 
