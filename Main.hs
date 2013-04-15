import System.IO
import Data.List
import Control.Monad
import Coordinate

data Cell = Alive | Dead
            deriving(Ord,Eq)

instance Show Cell where
    show Alive = "A"
    show Dead  = " "


type Board = [[Cell]]


data GameState = GameState {
    aliveCells :: [Coord],
    rate       :: Int
}

        
test :: GameState -> IO ()
test game = do
            showBoard game
            putStrLn "Next Generation?"
            getChar
            let next = crank game
            test next
        

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
                | Coord x y `elem` alives = Alive
                | otherwise                    = Dead

crank :: GameState -> GameState
crank state@(GameState alives rate) = (GameState newAlives rate)
    where 
        newAlives = 
            [Coord x y | x <- [0..boardWidth],y <-[0..boardHeight],
                                    liveOn (Coord x y) state]
toString :: Board -> [String]
toString board = [show row | row <- board]

display :: Board -> IO ()
display = (mapM_ putStrLn).toString

liveOn :: Coord -> GameState -> Bool
liveOn coord (GameState alives _) = 
    (coord `elem` alives && aliveNeighbours coord == 2 )
    || aliveNeighbours coord == 3
    where
    aliveNeighbours = length.(filter (`elem` alives)).neighbours 

neighbours :: Coord -> [Coord]
neighbours (Coord x y) = 
                [Coord i j| i <- (pm x), j <- (pm y), (x /=i || y /=j),
                                onBoard i, onBoard j]
            where 
                pm z = map (+z) [-1,0,1]
                onBoard m = 0 <= m && m <= boardWidth

resurrect :: Coord -> GameState -> GameState
resurrect (cell) (GameState alives rate) = GameState (cell : alives) rate 

kill :: Coord -> GameState -> GameState
kill cell (GameState alives rate) = GameState (delete cell alives) rate


b1 :: GameState
b1 = GameState cross 1
    where 
        cross = [Coord x y | x <- [0..boardWidth], y <- [0..boardWidth], 
                    x == y || x == boardWidth-y]
