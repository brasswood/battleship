module Main where

import Text.Read
import System.Random

main :: IO ()
main = return ()

blankBoard :: Board
blankBoard = Board ((replicate 10 . replicate 10) Blank)

data Square = HitSquare | MissSquare | BlankSquare

newtype Board = Board [[Square]]

instance Show Board where
  show (Board (row:rows)) = showRow row ++ "\n" ++ show (Board rows)
    where showRow (HitSquare:squares)   = "X" ++ showRow squares
          showRow (MissSquare:squares)  = "O" ++ showRow squares
          showRow (BlankSquare:squares) = " " ++ showRow squares
          showRow []              = ""
  show (Board [])         = ""


squareAt :: String -> Board -> Maybe Square
squareAt coord (Board arr) = case (getRow coord, getCol coord) of
                         (Just row, Just col) -> Just (arr !! row !! col)
                         _                    -> Nothing

getRow :: String -> Maybe Int
getRow coord
  | length coord /= 2 && length coord /= 3 = Nothing
  | row <= 'J' && row >= 'A' = Just ((fromEnum row) - (fromEnum 'A'))
  | otherwise = Nothing
  where row = head coord

getCol :: String -> Maybe Int
getCol coord
  | length coord /= 2 && length coord /= 3 = Nothing
  | otherwise = case (readMaybe (drop 1 coord) :: Maybe Int)  of
                  Just y -> if (y <= 10 && y >= 1) then Just (y-1) 
                            else Nothing
                  _      -> Nothing

data Orientation = Hor | Vert deriving (Show)
data BoatName = PatrolBoat | Submarine | Destroyer | Battleship | Carrier deriving (Show)
data Boat = Boat { origin :: (Int,Int), 
                   boatLength :: Int, 
                   orientation :: Orientation,
                   name :: BoatName } deriving (Show)

patrolBoat origin orientation = Boat origin 2 orientation PatrolBoat
submarine origin orientation = Boat origin 3 orientation Submarine
destroyer origin orientation = Boat origin 3 orientation Destroyer
battleship origin orientation = Boat origin 4 orientation Battleship
carrier origin orientation = Boat origin 5 orientation Carrier

asList :: Boat -> [(Int,Int)]
asList (Boat origin boatLength orientation _) =
  asListRec origin boatLength orientation
    where asListRec _ 0 _ = []
          asListRec (x,y) boatLength Hor = ((x,y):(boatRec (x,y+1) (boatLength - 1) Hor))
          asListRec (x,y) boatLength Vert = ((x,y):(boatRec(x+1,y) (boatLength - 1) Vert))

validPlacement :: Boat -> [Boat] -> Bool
validPlacement boat boats = all (\coord@(x,y) -> (x >= 0) && (x <= 9) && (y >= 0) && (y <= 9) && coord `notElem` coords) (asList boat)
  where coords = foldr ((++) . spaces) [] boats
  
placeBoat :: ((Int,Int) -> Orientation -> Boat) -> [Boat] -> IO [Boat]
placeBoat boatConstructor boats = do
  x <- randomRIO (0,9) :: IO Int
  y <- randomRIO (0,9) :: IO Int
  o <- randomRIO (0,1) :: IO Int
  let boat = boatConstructor (x,y) (toOrientation o)
      toOrientation 0 = Hor
      toOrientation 1 = Vert
    in case validPlacement boat boats of
         True -> return (boat:boats)
         False -> placeBoat boatConstructor boats

placeAllBoats :: IO [Boat]
placeAllBoats = placeAllBoatsRec [patrolBoat, destroyer, submarine, battleship, carrier] []
  where
    placeAllBoatsRec :: [((Int,Int) -> Orientation -> Boat)] -> [Boat] -> IO [Boat]
    placeAllBoatsRec [] boats = return boats
    placeAllBoatsRec boatTypes boats = do
      r <- randomRIO (0, (length boatTypes)-1)
      let boatType = boatTypes !! r
          remainingBoatTypes = (take r boatTypes) ++ (drop (r+1) boatTypes)
        in do 
             recBoats <- placeBoat boatType boats
             placeAllBoatsRec remainingBoatTypes recBoats

data Player = Player { hits :: [(Int,Int)],
                       misses :: [(Int,Int)],
                       ships :: [Boat] }

data Outcome = Hit | Miss | Sunk BoatType | Win

attack :: Player -> (Int,Int) -> Player -> (Outcome, Player)
attack p1@(Player hits misses _) coord (Player _ _ boats) =
  let 
      attackBoat :: Player -> (Int,Int) -> Boat -> (Outcome, Player)
      attackBoat p1@(Player hits _ _) coord boat
