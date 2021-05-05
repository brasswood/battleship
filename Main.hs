module Main where

import Text.Read
import System.Random
import Data.Char

main :: IO ()
main = return ()

blankBoard :: Board
blankBoard = Board ((replicate 10 . replicate 10) BlankSquare)

addShot :: (Int,Int) -> Square -> Board -> Board
addShot (x,y) square (Board board) = Board ((take x board) ++ [row] ++ (drop (x+1) board))
  where row = (take y (board !! x)) ++ [square] ++ (drop (y+1) (board !! x))

generateBoard :: [(Int,Int)] -> [(Int,Int)] -> Board
generateBoard [] [] = blankBoard
generateBoard (hit:hits) [] = addShot hit HitSquare (generateBoard hits [])
generateBoard hits (miss:misses) = addShot miss MissSquare (generateBoard hits misses)


data Square = HitSquare | MissSquare | BlankSquare deriving (Show, Eq)

newtype Board = Board [[Square]]

instance Show Board where
  show (Board (row:rows)) = showRow row ++ "\n" ++ show (Board rows)
    where showRow (HitSquare:squares)   = "X" ++ showRow squares
          showRow (MissSquare:squares)  = "O" ++ showRow squares
          showRow (BlankSquare:squares) = "W" ++ showRow squares
          showRow []              = ""
  show (Board [])         = ""


squareAt :: String -> Board -> Maybe Square
squareAt coord (Board arr) = case (getRow coord, getCol coord) of
                         (Just row, Just col) -> Just (arr !! row !! col)
                         _                    -> Nothing

asTuple :: String -> Maybe (Int,Int)
asTuple str = case (getRow str, getCol str) of
                (Just row, Just col) -> Just (row, col)
                _                    -> Nothing

fromTuple :: (Int,Int) -> String
fromTuple (x,y) = ((chr (x+65)):(show (y+1)))

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

data Orientation = Hor | Vert deriving (Show, Eq)
data BoatName = PatrolBoat | Submarine | Destroyer | Battleship | Carrier deriving (Eq)
instance Show BoatName where
  show PatrolBoat = "Patrol Boat"
  show Submarine = "Submarine"
  show Destroyer = "Destroyer"
  show Battleship = "Battleship"
  show Carrier = "Carrier"

data Boat = Boat { coords :: [(Int,Int)], name :: BoatName } deriving (Eq)

instance Show Boat where
  show (Boat coords name) = (show name) ++ " at " ++ (show (map fromTuple coords))

showBoats :: [Boat] -> String
showBoats [] = ""
showBoats (boat:boats) = (show boat) ++ "\n" ++ (showBoats boats)

boat :: (Int,Int) -> Int -> Orientation -> BoatName -> Boat
boat origin length orientation = Boat (boatRec origin length orientation)
  where boatRec :: (Int,Int) -> Int -> Orientation -> [(Int,Int)]
        boatRec _ 0 _ = []
        boatRec (x,y) length Hor = ((x,y):boatRec (x,y+1) (length-1) Hor)
        boatRec (x,y) length Vert = ((x,y):boatRec (x+1,y) (length-1) Vert)

patrolBoat origin orientation = boat origin 2 orientation PatrolBoat
submarine origin orientation = boat origin 3 orientation Submarine
destroyer origin orientation = boat origin 3 orientation Destroyer
battleship origin orientation = boat origin 4 orientation Battleship
carrier origin orientation = boat origin 5 orientation Carrier

validPlacement :: Boat -> [Boat] -> Bool
validPlacement boat boats = all validSquare (coords boat)
  where validSquare :: (Int,Int) -> Bool
        validSquare (x,y)
          | (x<0)||(x>9)||(y<0)||(y>9) = False
          | otherwise = (x,y) `notElem` (concat (map coords boats))

randomCoord :: IO (Int,Int)
randomCoord = do
  x <- randomRIO (0,9) :: IO Int
  y <- randomRIO (0,9) :: IO Int
  return (x,y)
  
placeBoat :: ((Int,Int) -> Orientation -> Boat) -> [Boat] -> IO [Boat]
placeBoat boatConstructor boats = do
  (x,y) <- randomCoord
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

data PlayerType = Human | Computer deriving (Show, Eq)
data Player = Player { hits :: [(Int,Int)],
                       misses :: [(Int,Int)],
                       ships :: [Boat],
                       playerType :: PlayerType } deriving (Show, Eq)

data Outcome = Hit | Miss | Sunk BoatName | Win BoatName deriving (Show, Eq)

attack :: Player -> (Int,Int) -> Player -> (Outcome, Player)
attack (Player hits misses boats ptype) shot (Player _ _ opponentBoats _) =
  let 
      subset :: (Foldable t, Eq a) => t a -> t a -> Bool
      subset lst1 lst2 = all ((flip elem) lst2) lst1
      attackBoat :: Boat -> Outcome
      attackBoat (Boat coords name)
        | shot `notElem` coords = Miss
        | shot `elem` hits = Hit
        | (shot `elem` coords) && (coords `subset` (shot:hits)) = 
            Sunk name
        | otherwise = Hit
      foldOutcome :: Boat -> Outcome -> Outcome
      foldOutcome boat Miss = attackBoat boat
      foldOutcome _ nonMiss = nonMiss
      foldedOutcome = foldr foldOutcome Miss opponentBoats
      totalPlayer Miss = Player hits (shot:misses) boats ptype
      totalPlayer _    = Player (shot:hits) misses boats ptype
    in case foldedOutcome of
      Sunk name -> if concat (map coords opponentBoats) `subset` 
                        (shot:hits) then (Win name, totalPlayer (Win name))
                   else (Sunk name, totalPlayer (Sunk name))
      nonSunk   -> (nonSunk, totalPlayer nonSunk)

game :: IO ()
game = do
  boats1 <- placeAllBoats
  boats2 <- placeAllBoats
  putStrLn "Psst... Here's player 2's board"
  putStrLn (showBoats boats2)
  let player1 = Player [] [] boats1 Human
      player2 = Player [] [] boats2 Computer
      gameLoop :: Player ->  Player -> IO ()
      gameLoop player1@(Player p1hits p1misses _ Human) player2 = do
        putStrLn ("Your board:")
        putStrLn (show (generateBoard p1hits p1misses))
        putStr "Guess: "
        p1guess <- getLine
        case asTuple p1guess of
          Nothing -> do putStrLn "Invalid guess."
                        gameLoop player1 player2
          Just shot -> let (outcome, newPlayer1) =
                              attack player1 shot player2 in
                         case outcome of
                           Miss -> do putStrLn "Miss."
                                      gameLoop player2 newPlayer1
                           Hit  -> do putStrLn "Hit!"
                                      gameLoop player2 newPlayer1
                           Sunk boatName -> do putStrLn ("You sunk my " ++ 
                                                 show boatName ++ "!")
                                               gameLoop player2 newPlayer1
                           Win boatName -> putStrLn ("You sunk my " ++
                                             show boatName ++ "!"
                                             ++ " You win!")
      gameLoop player1@(Player _ _ _ Computer) player2 = do
        shot <- randomCoord
        putStr ("I guess " ++ (fromTuple shot) ++ ". ")
        let (outcome, newPlayer1) = attack player1 shot player2 in
          case outcome of
            Miss -> do putStrLn "I missed."
                       gameLoop player2 newPlayer1
            Hit -> do putStrLn "I hit!"
                      gameLoop player2 newPlayer1
            Sunk boatName -> do putStrLn ("I sunk your " ++ (show boatName)
                                            ++ "!")
                                gameLoop player2 newPlayer1
            Win boatName -> putStrLn ("I sunk your " ++ (show boatName) ++
                                        "! I win!")
     in gameLoop player1 player2
