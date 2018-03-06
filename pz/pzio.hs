import Data.Maybe
import Data.Char
import Data.List


data Position = Position [[Int]]|Empty deriving (Eq)

posList :: Position -> [[Int]]
posList Empty = []
posList (Position a) = a


instance Show Position where
  show (Position p) = show list
                          where list = replace2d (findSpace (Position p)) '_' (convertIntToChar p)

convertIntToChar :: [[Int]] -> [[Char]]
convertIntToChar p = [map intToDigit x|x <- p]

-- check if the position is at solved state
solved :: Position -> Bool
solved p = (p == Position [[0,1,2],[3,4,5],[6,7,8]])

data Move = U|D|L|R deriving(Eq, Show, Read)


-- if it cannot move to a direction , predule will throw exception
moves :: Position -> [(Move,Position)]
moves p = if checkIfCanGoU p && checkIfCanGoD p && checkIfCanGoL p && checkIfCanGoR p then [(U, goUp p),(D, goDown p),(L, goLeft p),(R, goRight p)]
          else if checkIfCanGoU p && checkIfCanGoD p && checkIfCanGoL p then [(U, goUp p),(D, goDown p),(L, goLeft p)]
          else if checkIfCanGoU p && checkIfCanGoD p && checkIfCanGoR p then [(U, goUp p),(D, goDown p),(R, goRight p)]
          else if checkIfCanGoD p && checkIfCanGoL p && checkIfCanGoR p then [(D, goDown p),(L, goLeft p),(R, goRight p)]
          else if checkIfCanGoL p && checkIfCanGoR p && checkIfCanGoU p then [(L, goLeft p),(U, goUp p),(R, goRight p)]
          -- 2 direction available
          else if checkIfCanGoD p && checkIfCanGoL p then [(D, goDown p),(L, goLeft p)]
          else if checkIfCanGoD p && checkIfCanGoR p then [(D, goDown p),(R, goRight p)]
          else if checkIfCanGoU p && checkIfCanGoL p then [(U, goUp p),(L, goLeft p)]
          else if checkIfCanGoU p && checkIfCanGoR p then [(U, goUp p),(R, goRight p)]
          else []


--moveToLeft :: Position -> (Move,Position)
--moveToLeft = (L, Position p) where p =

checkIfCanGoU :: Position -> Bool
checkIfCanGoU p = fst (findSpace p) == 1 || fst (findSpace p) == 2

checkIfCanGoD :: Position -> Bool
checkIfCanGoD p = fst (findSpace p) == 0 || fst (findSpace p) == 1

checkIfCanGoL :: Position -> Bool
checkIfCanGoL p = snd (findSpace p) == 1 || snd (findSpace p) == 2

checkIfCanGoR :: Position -> Bool
checkIfCanGoR p = snd (findSpace p) == 0 || snd (findSpace p) == 1

-- find where the index is
findSpace :: Position -> (Int, Int)
findSpace p = unpack . find (isJust . fst ) . (`zip` [0..]). map (elemIndex 0) $ posList p
    where
            unpack a = case a of
             Just (Just x, y) -> (y,x)
             _ -> (100,100)  -- we don't have to care about this case.

-- given the index and find the number is the list
findNumWithIndex :: Position -> (Int,Int) -> Int
findNumWithIndex p (x,y) = list!!(3*x + y)
    where list  = concat' (posList p)

--               p        where zero is      number     final pos
swapWithZero :: Position -> (Int, Int) -> (Int, Int) -> Position
swapWithZero p (x,y) (a,b) = Position ( replace2d (a,b) 0 (replace2d (x,y) (findNumWithIndex p (a,b)) (posList p)))

-- flatten a 2d list
concat' :: [[a]] -> [a]
concat' xss = [x | xs <- xss, x <- xs]

-- replace items in a simple list
replace :: Int -> a -> [a] -> [a]
replace _ _ [] = []
replace i x xs = take i xs ++ [x] ++ drop (i+1) xs

-- replace items in a 2D list
replace2d :: (Int, Int) -> a -> [[a]] -> [[a]]
replace2d (_, _) _ [] = []
replace2d (r,c) x xs =
    let row_to_replace_in = xs !! r
        new_row = replace c x row_to_replace_in
    in replace r new_row xs

-- find the spot next to our index (different directions. Helper function for go directions)
findLeft :: (Int, Int) -> (Int, Int)
findLeft (x,y) = (x, y-1)

findRight :: (Int, Int) -> (Int, Int)
findRight (x,y) = (x, y+1)

findUp :: (Int, Int) -> (Int, Int)
findUp (x,y) = (x-1, y)

findDown :: (Int, Int) -> (Int, Int)
findDown (x,y) = (x+1, y)

-- go to the directions
goLeft :: Position -> Position
goLeft p = swapWithZero p (findSpace p) (findLeft (findSpace p))

goRight :: Position -> Position
goRight p = swapWithZero p (findSpace p) (findRight (findSpace p))

goUp :: Position -> Position
goUp p = swapWithZero p (findSpace p) (findUp (findSpace p))

goDown :: Position -> Position
goDown p = swapWithZero p (findSpace p) (findDown (findSpace p))



type Dist = Int -- for legibility
-- breadth-first search -- also A-star with monotone distance heuristic
-- OPEN (to be checked) CLOSED (seen) SOLUTION
bfs :: [(Position, Dist,[Move])] -> [Position] -> Maybe (Dist)
bfs [] _ = Nothing
bfs ((p,d,ms):_)     _   | (solved p)      = Just (d)
bfs ((p,d,ms):more) seen | otherwise = bfs (more++new) seen'
    where seen' = p:seen
          new = mark ((moves p)  `except` seen')
          mark = map (\(m,p') -> (p', d+1, m:ms))
-- eliminate all (Move, Position) that we’ve already seen
--             POSSIBLES        SEEN        UNSEEN-POSSIBLES
except :: [(Move, Position)] -> [Position] -> [(Move, Position)]
except x []     = x
except [] _     = []
except x (s:ss) = except (remove' x s) ss


remove' :: [(Move, Position)] -> Position -> [(Move, Position)]
remove' xs p = [c | c <- xs, snd c /= p]

-- helper function for play moves
playOneMove :: Position -> Move -> Position
playOneMove p m | m == D = goDown p
                | m == U = goUp p
                | m == L = goLeft p
                | otherwise = goRight p

prompt :: Position -> IO ()
prompt pos = do
    print pos
    print (determineStatus pos)
    putStr   "Move?"
    command <- getLine
    interpret command pos

determineSteps ::Position -> Maybe Int
determineSteps pos =  bfs [(pos,0,[])] []

maybeToChar :: Maybe Int -> Char
maybeToChar Nothing  = ' '
maybeToChar (Just a) = intToDigit a

determineStatus::Position -> String
determineStatus pos | solved pos == False = "-- Solved in " ++ [maybeToChar (determineSteps pos)]
                    | otherwise = "-- Solved"

interpret :: String -> Position -> IO ()
interpret  "U"  pos = prompt (playOneMove pos U)
interpret  "D"  pos = prompt (playOneMove pos D)
interpret  "R"  pos = prompt (playOneMove pos R)
interpret  "L"  pos = prompt (playOneMove pos L)
interpret  "S"  pos = prompt (Position [[0,1,2],[3,4,5],[6,7,8]])
interpret  "q"  pos = return ()
interpret   p   pos = do
    putStrLn ("Invalid command: " ++ p)
    prompt pos


main :: IO ()
main = do
       prompt (Position [[0,1,2],[3,4,5],[6,7,8]])


-- test cases
-- *Main> main
-- ["_12","345","678"]
-- "-- Solved"
-- Move?D
-- ["312","_45","678"]
-- "-- Solved in 1"
-- Move?S
-- ["_12","345","678"]
-- "-- Solved"
-- Move?D
-- ["312","_45","678"]
-- "-- Solved in 1"
-- Move?R
-- ["312","4_5","678"]
-- "-- Solved in 2"
-- Move?L
-- ["312","_45","678"]
-- "-- Solved in 1"
-- Move?U
-- ["_12","345","678"]
-- "-- Solved"
-- Move?q

-- *Main> main
-- ["_12","345","678"]
-- "-- Solved"
-- Move?D
-- ["312","_45","678"]
-- "-- Solved in 1"
-- Move?S
-- ["_12","345","678"]
-- "-- Solved"
-- Move?D
-- ["312","_45","678"]
-- "-- Solved in 1"
-- Move?R
-- ["312","4_5","678"]
-- "-- Solved in 2"
-- Move?L
-- ["312","_45","678"]
-- "-- Solved in 1"
-- Move?U
-- ["_12","345","678"]
-- "-- Solved"
-- Move?q
-- *Main> main
-- ["_12","345","678"]
-- "-- Solved"
-- Move?D
-- ["312","_45","678"]
-- "-- Solved in 1"
-- Move?D
-- ["312","645","_78"]
-- "-- Solved in 2"
-- Move?R
-- ["312","645","7_8"]
-- "-- Solved in 3"
-- Move?R
-- ["312","645","78_"]
-- "-- Solved in 4"
-- Move?U
-- ["312","64_","785"]
-- "-- Solved in 5"
-- Move?U
-- ["31_","642","785"]
-- "-- Solved in 6"
-- Move?D
-- ["312","64_","785"]
-- "-- Solved in 5"
-- Move?D
-- ["312","645","78_"]
-- "-- Solved in 4"
-- Move?S
-- ["_12","345","678"]
-- "-- Solved"
