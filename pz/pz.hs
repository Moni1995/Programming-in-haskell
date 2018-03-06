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

data Move = U|D|L|R deriving(Eq, Show)

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

solve p = bfs [(p,0,[])] []
type Dist = Int -- for legibility
-- breadth-first search -- also A-star with monotone distance heuristic
-- OPEN (to be checked) CLOSED (seen) SOLUTION
bfs :: [(Position, Dist,[Move])] -> [Position] -> Maybe (Dist,[Move],Position)
bfs [] _ = Nothing
bfs ((p,d,ms):_)     _   | (solved p)      = Just (d, reverse ms, p)
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

playMoves :: [Move] -> Position -> Position
--playMoves m p = foldr (playOneMove p) p m
playMoves [] p     = p
playMoves (x:xs) p = playMoves xs (playOneMove p x)

play :: [Move]  -> Position
play []= Position [[0,1,2],[3,4,5],[6,7,8]]
play m = playMoves m (Position [[0,1,2],[3,4,5],[6,7,8]])

main :: IO ()
main = let s = play []
           p = play [D, R, R, U]
           in do putStrLn ("Start is " ++ (show s))
                 putStrLn ("A mixed-up position is "++(show p))
                 putStrLn ("and it can be solved in "++(show (solve p)))


-- test case 1
--Main> main
--Start is ["_12","345","678"]
--A mixed-up position is ["31_","452","678"]
--and it can be solved in Just (4,[D,L,L,U],["_12","345","678"])


-- test case 2
--Main> play [D,D,R,U]
--["312","6_5","748"]
--Main> solve (play [D,D,R,U])
--Just (4,[D,L,U,U],["_12","345","678"])

-- test case 3
-- *Main> play [D,R,D,R]
--["312","475","68_"]
-- *Main> solve (play [D,R,D,R])
-- Just (4,[L,U,L,U],["_12","345","678"])

-- test case 4
-- Main>  play [D,R,D,L]
--["312","475","_68"]
-- Main>  solve (play [D,R,D,L])
--Just (4,[R,U,L,U],["_12","345","678"])
