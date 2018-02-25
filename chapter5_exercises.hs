-- exercise 1

pyths :: Int -> [(Int, Int, Int)]
pyths n = [(x,y,z)|x <- [1..n],y <-[1..n],z <- [1..n], x^2 + y^2 == z^2]

-- exercise 2
factor' n = [x|x <- [1..n-1], n `mod` x == 0]
perfect n = [x|x <- [1..n], sum (factor' x) == x]

-- another way to solve it
perfect' n = [x|x<-[1..n], sum ((\x ->[x' | x' <- [1..x-1], x `mod` x' == 0]) x) == x]

-- exercise 3
