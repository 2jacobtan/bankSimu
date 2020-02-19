import System.Random
import Data.List.Split (splitEvery)
import Data.List (foldl')
-- import Numeric.MCMC.Slice
  -- from https://hackage.haskell.org/package/speedy-slice
  -- wanted to use this but decided to hand-code slice sampling function instead

{- maths -}
-- CDF is 1 - e^(-t/a)
-- PDF is (1/a) * e^(-t/a) -- computed by hand the derivative of CDF
-- probability density function PDF must be used, for slice sampling
-- grahps of CDF and PDF at https://www.desmos.com/calculator/gqdoaxeo4i
a = 100
-- probability that next customer arrives t seconds later
f t = (1/a) * exp (-t/a)
-- inverse of f (formula done by hand)
f_inv y = a * (log (1/a) - log(y) )

-- slice sampling
-- algorithm from https://en.wikipedia.org/wiki/Slice_sampling
-- cf. YouTube https://www.youtube.com/watch?v=Qr6tg9oLGTA&t=1h8m44s
ss :: Double -> Double -> Double -> Double
ss x0 r1 r2 = -- random: r1 and r2
  x
    where
      y = r1 * f x0
      x = r2 * f_inv y

-- copy-pasta from:
-- https://www.notion.so/Bank-Simulation-94b50cdebe0b4da1b1297a7b01744682
-- To get the processing time for the customer, generate a random value between 0 and 1 and set x to it. The result of the equation will be the time in seconds that this customer takes to process.
b a b x = -- random: x
  p * x^(a-1) * (1-x)^(b-1)
    where
      p = 200

bY = b 2 5 -- Y for yellow
bR = b 2 2 -- R for red
bB = b 5 1 -- B for blue

-- random number generation
gen :: StdGen
gen = mkStdGen 1

randsAB :: Double -> Double -> [Double]
randsAB a b = randomRs (a,b) gen
rands = randsAB 0 1

{- Task -}
-- copy-pasta:
-- - Given only yellow customers, what are the average and maximum customer waiting times?
-- - Given only red customers, what are the average and maximum queue lengths in-front of the teller?
-- - Which type of customer(yellow, red or blue) gives the gives the closest value between the average and maximum customer waiting times?

{- Soloution v1 (abandoned) -}
-- generate list (length n) of tuples:
  -- (time taken to arrive after previous customer, processing time taken)
-- fold over the list to get required statistics

{- Soloution v2 -}
-- while folding over list of random numbers:
  -- generate next customer parameters: arrival, time needed
  -- keep track of statistics
-- each customer takes 3 random numbers to generate:
  -- slice sampling for arrival time takes 2
  -- processing time takes 1

-- Given only yellow customers, what are the average and maximum customer waiting times?
type Accumulator = ([(Double,Double,Double)], Double, Double, Double, Double)
task1 :: (Double -> Double) -> Int -> (Double, Double) -- returns (waitedTime, maxTime)
task1 bColour n = (waitedTime / fromIntegral n, maxTime)
-- task1 n = (q, ss3, waitedTime / fromIntegral n, maxTime, currTime3)
  -- for debug
  where
    (q, ss3, waitedTime, maxTime, currTime3) = foldl' go ([], 0, 0, 0, 0) (randList n)
    -- queue is a list of tuples, each representing a person waiting in line
      -- fst is time arrived, snd is process time
    go :: Accumulator -> [Double] -> Accumulator
    go (queue0, ss0, waitedTime0, maxTime0, currTime0) rList
      = (queue2, ss1, waitedTime2, maxTime2, currTime2)
      where
        (queue2, _, waitedTime2, maxTime2, currTime2)
          = processing (queue0, ss1, waitedTime0, maxTime0, currTime0)
        processing :: Accumulator -> Accumulator
        -- when nobody in queue: fast-forward until next customer arrives
        processing ([], ss', waitedTime', maxTime', currTime')
          = ([(currTime' + ss', b1, b1)], 0, waitedTime', maxTime', currTime' + ss')
        -- when there's a queue:
        processing (x:xs, ss', waitedTime', maxTime', currTime')
          -- when active customer finishes before next customer arrives:
          | pT' < ss' = processing (xs, ss' - pT', waitedTime' + (currTime' + pT' - pT - arrT), max maxTime' (currTime' + pT' - pT - arrT), currTime' + pT')
          -- when active customer finishes after next custoremr arrives: fast-forward until next customer arrives
          | otherwise = (((arrT, pT' - ss', pT):xs)++[(currTime' + ss',b1,b1)], 0, waitedTime', maxTime', currTime' + ss')
          where
            (arrT, pT', pT) = x
        -- extract the 3 random numbers
        [r0,r1,r2] = rList
        -- generate new customer's arrival time and process time
        ss1 = ss ss0 r0 r1
        b1 = bColour r2
randList :: Int -> [ [Double] ]
randList n = take n $ splitEvery 3 $ rands

main :: IO ()
main = do
  putStrLn "Task 1:"
  putStrLn "average wait time, maximum wait time"
  t1 10000
  putStrLn ""
  t3 10000
  t2 10000

t1 n = do
  print $ task1 bY n

t3 n = do
  putStrLn "Task 3:"
  print $ yellow
  print (let (avg,max) = yellow in max - avg)
  putStrLn "= Yellow difference\n"
  print $ red
  print (let (avg,max) = red in max - avg)
  putStrLn "= Red difference\n"
  print $ blue
  print (let (avg,max) = blue in max - avg)
  putStrLn "= Blue difference\n"
  where
    yellow = task1 bY n
    red = task1 bR n
    blue = task1 bB n
-- misc tests
test = do
  print $ take 1 $ rands
  print $ foldl' (++) [] (randList 2)


-- Hereon dedicated to task 2

t2 n = do
  putStrLn "Task 2:"
  putStrLn "average queue length, maximum queue length"
  print $ task2 bR n

-- task 2: copy-edit from task 1 code
type Accumulator2 = ([(Double,Double)], Double, Double, Double, Double,Double,Int)
task2 :: (Double -> Double) -> Int -> (Double, Int)
-- average queue length (over time) is calculated by:
  -- time-weighted sum of queue length, divide by total time
task2 bColour n = (qLenTime / currTime3, maxQLen)
  where
    (q, ss3, waitedTime, maxTime, currTime3, qLenTime, maxQLen) = foldl' go ([], 0, 0, 0, 0, 0, 0) (randList n)
    -- queue is a list of tuples, each representing a person waiting in line
      -- fst is time arrived, snd is process time
    go :: Accumulator2 -> [Double] -> Accumulator2
    go (queue0, ss0, waitedTime0, maxTime0, currTime0, qLenTime0, maxQLen0) rList
      = (queue2, ss1, waitedTime2, maxTime2, currTime2, qLenTime2, maxQLen2)
      where
        (queue2, _, waitedTime2, maxTime2, currTime2, qLenTime2, maxQLen2)
          = processing (queue0, ss1, waitedTime0, maxTime0, currTime0, qLenTime0, maxQLen0)
        processing :: Accumulator2 -> Accumulator2
        -- when nobody in queue: fast-forward until next customer arrives
        processing ([], ss', waitedTime', maxTime', currTime', qLenTime', maxQLen')
          = ([(currTime' + ss', b1)], 0, waitedTime', maxTime', currTime' + ss', qLenTime', maxQLen')
        -- when there's a queue:
        processing (x:xs, ss', waitedTime', maxTime', currTime', qLenTime', maxQLen')
          -- when active customer finishes before next customer arrives:
          | snd x < ss' = processing (xs, ss' - snd x, waitedTime' + currTime' - fst x, max maxTime' (currTime' - fst x), currTime' + snd x, qLenTime' + (snd x * fromIntegral (length (x:xs))), max maxQLen' (length (x:xs)))
          -- when active customer finishes after next custoremr arrives: fast-forward until next customer arrives
          | otherwise = (((fst x, snd x - ss'):xs)++[(currTime' + ss',b1)], 0, waitedTime', maxTime', currTime' + ss', qLenTime' + (snd x * fromIntegral (length (x:xs))), maxQLen')
        -- extract the 3 random numbers
        [r0,r1,r2] = rList
        -- generate new customer's arrival time and process time
        ss1 = ss ss0 r0 r1
        b1 = bColour r2