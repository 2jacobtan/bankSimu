import System.Random
-- import Numeric.MCMC.Slice -- wanted to use this but decided to hand-code slice sampling function instead

{- Task -}
-- copy-pasta:
-- - Given only yellow customers, what are the average and maximum customer waiting times?
-- - Given only red customers, what are the average and maximum queue lengths in-front of the teller?
-- - Which type of customer(yellow, red or blue) gives the gives the closest value between the average and maximum customer waiting times?

{- maths -}
-- CDF is 1 - e^(-t/a)
-- PDF is (1/a) * e^(-t/a)
-- probability density function PDF must be used, for slice sampling

-- probability that next customer arrives t seconds later
f t = (1/a) * exp (-t/a)
  where
    a = 100

-- inverse of f (formula done by hand)
f_inv y = a * (log (1/a) - log(y) )
  where
    a = 100

-- slice sampling
-- algorithm from https://en.wikipedia.org/wiki/Slice_sampling
-- cf. YouTube https://www.youtube.com/watch?v=Qr6tg9oLGTA&t=1h8m44s
ss :: Double -> Double -> Double -> (Double, Double)
ss x0 r1 r2 = -- random: r1 and r2
  (x,y)
    where
      y = r1 * f x0
      x = r2 * f_inv y

-- copy-pasta from:
-- https://www.notion.so/Bank-Simulation-94b50cdebe0b4da1b1297a7b01744682
-- To get the processing time for the customer, generate a random value between 0 and 1 and set x to it. The result of the equation will be the time in seconds that this customer takes to process.
b a b x = p * x^(a-1) * (1-x)^(b-1)
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



-- test rands
main :: IO ()
main = do
  print $ take 10 $ rands


