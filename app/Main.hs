module Main where
import qualified Data.Map.Strict as M
import Data.List (maximumBy)
import Data.Ord (comparing)
import Data.Array (Array, listArray, (!))
import Text.Printf (printf)

main :: IO ()
main = do
  mapM_ printWinProb [2..50]

newtype Probability a
  = Probability { getProbabilities :: [(a, Double)]}
    deriving Show

instance Functor Probability where
  fmap f (Probability as) = Probability (map (\(a, p) -> (f a, p)) as)

instance Applicative Probability where
  pure a = Probability [(a, 1.0)]
  Probability fs <*> Probability as = 
    Probability([(f a, p*q) | (f,p) <- fs, (a,q) <- as])

instance Monad Probability where
  Probability as >>= f =
    Probability $
      concatMap
        (\(a, p) ->
          let Probability bs = f a
          in map (\(b, q) -> (b, p * q)) bs
        )
        as

roll :: Probability Int
roll = Probability [ (i,1/6) | i <- [1..6] ]

twoRolls :: Probability Int
twoRolls = do
  a <- roll
  b <- roll
  pure $ a + b

-- i'm using map to do the flattening in an easy way (combining same outcomes
-- in a probability space as keys in a map)
normalize :: (Ord a) => Probability a -> Probability a
normalize (Probability a) =
  Probability (M.toList (M.fromListWith (+) a))

-- idea: strictly implement rollDice, folding steps of combining rolls
step :: Probability Int -> Probability Int
step dist = normalize $ do
  s <- dist
  d <- roll
  pure (s + d)

rollDice :: Int -> Probability Int
rollDice n = foldl (\dist _ -> step dist) (Probability [(0, 1.0)]) [1..n]

-- running mode $ rollDice 100 got me (350, 2.33226e-2)
mode :: Probability a -> (a, Double)
mode (Probability a) = maximumBy (comparing snd) a

-- i gave the rollingStones array an arbitrary upper bound of 200
rollingStones :: Array Int (Probability Int)
rollingStones = stoneprobs
  where
    stoneprobs = listArray (0, 200) [getDist i | i <- [0..200]]
    getDist :: Int -> Probability Int
    getDist 0 = pure 0
    getDist i = normalize $ do
      r <- roll
      if r >= i
        then pure 1
        else (+1) <$> (stoneprobs ! (i - r))

firstPlayerWinProb :: Int -> Double
firstPlayerWinProb n =
  sum[p | (k, p) <- getProbabilities (rollingStones ! n), odd k]

printWinProb :: Int -> IO ()
printWinProb n =
  printf "%d: %.2f%%\n" n (100 * firstPlayerWinProb n)
