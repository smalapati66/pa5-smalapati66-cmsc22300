module Main where

main :: IO ()
main = putStrLn "Hello, Haskell!"

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