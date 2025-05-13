{-# LANGUAGE BangPatterns     #-}
{-# LANGUAGE DeriveAnyClass   #-}
{-# LANGUAGE DeriveGeneric    #-}

module Main (main) where

import qualified Data.ByteString.Lazy        as B
import           Data.Aeson                  (eitherDecode, FromJSON)
import           Data.Either                 (partitionEithers)
import           Data.List                   (delete, maximumBy)
import           Data.Ord                    (comparing)
import           GHC.Generics                (Generic)
import           Control.DeepSeq             (NFData)
import           Control.Parallel.Strategies (rdeepseq, parMap, withStrategy, parListChunk)
import           System.Directory            (getDirectoryContents)
import           System.Random.SplitMix      (SMGen, mkSMGen, splitSMGen
                                             , nextDouble)
import           Data.Word                   (Word64)

import qualified Data.Vector           as V       -- boxed
import qualified Data.Vector.Unboxed    as UV      -- flat, unboxed

data Quote = Quote
  { date  :: !String
  , close :: !Double
  } deriving (Show, Eq, Generic, NFData)

instance FromJSON Quote

data StockHistory = StockHistory
  { symbol     :: !String
  , historical :: ![Quote]
  } deriving (Show, Eq, Generic, NFData)

instance FromJSON StockHistory

newtype HistoricalStockList = HistoricalStockList
  { historicalStockList :: [StockHistory]
  } deriving (Show, Eq, Generic, NFData)

instance FromJSON HistoricalStockList

loadStockFile :: FilePath -> IO (Either String HistoricalStockList)
loadStockFile fp = eitherDecode <$> B.readFile fp

combinations :: Int -> [a] -> [[a]]
combinations 0 _      = [[]]
combinations _ []     = []
combinations k (x:xs)
  | k < 0     = []
  | otherwise = map (x:) (combinations (k-1) xs)
           ++  combinations k         xs

weightLists :: Int           -- ^ how many weight vectors
            -> Int           -- ^ dimension of each vector
            -> Word64        -- ^ RNG seed
            -> [UV.Vector Double]
weightLists m k seed = parMap rdeepseq (mkWeights k) gens
  where
    gens :: [SMGen]
    gens = take m $ iterate (snd . splitSMGen) (mkSMGen seed)

    mkWeights :: Int -> SMGen -> UV.Vector Double
    mkWeights n g0 =
      let !(ws, _) = pull n g0
          !s       = UV.sum ws
      in  UV.map (/ s) ws

    pull :: Int -> SMGen -> (UV.Vector Double, SMGen)
    pull 0 !g = (UV.empty, g)
    pull n !g =
      let (!u,  g1)  = nextDouble g
          (!us, g2)  = pull (n-1) g1
      in  (UV.cons u us, g2)

type ReturnTable = V.Vector (UV.Vector Double)

mkReturnTable :: [StockHistory] -> ReturnTable
mkReturnTable stocks =
  let priceSeries   = map (map close . historical) stocks
      tMin          = minimum (map length priceSeries)
      trimmedPrices = map (UV.fromList . take tMin) priceSeries
      returnsCols   = map perStock trimmedPrices
  in  V.fromList returnsCols
  where
    perStock :: UV.Vector Double -> UV.Vector Double
    perStock ps = UV.zipWith (\p1 p0 -> (p1 - p0) / p0) (UV.tail ps) ps

sharpeFast
  :: ReturnTable           -- ^ t × n matrix, column-major
  -> UV.Vector Double      -- ^ weight vector (Σw = 1)
  -> Double
sharpeFast table w =
  let
      weightedCols :: V.Vector (UV.Vector Double)
      weightedCols =
        V.imap (\j col -> UV.map (* (w UV.! j)) col) table

      portfolioR :: UV.Vector Double      -- length = #days
      portfolioR =
        V.foldl1' (UV.zipWith (+)) weightedCols

      t :: Double
      t = fromIntegral (UV.length portfolioR)

      meanR :: Double
      meanR = UV.sum portfolioR / t

      variance :: Double
      variance =
        UV.foldl' (\acc r -> let d = r - meanR
                             in  acc + d*d) 0.0 portfolioR
        / t

      stdDev = sqrt variance
  in
      (252 * meanR) / (sqrt 252 * stdDev)


bestSharpePerComb
  :: [UV.Vector Double]             -- ^ pre-generated weights
  -> [StockHistory]                 -- ^ one combination of stocks
  -> (Double, [String], UV.Vector Double)
bestSharpePerComb weightVectors stocks =
  let table     = mkReturnTable stocks
      (best, w) = maximumBy (comparing fst) $
        parMap rdeepseq
               (\v -> (sharpeFast table v, v))
               weightVectors
  in  (best, map symbol stocks, w)

main :: IO ()
main = do
  stockFiles <- fmap (delete "." . delete "..")
                $ getDirectoryContents "data"
  results    <- mapM (loadStockFile . ("data/" ++)) stockFiles
  let (bad, good) = partitionEithers results
  mapM_ (putStrLn . ("JSON error: " ++)) bad
  let allStocks = concatMap historicalStockList good
  putStrLn $ "Merged symbols (" ++ show (length allStocks) ++ "): "
  print (map symbol allStocks)

  let combs = combinations 25 allStocks

  let k = 10
  let weightVecs = weightLists k 25 42

  let perComb = withStrategy (parListChunk 256 rdeepseq)
                $ map (bestSharpePerComb weightVecs) combs

  putStrLn "Finished evaluating every combination!"

  let (bestSharpe, names, bestW) = maximumBy (comparing (\(s,_,_) -> s)) perComb

  putStrLn "\n*** Portfolio with the best Sharpe ***"
  putStrLn $ "Sharpe: " ++ show bestSharpe
  putStrLn $ "Stocks : " ++ show names
  putStrLn $ "Weights: " ++ show (UV.toList bestW)
