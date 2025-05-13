{-# LANGUAGE DeriveGeneric #-} -- Add this extension
{-# LANGUAGE BangPatterns  #-} -- Add this extension
{-# LANGUAGE DeriveAnyClass#-} -- Add this extension

import qualified Data.ByteString.Lazy as B -- For reading file
import GHC.Generics (Generic)           -- For deriving
import Data.Aeson (FromJSON, decode, eitherDecode) -- aeson functions
import System.Directory
import Data.Either
import Data.List
import Data.List.Split
import Data.Ord
import Control.Parallel.Strategies
import System.Random.SplitMix
import Data.Word

-- Add 'Generic' and derive 'FromJSON' for aeson
data Quote = Quote {  date             :: String
                    --, open             :: Double
                    --, high             :: Double
                    --, low              :: Double
                    , close            :: Double
                    --, adjClose         :: Double
                    --, volume           :: Int
                    --, unadjustedVolume :: Int
                    --, change           :: Double
                    --, changePercent    :: Double
                    --, vwap             :: Double
                    --, label            :: String
                    --, changeOverTime   :: Double 
} deriving (Show, Eq, Generic, NFData)

instance FromJSON Quote

data StockHistory = StockHistory { symbol     :: String
, historical :: [Quote]  } deriving (Show, Eq, Generic, NFData)
instance FromJSON StockHistory

data HistoricalStockList = HistoricalStockList { historicalStockList :: [StockHistory]  } deriving (Show, Eq, Generic, NFData)
instance FromJSON HistoricalStockList -- This instance will now work correctly with aeson!

loadStockFile :: FilePath -> IO (Either String HistoricalStockList)
loadStockFile fp = do
  rawBytes <- B.readFile fp -- Read as lazy ByteString
  -- Use eitherDecode for better error messages
  pure $ eitherDecode rawBytes

combinations :: Int -> [a] -> [[a]]
combinations 0 _      = [[]]
combinations _ []     = []
combinations k (x:xs)
  | k < 0             = []                         -- guard against misuse
  | otherwise         = map (x:) (combinations (k-1) xs)
                       ++       combinations k     xs


--sampleW :: Int -> IO [Double]
--sampleW k = do
--  xs <- Par.replicateM k (randomRIO (1e-12, 1.0))  -- avoid exact zeros
--  let s = sum xs
--  pure $ map (/ s) xs
--
---- Re-sample until the 20 % constraint is satisfied.
--generateWeights :: Int -> IO [Double]
--generateWeights n = do
--   ws <- sampleW n
--   if all (<= 0.20) ws then pure ws else generateWeights n


weightLists :: Int -> Int -> Word64 -> [[Double]]
weightLists m k seed =

  -- Parallel evaluation is entirely optional; remove `parMap rdeepseq`
  -- if you want to keep it 100 % sequential.
  parMap rdeepseq (mkWeights k) gens
  where
    -- Split the root generator repeatedly to obtain as many
    -- independent sub-generators as we need.
    gens :: [SMGen]
    gens = take m $ iterate (snd . splitSMGen) (mkSMGen seed)

    ----------------------------------------------------------------------------
    -- Given a generator, make one weight vector -------------------------------
    ----------------------------------------------------------------------------
    mkWeights :: Int -> SMGen -> [Double]
    mkWeights n g0 = let !(raw, _) = pull n g0
                         !s       = sum raw            -- normalising constant
                     in  map (/ s) raw                -- every list sums to 1

    -- pull n raw uniform(0,1) doubles and thread the generator state
    pull :: Int -> SMGen -> ([Double], SMGen)
    pull 0 !g = ([], g)
    pull n !g =
      let (!u,  g1)  = nextDouble g
          (!us, g2)  = pull (n-1) g1
      in  (u:us, g2)

weightReturns :: [StockHistory] -> [Double] -> [Double]
weightReturns !stocks !wRaw
  | null stocks                 = []
  | length stocks /= length wRaw = error "weightReturns: weights / stocks length mismatch"
  | otherwise                   = portfolioReturns
  where
    --------------------------------------------------------------------------
    -- 1.  Normalise weights so Σw = 1
    --------------------------------------------------------------------------
    !w =
      let s = sum wRaw
      in  if s == 0 then error "weightReturns: sum of weights is zero"
                    else map (/ s) wRaw

    --------------------------------------------------------------------------
    -- 2.  Pull out close-price series and equalise lengths
    --------------------------------------------------------------------------
    priceSeries :: [[Double]]
    priceSeries = map (map close . historical) stocks         -- [[P_t]]
    
    !tMin = minimum (map length priceSeries)                  -- shortest series
    !trimmedPrices = map (take tMin) priceSeries              -- force equal length

    --------------------------------------------------------------------------
    -- 3.  Per-stock simple daily returns
    --------------------------------------------------------------------------
    returnsPerStock :: [[Double]]                             -- length = tMin-1
    returnsPerStock =
      map (\ps -> zipWith (\p1 p0 -> (p1 - p0) / p0) (tail ps) ps)
          trimmedPrices

    --------------------------------------------------------------------------
    -- 4.  Weight every stock’s return series and sum across stocks
    --------------------------------------------------------------------------
    weighted :: [[Double]]
    weighted = zipWith (\wi rs -> map (wi *) rs) w returnsPerStock

    portfolioReturns :: [Double]
    portfolioReturns = foldl1' (zipWith (+)) weighted


stdev :: [Double] -> Double 
stdev xs = sqrt . average . map ((^2) . (-) axs) $ xs
           where average = (/) <$> sum <*> realToFrac . length
                 axs     = average xs

-- No Double average for absolutely no reason. Imagine using float in the big 25
average :: [Double] -> Double
average xs =
  let !s = foldl' (+) 0 xs
  in  s / fromIntegral (length xs)

sharpe :: [StockHistory] -> [Double] -> Double
sharpe c w =
  let wr      = weightReturns c w          -- daily portfolio returns
      mr      = average wr
      stdvr   = stdev wr
      annMr   = mr * 252
      annStdvr= stdvr * sqrt 252
  in  annMr / annStdvr

first3 :: (x, y, z) -> x
first3 (x, _, _) = x        -- extract the first component

main :: IO ()

main = do
  stockFiles <- getDirectoryContents "data" 
  let cleanFs = delete "." . delete ".."
  let a = cleanFs stockFiles
  b <- mapM (loadStockFile . ("data/" ++)) a
  let (bad, good) = partitionEithers b



  --ṕrint and report errors from parsing JSON data
  mapM_ (\err -> do putStrLn ("JSON error: " ++ err )) bad
  let histData = concatMap historicalStockList good
  --checked if all data was read properly
  putStrLn $ "Merged symbols (" ++ show (length histData) ++ "):"
  print (map symbol histData)
  let !comb = combinations 25 histData


  --IMPORTANT - number of random samples per combination
  let k = 1000
  let !ws = weightLists k 25 42
  print (head ws)

  let bestTriple = maximumBy (comparing first3)

  let !bct = parMap rdeepseq  (\c -> bestTriple (parMap rdeepseq -- Best combination triple - best triple from each combination
                                (\w -> ((sharpe c w), (map symbol c), w)) ws
                              )) comb

  print "Finished calculating best sharpe of each combination"
  print "Processing sharpe of each chunk"

  let bctChunks  = chunksOf 1000 bct --
  let !bestSharpes = parMap rdeepseq (\c -> bestTriple c) bctChunks 
  print "Finished calculating best sharpe of each chunk"

  print "Len"
  print (length bestSharpes)

  print "Resulting chunks:"
  print bestSharpes

  let !bestPortfolio = bestTriple bestSharpes

  
  print "Portfolio with the best sharpe"
  print bestPortfolio


  -- print (head b)
