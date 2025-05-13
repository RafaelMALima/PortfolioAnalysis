import System.Environment (getEnv)
import Network.Curl
import Data.List          (intercalate)
import Control.Monad      (when)
import System.IO          (appendFile)

queryDates :: String
dow30 :: [[String]]
dow30     = --split into lists of 5 to comply with api limits of four stocks per request
              [["MMM","AXP","AMGN","AMZN","AAPL"],
              ["BA","CAT","CVX","CSCO","KO"],
              ["DIS","GS","HD","HON","IBM"],
              ["JNJ","JPM","MCD","MRK","MSFT"],
              ["NKE","NVDA","PG","CRM","SHW"],
              ["TRV","UNH","VZ","V","WMT"]]
queryDates = "?from=2024-10-01&to=2024-12-31&apikey="

baseUrl :: String
baseUrl = "https://financialmodelingprep.com/api/v3/historical-price-full/"

-- | Pure helper: takes a key and glues the URL together.
buildUrl :: String -> String -> String -> String -> String
buildUrl baseUrl stocks queryDates k = baseUrl ++ stocks ++ queryDates ++ k

main :: IO ()
main = withCurlDo $ do
  let stocks_groups = map (intercalate ",") dow30
  mapM_ (\x -> do 
                api_key <- (getEnv "FMP_KEY")        -- url :: String
                let url = buildUrl baseUrl x queryDates api_key
                (code, body) <- curlGetString url
                                     [ CurlHttpGet True
                                     , CurlUserAgent "curl-hs/1.0" ]
                when (code /= CurlOK) $
                       fail ("libcurl error: " ++ show code)
                putStrLn body
                writeFile ("data/dow30_2024_q4" ++ x ++ ".json") body
        ) stocks_groups
  
