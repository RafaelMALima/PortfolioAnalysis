# Portfolio generator

This package uses historical data to calculate the sharpe rating of different portfolios, made using combinations of the Dow 30.

To run this project, with cabal and ghc installed run  
<code>cabal build</code>  
<code>cabal run Paralelismo -- +RTS -N</code> 

The project also includes a module for dynamically fetching the data used. To use it, head to  
https://site.financialmodelingprep.com/  
Register, and access the dashboard. There, get your api key, and add it to your path. The name used for the environment variable in this project is 

<code>export FMP_KEY=YOUR_KEY_HERE </code>  

Afterwards, to download the data, simply run 
<code>cabal run Dow30</code>  



Despite the parallelism, the project still takes a significant amount of time. With a k of 1000, it took around 2h for the project to run.
Lower values for k, such as 10 or 100 yield significantly faster results

The best sharpe value found was of 3.448845328463113, with a portfolio made up by:

| Ticker | Value    |
| ------ | -------- |
| CAT    | 0.006230 |
| CVX    | 0.068834 |
| KO     | 0.090078 |
| NKE    | 0.089598 |
| NVDA   | 0.005665 |
| PG     | 0.020839 |
| CRM    | 0.001027 |
| SHW    | 0.025353 |
| TRV    | 0.002583 |
| UNH    | 0.058155 |
| VZ     | 0.071946 |
| V      | 0.026970 |
| WMT    | 0.054668 |
| DIS    | 0.014118 |
| GS     | 0.000236 |
| HD     | 0.006228 |
| HON    | 0.018817 |
| IBM    | 0.060433 |
| MMM    | 0.097415 |
| AXP    | 0.007616 |
| AMGN   | 0.088728 |
| JNJ    | 0.071545 |
| MCD    | 0.035703 |
| MRK    | 0.063276 |
| MSFT   | 0.013938 |
