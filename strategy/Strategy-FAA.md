---
layout: page
title: Flexible Asset Allocation (FAA)
---


To install [Systematic Investor Toolbox (SIT)](https://github.com/systematicinvestor/SIT) please visit [About](/about) page.





The [Generalized Momentum and Flexible Asset Allocation (FAA): An Heuristic Approach by Wouter J. Keller and Hugo S.van Putten (2012)](http://papers.ssrn.com/sol3/papers.cfm?abstract_id=2193735)
backtest and live signal. For more details please see:
 
* [Generalized Momentum and Flexible Asset Allocation (FAA): An Heuristic Approach by Wouter J. Keller and Hugo S.van Putten (2012)](http://papers.ssrn.com/sol3/papers.cfm?abstract_id=2193735)
* [Flexible Asset Allocation](http://turnkeyanalyst.com/2013/01/flexible-asset-allocation/)
* [Asset Allocation Combining Momentum, Volatility, Correlation and Crash Protection](http://www.cxoadvisory.com/subscription-options/?wlfrom=%2F22480%2Fvolatility-effects%2Fasset-allocation-combining-momentum-volatility-correlation-and-crash-protectio%2F)

The [FAA Strategy](http://papers.ssrn.com/sol3/papers.cfm?abstract_id=2193735)
allocates across Price, Volatility, and Correlation Momentum.





Load historical data from Yahoo Finance:



{% highlight r %}
#*****************************************************************
# Load historical data
#*****************************************************************
library(SIT)
load.packages('quantmod')

tickers = '
US.STOCKS = VTI + VTSMX
FOREIGN.STOCKS = VEU + FDIVX
EMERGING>MARKETS=EEM + VEIEX
US.10YR.GOV.BOND = IEF + VFITX
REAL.ESTATE = VNQ + VGSIX
COMMODITIES = DBC + CRB
CASH = BND + VBMFX
'

# load saved Proxies Raw Data, data.proxy.raw
load('data.proxy.raw.Rdata')

data <- new.env()

getSymbols.extra(tickers, src = 'yahoo', from = '1970-01-01', env = data, raw.data = data.proxy.raw, auto.assign = T, set.symbolnames = T, getSymbols.fn = getSymbols.fn, calendar=calendar)
  for(i in data$symbolnames) data[[i]] = adjustOHLC(data[[i]], use.Adjusted=T)
bt.prep(data, align='remove.na', dates='::')

print(last(data$prices))
{% endhighlight %}



|           | US.STOCKS| FOREIGN.STOCKS| EMERGING>MARKETS| US.10YR.GOV.BOND| REAL.ESTATE| COMMODITIES|  CASH|
|:----------|---------:|--------------:|----------------:|----------------:|-----------:|-----------:|-----:|
|2015-02-06 |    106.33|          47.78|            39.82|           107.86|        85.2|       18.02| 83.13|
    




{% highlight r %}
#*****************************************************************
# Setup
#*****************************************************************
data$universe = data$prices > 0
	# do not allocate to CASH
	data$universe$CASH = NA 

prices = data$prices * data$universe
	n = ncol(prices)
{% endhighlight %}





Code Strategy Rules:



{% highlight r %}
#*****************************************************************
# Code Strategy
#******************************************************************
ret = diff(log(prices))

n.top = 3

mom.lookback = 80
vol.lookback = 80
cor.lookback = 80

weight=c(1, 0.5, 0.5)

hist.vol = sqrt(252) * bt.apply.matrix(ret, runSD, n = vol.lookback)

mom = (prices / mlag(prices, mom.lookback) - 1)[period.ends,]

#*****************************************************************
# Compute Average Correlation
#******************************************************************
avg.cor = data$weight * NA
for(i in period.ends[period.ends > cor.lookback]){
  hist = ret[(i - cor.lookback):i,]
    include.index = !is.na(colSums(hist))
  correlation = cor(hist[,include.index], use='complete.obs',method='pearson')
  avg.correlation = rowSums(correlation, na.rm=T)

  avg.cor[i,include.index] = avg.correlation
}


mom.rank = br.rank(mom)
cor.rank = br.rank(-avg.cor[period.ends,])
vol.rank = br.rank(-hist.vol[period.ends,])

avg.rank = weight[1]*mom.rank + weight[2]*vol.rank + weight[3]*cor.rank
meta.rank = br.rank(-avg.rank)

#absolute momentum filter 
weight = (meta.rank <= n.top)/rowSums(meta.rank <= n.top, na.rm=T) * (mom > 0)

# cash logic
weight$CASH = 1 - rowSums(weight,na.rm=T)

obj$weights$strategy = weight
{% endhighlight %}


![plot of chunk plot-6](/public/images/Strategy-FAA/plot-6-1.png) 

#Strategy Performance:
    




|           |strategy          |
|:----------|:-----------------|
|Period     |Jul1996 - Feb2015 |
|Cagr       |13.22             |
|Sharpe     |1.3               |
|DVR        |1.2               |
|Volatility |9.94              |
|MaxDD      |-16.1             |
|AvgDD      |-1.51             |
|VaR        |-0.96             |
|CVaR       |-1.46             |
|Exposure   |99.98             |
    


![plot of chunk plot-6](/public/images/Strategy-FAA/plot-6-2.png) 

#Monthly Results for strategy :
    




|     |Jan   |Feb   |Mar   |Apr   |May   |Jun   |Jul   |Aug   |Sep   |Oct   |Nov   |Dec   |Year  |MaxDD |
|:----|:-----|:-----|:-----|:-----|:-----|:-----|:-----|:-----|:-----|:-----|:-----|:-----|:-----|:-----|
|1996 |      |      |      |      |      |      |      | -0.2 |  1.7 |  2.2 |  4.0 |  4.1 | 12.3 | -1.9 |
|1997 |  0.3 | -0.6 | -2.4 | -1.1 |  6.0 |  1.5 |  2.4 | -7.3 |  5.3 |  0.0 | -0.9 |  1.5 |  4.0 | -7.3 |
|1998 |  0.3 |  2.3 |  3.2 |  1.6 | -0.6 |  0.2 | -0.6 | -3.2 |  2.5 | -0.6 |  0.3 |  3.4 |  8.9 | -5.6 |
|1999 | -1.6 | -2.9 |  2.3 |  2.7 | -3.1 |  4.4 | -0.9 |  1.2 |  2.5 |  0.3 |  4.3 |  9.4 | 19.4 | -6.5 |
|2000 | -3.4 |  4.8 |  0.6 | -2.5 |  3.3 |  2.8 |  1.7 |  2.5 |  1.0 | -1.4 |  3.1 |  0.9 | 13.7 | -5.6 |
|2001 |  0.7 | -0.4 | -1.2 |  0.3 |  1.0 |  2.3 |  1.0 | -0.5 | -0.2 |  0.3 | -1.6 |  0.2 |  1.8 | -4.8 |
|2002 |  1.4 |  1.9 |  3.0 |  0.3 |  0.4 |  2.5 | -0.8 |  2.9 |  2.9 | -0.8 | -0.5 |  1.6 | 15.9 | -4.6 |
|2003 |  2.6 |  1.0 | -2.1 |  0.7 |  6.2 |  1.9 |  0.3 |  4.2 |  0.4 |  3.5 |  2.9 |  5.4 | 30.3 | -4.3 |
|2004 |  2.8 |  2.3 |  2.5 | -6.5 |  1.7 | -1.3 |  2.1 |  1.2 |  0.1 |  2.6 |  4.0 |  3.0 | 15.0 | -7.5 |
|2005 | -3.4 |  4.2 | -4.0 | -2.2 |  0.5 |  2.7 |  1.3 |  0.4 |  1.3 | -4.0 |  3.0 |  4.1 |  3.6 | -8.3 |
|2006 |  7.0 | -2.7 |  3.6 |  2.5 | -6.2 | -0.8 |  2.2 |  0.6 |  1.3 |  3.4 |  3.1 | -1.6 | 12.5 |-16.1 |
|2007 |  3.4 | -3.0 |  0.3 |  2.9 |  1.9 |  1.3 | -0.1 |  0.0 |  7.0 |  7.2 | -1.4 |  1.8 | 22.9 | -8.9 |
|2008 | -0.9 |  6.2 |  0.3 |  1.1 |  1.6 | -0.1 | -3.2 | -1.6 | -0.4 | -2.2 |  5.2 |  5.1 | 11.1 |-11.7 |
|2009 | -2.7 | -0.6 |  1.8 |  6.4 |  5.8 | -2.3 |  6.2 |  0.9 |  4.4 | -2.6 |  5.3 |  0.6 | 25.0 | -9.2 |
|2010 | -6.7 |  4.4 |  5.2 |  3.5 | -3.4 | -2.7 |  3.8 |  2.2 |  3.9 |  3.7 | -1.7 |  4.5 | 16.9 |-10.1 |
|2011 |  1.9 |  4.2 |  0.5 |  4.4 | -1.6 | -2.7 |  2.4 |  0.2 | -6.2 | -0.4 |  0.2 |  2.7 |  5.1 | -9.3 |
|2012 |  4.1 |  1.5 | -0.1 |  1.6 | -2.6 | -0.1 |  1.5 | -0.1 |  0.6 | -1.2 |  1.3 |  1.8 |  8.5 | -4.6 |
|2013 |  1.5 |  0.1 |  2.3 |  3.1 | -2.2 | -1.7 |  2.2 | -1.9 |  0.5 |  2.8 |  0.9 |  0.7 |  8.4 | -8.5 |
|2014 | -2.0 |  3.4 |  0.5 |  1.5 |  0.9 |  1.8 |  0.7 |  2.6 | -3.6 |  2.1 |  1.9 |  0.6 | 10.8 | -5.3 |
|2015 |  2.8 | -0.2 |      |      |      |      |      |      |      |      |      |      |  2.6 | -1.5 |
|Avg  |  0.4 |  1.4 |  0.9 |  1.1 |  0.5 |  0.5 |  1.2 |  0.2 |  1.3 |  0.8 |  1.7 |  2.6 | 12.4 | -7.1 |
    


![plot of chunk plot-6](/public/images/Strategy-FAA/plot-6-3.png) 

#Trades for strategy :
    




|strategy         |weight |entry.date |exit.date  |nhold |entry.price |exit.price |return |
|:----------------|:------|:----------|:----------|:-----|:-----------|:----------|:------|
|EMERGING>MARKETS | 33.3  |2014-07-31 |2014-08-29 |29    | 43.20      | 44.42     | 0.94  |
|US.10YR.GOV.BOND | 33.3  |2014-07-31 |2014-08-29 |29    |102.08      |104.00     | 0.63  |
|REAL.ESTATE      | 33.3  |2014-07-31 |2014-08-29 |29    | 73.21      | 75.44     | 1.02  |
|US.STOCKS        | 33.3  |2014-08-29 |2014-09-30 |32    |102.87      |100.71     |-0.70  |
|EMERGING>MARKETS | 33.3  |2014-08-29 |2014-09-30 |32    | 44.42      | 40.97     |-2.59  |
|US.10YR.GOV.BOND | 33.3  |2014-08-29 |2014-09-30 |32    |104.00      |102.91     |-0.35  |
|US.STOCKS        | 50.0  |2014-09-30 |2014-10-31 |31    |100.71      |103.47     | 1.37  |
|US.10YR.GOV.BOND | 50.0  |2014-09-30 |2014-10-31 |31    |102.91      |104.49     | 0.77  |
|US.STOCKS        | 33.3  |2014-10-31 |2014-11-28 |28    |103.47      |106.04     | 0.83  |
|US.10YR.GOV.BOND | 33.3  |2014-10-31 |2014-11-28 |28    |104.49      |105.84     | 0.43  |
|REAL.ESTATE      | 33.3  |2014-10-31 |2014-11-28 |28    | 77.92      | 79.48     | 0.67  |
|US.STOCKS        | 33.3  |2014-11-28 |2014-12-31 |33    |106.04      |106.00     |-0.01  |
|US.10YR.GOV.BOND | 33.3  |2014-11-28 |2014-12-31 |33    |105.84      |105.82     |-0.01  |
|REAL.ESTATE      | 33.3  |2014-11-28 |2014-12-31 |33    | 79.48      | 81.00     | 0.64  |
|US.STOCKS        | 33.3  |2014-12-31 |2015-01-30 |30    |106.00      |103.10     |-0.91  |
|US.10YR.GOV.BOND | 33.3  |2014-12-31 |2015-01-30 |30    |105.82      |110.37     | 1.43  |
|REAL.ESTATE      | 33.3  |2014-12-31 |2015-01-30 |30    | 81.00      | 86.55     | 2.28  |
|US.STOCKS        | 33.3  |2015-01-30 |2015-02-06 | 7    |103.10      |106.33     | 1.04  |
|US.10YR.GOV.BOND | 33.3  |2015-01-30 |2015-02-06 | 7    |110.37      |107.86     |-0.76  |
|REAL.ESTATE      | 33.3  |2015-01-30 |2015-02-06 | 7    | 86.55      | 85.20     |-0.52  |
    




#Signals for strategy :
    




|           | strategy| FOREIGN.STOCKS| EMERGING>MARKETS| US.10YR.GOV.BOND| REAL.ESTATE| COMMODITIES| CASH|
|:----------|--------:|--------------:|----------------:|----------------:|-----------:|-----------:|----:|
|2013-06-27 |       33|              0|                0|                0|           0|           0|   67|
|2013-07-30 |       50|              0|                0|                0|           0|           0|   50|
|2013-08-29 |       33|              0|                0|                0|           0|          33|   33|
|2013-09-27 |       33|             33|                0|                0|           0|           0|   33|
|2013-10-30 |       50|              0|                0|               50|           0|           0|    0|
|2013-11-27 |       33|             33|                0|               33|           0|           0|    0|
|2013-12-30 |       33|             33|                0|               33|           0|           0|    0|
|2014-01-30 |       33|              0|                0|               33|          33|           0|    0|
|2014-02-27 |       50|              0|                0|                0|          50|           0|    0|
|2014-03-28 |       33|              0|                0|                0|          33|          33|    0|
|2014-04-29 |        0|              0|                0|               33|          33|          33|    0|
|2014-05-29 |        0|              0|               50|                0|          50|           0|    0|
|2014-06-27 |        0|              0|               50|                0|          50|           0|    0|
|2014-07-30 |        0|              0|               33|               33|          33|           0|    0|
|2014-08-28 |       33|              0|               33|               33|           0|           0|    0|
|2014-09-29 |       50|              0|                0|               50|           0|           0|    0|
|2014-10-30 |       33|              0|                0|               33|          33|           0|    0|
|2014-11-26 |       33|              0|                0|               33|          33|           0|    0|
|2014-12-30 |       33|              0|                0|               33|          33|           0|    0|
|2015-01-29 |       33|              0|                0|               33|          33|           0|    0|
    







For your convenience, the 
[Strategy-FAA](/public/images/Strategy-FAA/Strategy-FAA.pdf)
report can also be downloaded and viewed the pdf format.





















*(this report was produced on: 2015-02-09)*
