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
|2015-08-25 |     96.95|          43.11|            31.74|           107.24|       72.47|        14.5| 81.75|
    




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
    




|              |strategy          |
|:-------------|:-----------------|
|Period        |May1996 - Aug2015 |
|Cagr          |11.66             |
|Sharpe        |1.18              |
|DVR           |1.09              |
|R2            |0.92              |
|Volatility    |9.83              |
|MaxDD         |-16.18            |
|Exposure      |99.71             |
|Win.Percent   |63.71             |
|Avg.Trade     |0.37              |
|Profit.Factor |2.09              |
|Num.Trades    |631               |
    


![plot of chunk plot-6](/public/images/Strategy-FAA/plot-6-2.png) 

#Monthly Results for strategy :
    




|     |Jan   |Feb   |Mar   |Apr   |May   |Jun   |Jul   |Aug   |Sep   |Oct   |Nov   |Dec   |Year  |MaxDD |
|:----|:-----|:-----|:-----|:-----|:-----|:-----|:-----|:-----|:-----|:-----|:-----|:-----|:-----|:-----|
|1996 |      |      |      |      |      |  1.3 |  0.2 | -0.2 |  1.6 |  1.5 |  4.0 |  4.0 | 12.9 | -1.8 |
|1997 |  0.2 | -0.7 | -2.5 | -1.3 |  5.9 |  1.5 |  2.3 | -7.4 |  5.3 | -0.1 | -1.0 |  1.4 |  2.9 | -7.4 |
|1998 |  0.2 |  2.2 |  3.2 |  1.5 | -0.6 |  0.1 | -0.7 | -3.2 |  2.5 | -0.6 |  0.1 |  3.3 |  8.1 | -5.8 |
|1999 | -1.7 | -2.9 |  2.2 |  2.6 | -3.2 |  4.3 | -1.0 |  1.1 |  2.4 |  0.3 |  4.2 |  9.1 | 18.0 | -6.7 |
|2000 | -3.5 |  4.8 |  0.6 | -2.6 |  3.3 |  2.7 |  1.7 |  2.4 |  1.0 | -1.5 |  3.0 |  0.9 | 13.0 | -5.6 |
|2001 |  0.7 | -0.4 | -1.3 |  0.2 |  1.0 |  2.3 |  0.9 | -0.5 | -0.2 |  0.2 | -1.7 |  0.1 |  1.2 | -4.8 |
|2002 |  1.3 |  1.8 |  3.0 |  0.2 |  0.5 |  2.5 | -0.9 |  2.9 |  2.8 | -0.9 | -0.5 |  1.6 | 15.1 | -4.7 |
|2003 |  2.5 |  0.9 | -2.1 |  0.6 |  6.1 |  0.9 |  0.2 |  4.1 |  0.2 |  3.5 |  2.9 |  5.3 | 27.9 | -4.4 |
|2004 |  2.7 |  2.3 |  2.5 | -6.6 |  1.7 | -1.3 |  2.0 |  1.2 |  0.0 |  2.5 |  3.9 |  3.0 | 14.2 | -7.5 |
|2005 | -0.8 |  1.4 | -4.1 | -2.3 |  0.5 |  2.6 |  1.3 |  0.4 |  1.3 | -4.0 |  2.9 |  4.1 |  2.9 | -8.5 |
|2006 |  7.0 | -2.7 |  3.6 |  2.5 | -6.3 | -0.8 |  2.2 |  0.6 |  1.3 |  3.3 |  3.0 |  0.8 | 14.7 |-16.2 |
|2007 |  3.3 | -3.0 |  0.3 |  2.8 |  1.9 | -0.6 | -0.2 |  0.0 |  7.0 |  7.1 | -1.4 |  1.8 | 20.0 | -9.0 |
|2008 | -0.9 |  6.1 |  0.2 |  1.0 |  1.6 | -0.1 | -3.3 | -1.7 | -0.4 | -2.3 |  5.2 |  5.1 | 10.7 |-11.9 |
|2009 | -2.7 | -0.7 |  1.8 |  6.4 |  5.7 | -2.4 |  6.1 |  0.8 |  4.4 | -2.7 |  5.3 |  0.5 | 24.1 | -9.3 |
|2010 | -6.7 |  4.3 |  5.1 |  3.4 | -3.5 | -2.7 |  3.7 |  2.1 |  3.8 |  3.7 | -1.8 |  4.4 | 16.2 |-10.1 |
|2011 |  1.8 |  4.2 |  0.5 |  4.4 | -1.7 | -2.7 |  2.3 |  0.2 | -4.0 | -0.4 |  0.1 |  2.6 |  7.2 | -7.3 |
|2012 |  4.1 |  1.5 | -0.2 |  1.5 | -2.7 | -0.1 |  1.5 | -0.1 |  0.5 | -1.2 |  1.2 |  1.7 |  7.8 | -4.6 |
|2013 |  1.4 |  0.1 |  2.3 |  3.1 | -2.3 | -1.7 |  2.1 | -2.0 |  0.5 |  2.8 |  0.9 |  0.7 |  7.9 | -8.5 |
|2014 | -2.0 |  3.4 |  0.5 |  1.4 |  0.9 |  1.7 |  0.7 |  2.5 | -3.7 |  2.1 |  1.9 |  0.6 | 10.3 | -5.3 |
|2015 |  2.8 | -0.2 |  0.4 | -2.0 | -1.1 | -1.9 |  0.5 | -3.6 |      |      |      |      | -5.1 | -9.3 |
|Avg  |  0.5 |  1.2 |  0.8 |  0.9 |  0.4 |  0.3 |  1.1 |  0.0 |  1.4 |  0.7 |  1.7 |  2.7 | 11.5 | -7.4 |
    


![plot of chunk plot-6](/public/images/Strategy-FAA/plot-6-3.png) ![plot of chunk plot-6](/public/images/Strategy-FAA/plot-6-4.png) 

#Trades for strategy :
    




|strategy         |weight |entry.date |exit.date  |nhold |entry.price |exit.price |return |
|:----------------|:------|:----------|:----------|:-----|:-----------|:----------|:------|
|REAL.ESTATE      | 33.3  |2014-12-31 |2015-01-30 |30    | 79.71      | 85.17     | 2.28  |
|US.STOCKS        | 33.3  |2015-01-30 |2015-02-27 |28    |102.18      |108.05     | 1.91  |
|US.10YR.GOV.BOND | 33.3  |2015-01-30 |2015-02-27 |28    |109.34      |106.63     |-0.82  |
|REAL.ESTATE      | 33.3  |2015-01-30 |2015-02-27 |28    | 85.17      | 82.05     |-1.22  |
|US.STOCKS        | 33.3  |2015-02-27 |2015-03-31 |32    |108.05      |106.79     |-0.39  |
|US.10YR.GOV.BOND | 33.3  |2015-02-27 |2015-03-31 |32    |106.63      |107.55     | 0.29  |
|REAL.ESTATE      | 33.3  |2015-02-27 |2015-03-31 |32    | 82.05      | 83.47     | 0.58  |
|US.STOCKS        | 33.3  |2015-03-31 |2015-04-30 |30    |106.79      |107.45     | 0.21  |
|US.10YR.GOV.BOND | 33.3  |2015-03-31 |2015-04-30 |30    |107.55      |106.87     |-0.21  |
|REAL.ESTATE      | 33.3  |2015-03-31 |2015-04-30 |30    | 83.47      | 78.59     |-1.95  |
|US.STOCKS        | 33.3  |2015-04-30 |2015-05-29 |29    |107.45      |108.84     | 0.43  |
|EMERGING>MARKETS | 33.3  |2015-04-30 |2015-05-29 |29    | 42.56      | 40.82     |-1.37  |
|US.10YR.GOV.BOND | 33.3  |2015-04-30 |2015-05-29 |29    |106.87      |106.42     |-0.14  |
|US.STOCKS        | 33.3  |2015-05-29 |2015-06-30 |32    |108.84      |107.02     |-0.56  |
|FOREIGN.STOCKS   | 33.3  |2015-05-29 |2015-06-30 |32    | 49.95      | 48.55     |-0.94  |
|CASH             | 33.3  |2015-05-29 |2015-06-30 |32    | 81.84      | 80.93     |-0.37  |
|FOREIGN.STOCKS   | 33.3  |2015-06-30 |2015-07-31 |31    | 48.55      | 48.41     |-0.10  |
|CASH             | 66.7  |2015-06-30 |2015-07-31 |31    | 80.93      | 81.64     | 0.59  |
|US.STOCKS        | 33.3  |2015-07-31 |2015-08-25 |25    |108.84      | 96.95     |-3.64  |
|CASH             | 66.7  |2015-07-31 |2015-08-25 |25    | 81.64      | 81.75     | 0.09  |
    




#Signals for strategy :
    




|           | US.STOCKS| FOREIGN.STOCKS| EMERGING>MARKETS| US.10YR.GOV.BOND| REAL.ESTATE| COMMODITIES| CASH|
|:----------|---------:|--------------:|----------------:|----------------:|-----------:|-----------:|----:|
|2013-12-30 |        33|             33|                0|               33|           0|           0|    0|
|2014-01-30 |        33|              0|                0|               33|          33|           0|    0|
|2014-02-27 |        50|              0|                0|                0|          50|           0|    0|
|2014-03-28 |        33|              0|                0|                0|          33|          33|    0|
|2014-04-29 |         0|              0|                0|               33|          33|          33|    0|
|2014-05-29 |         0|              0|               50|                0|          50|           0|    0|
|2014-06-27 |         0|              0|               50|                0|          50|           0|    0|
|2014-07-30 |         0|              0|               33|               33|          33|           0|    0|
|2014-08-28 |        33|              0|               33|               33|           0|           0|    0|
|2014-09-29 |        50|              0|                0|               50|           0|           0|    0|
|2014-10-30 |        33|              0|                0|               33|          33|           0|    0|
|2014-11-26 |        33|              0|                0|               33|          33|           0|    0|
|2014-12-30 |        33|              0|                0|               33|          33|           0|    0|
|2015-01-29 |        33|              0|                0|               33|          33|           0|    0|
|2015-02-26 |        33|              0|                0|               33|          33|           0|    0|
|2015-03-30 |        33|              0|                0|               33|          33|           0|    0|
|2015-04-29 |        33|              0|               33|               33|           0|           0|    0|
|2015-05-28 |        33|             33|                0|                0|           0|           0|   33|
|2015-06-29 |         0|             33|                0|                0|           0|           0|   67|
|2015-07-30 |        33|              0|                0|                0|           0|           0|   67|
    







For your convenience, the 
[Strategy-FAA](/public/images/Strategy-FAA/Strategy-FAA.pdf)
report can also be downloaded and viewed the pdf format.





















*(this report was produced on: 2015-08-26)*
