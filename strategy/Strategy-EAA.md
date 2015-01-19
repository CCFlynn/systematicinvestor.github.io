---
layout: page
title: Elastic Asset Allocation Strategy
---


To install [Systematic Investor Toolbox (SIT)](https://github.com/systematicinvestor/SIT) please visit [About](/about) page.





The [Elastic Asset Allocation Strategy(EAA)](http://papers.ssrn.com/sol3/papers.cfm?abstract_id=2543979)
backtest and live signal. For more details please see [SSRN paper](http://papers.ssrn.com/sol3/papers.cfm?abstract_id=2543979)

The [EAA Strategy](http://papers.ssrn.com/sol3/papers.cfm?abstract_id=2543979)
 uses a geometrical weighted average of the historical returns, volatilities
and correlations, using elasticities as weights:

> asset's attractivness = [return ^ wR] * [(1-correlation) ^ wC] / [volatility ^ wV] 

where:
---

* return is average asset's return over last year using different timeframes
* correlation is asset's correlation with equal weight, market, index
* volatility is asset's historical volatility
* wR, wC, wV are user elasticities





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
EMERGING.MARKETS=EEM + VEIEX
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



|           | US.STOCKS| FOREIGN.STOCKS| EMERGING.MARKETS| US.10YR.GOV.BOND| REAL.ESTATE| COMMODITIES|  CASH|
|:----------|---------:|--------------:|----------------:|----------------:|-----------:|-----------:|-----:|
|2015-01-16 |    104.02|           46.8|            39.49|           109.07|       87.34|       17.55| 83.69|
    




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
vol.lookback = 12*22
cor.lookback = 12*22

weight=list(r=1, v=0, c=0.5, s=2)

hist.vol = sqrt(252) * bt.apply.matrix(ret, runSD, n = vol.lookback)

#mean of cumulative returns of 1, 3, 6, and 12 month periods
mom = (prices / mlag(prices, 22)-1 + prices / mlag(prices, 3*22)-1
+ prices / mlag(prices, 6*22)-1 + prices / mlag(prices, 12 * 22)-1)/22

mkt.ret = rowMeans(ret, na.rm=T)

#*****************************************************************
# Compute Correlation to Market
#******************************************************************
mkt.cor = data$weight * NA
for(i in period.ends[period.ends > cor.lookback]){
  index = (i - cor.lookback):i
  hist = ret[index,]
    include.index = !is.na(colSums(hist))

  mkt.cor[i,include.index] = cor(hist[,include.index], mkt.ret[index], use='complete.obs',method='pearson')
}

avg.rank = (mom^weight$r * (1 - mkt.cor)^weight$c / hist.vol^weight$v) ^ weight$s
meta.rank = br.rank(avg.rank[period.ends,])

#absolute momentum filter
weight = (meta.rank <= n.top)/rowSums(meta.rank <= n.top, na.rm=T) * (mom[period.ends,] > 0)

# otherwise, it's weight is allocated to cash
weight$CASH = 1 - rowSums(weight,na.rm=T)

obj$weights$strategy = weight
{% endhighlight %}


![plot of chunk plot-6](/public/images/Strategy-EAA/plot-6-1.png) 

#Strategy Performance:
    




|           |strategy          |
|:----------|:-----------------|
|Period     |Jul1996 - Jan2015 |
|Cagr       |12.73             |
|Sharpe     |1.15              |
|DVR        |1.1               |
|Volatility |10.98             |
|MaxDD      |-14.13            |
|AvgDD      |-1.76             |
|VaR        |-1.06             |
|CVaR       |-1.65             |
|Exposure   |99.98             |
    


![plot of chunk plot-6](/public/images/Strategy-EAA/plot-6-2.png) 

#Monthly Results for strategy :
    




|     |Jan   |Feb   |Mar   |Apr   |May   |Jun   |Jul   |Aug   |Sep   |Oct   |Nov   |Dec   |Year  |MaxDD |
|:----|:-----|:-----|:-----|:-----|:-----|:-----|:-----|:-----|:-----|:-----|:-----|:-----|:-----|:-----|
|1996 |      |      |      |      |      |      |      | -0.2 |  1.7 |  2.2 |  1.9 | -0.9 |  4.6 | -1.9 |
|1997 |  0.2 |  0.2 | -1.2 |  1.6 |  0.9 |  1.1 |  2.7 | -3.5 |  7.3 | -3.8 | -0.9 |  1.7 |  6.2 | -8.0 |
|1998 |  0.1 |  2.3 |  3.3 |  1.5 | -0.5 |  1.2 | -0.6 | -4.0 |  2.5 | -0.6 |  0.3 |  2.4 |  8.0 | -6.2 |
|1999 |  1.8 | -2.9 |  1.7 |  6.8 | -3.2 |  3.8 | -1.7 |  1.5 |  1.9 |  1.6 |  7.1 | 10.5 | 31.7 | -5.9 |
|2000 | -2.0 |  4.8 | -0.2 | -4.9 |  2.6 |  3.6 |  1.0 |  2.5 | -0.9 | -1.4 |  3.1 |  0.9 |  9.0 |-11.2 |
|2001 |  0.7 | -0.4 | -1.3 | -0.6 |  1.0 |  2.3 |  1.0 |  1.9 | -0.2 |  2.0 | -1.6 |  0.2 |  4.8 | -4.2 |
|2002 |  1.1 |  1.4 |  0.8 |  0.3 |  0.1 | -1.2 | -0.8 |  2.9 |  2.9 | -0.8 | -0.5 |  4.1 | 10.6 | -6.8 |
|2003 |  2.6 |  3.0 | -2.8 | -0.3 |  4.6 |  0.6 |  0.6 |  4.3 |  0.5 |  5.4 |  1.2 |  6.9 | 29.6 | -5.3 |
|2004 |  3.1 |  4.3 |  2.5 | -8.3 |  1.9 | -0.3 |  0.4 |  2.8 |  2.1 |  3.2 |  4.7 |  1.5 | 18.7 |-12.5 |
|2005 | -3.4 |  6.6 | -1.9 | -3.1 |  1.8 |  3.0 |  6.4 |  1.1 |  3.8 | -4.6 |  3.2 |  3.8 | 17.0 | -7.7 |
|2006 |  8.9 | -3.5 |  3.6 |  2.5 | -4.7 |  1.1 |  2.8 |  0.6 | -0.8 |  3.4 |  3.5 |  2.1 | 20.5 |-11.8 |
|2007 |  3.3 | -2.2 | -0.5 |  1.4 |  0.9 | -1.6 | -1.3 |  0.8 |  4.1 |  8.9 | -1.4 |  1.8 | 14.5 |-12.2 |
|2008 |  2.5 |  4.2 |  0.3 |  1.1 |  2.8 |  0.8 | -3.0 | -1.4 | -3.6 | -2.2 |  3.9 |  5.1 | 10.3 |-13.7 |
|2009 | -2.7 | -0.6 |  1.1 | -0.6 | -0.2 | -1.2 |  4.6 |  0.9 |  7.5 | -2.5 |  5.7 |  2.1 | 14.2 | -7.5 |
|2010 | -7.0 |  2.9 |  8.2 |  1.7 | -7.5 | -0.2 |  3.8 | -0.4 |  5.4 |  3.1 | -1.1 |  3.7 | 11.9 |-14.1 |
|2011 |  3.0 |  4.2 |  0.5 |  4.4 | -1.6 | -2.7 |  3.1 | -0.5 | -3.9 | -0.4 | -1.2 |  1.4 |  6.1 |-11.0 |
|2012 |  2.6 |  0.7 |  0.6 |  1.6 | -2.6 | -0.1 |  1.5 |  0.0 |  0.1 | -1.0 |  0.5 |  1.4 |  5.2 | -4.9 |
|2013 |  2.0 |  0.4 |  2.5 |  3.3 | -3.7 | -2.3 |  2.2 | -3.6 |  2.0 |  2.8 |  0.9 |  1.2 |  7.5 | -9.3 |
|2014 | -2.5 |  1.9 |  0.5 |  1.6 |  1.0 |  1.8 | -1.1 |  3.3 | -5.3 |  1.7 |  1.4 |  0.5 |  4.6 | -6.7 |
|2015 |  4.1 |      |      |      |      |      |      |      |      |      |      |      |  4.1 | -0.1 |
|Avg  |  1.0 |  1.5 |  1.0 |  0.6 | -0.4 |  0.5 |  1.2 |  0.5 |  1.4 |  0.9 |  1.6 |  2.6 | 12.0 | -8.0 |
    


![plot of chunk plot-6](/public/images/Strategy-EAA/plot-6-3.png) 

#Trades for strategy :
    




|symbol           |weight |entry.date |exit.date  |entry.price |exit.price |return |
|:----------------|:------|:----------|:----------|:-----------|:----------|:------|
|FOREIGN.STOCKS   | 33.3  |2014-06-30 |2014-07-31 | 51.60      | 50.84     |-0.49  |
|REAL.ESTATE      | 33.3  |2014-06-30 |2014-07-31 | 73.15      | 73.21     | 0.03  |
|US.STOCKS        | 33.3  |2014-07-31 |2014-08-29 | 98.77      |102.87     | 1.38  |
|EMERGING.MARKETS | 33.3  |2014-07-31 |2014-08-29 | 43.20      | 44.42     | 0.94  |
|REAL.ESTATE      | 33.3  |2014-07-31 |2014-08-29 | 73.21      | 75.44     | 1.02  |
|US.STOCKS        | 33.3  |2014-08-29 |2014-09-30 |102.87      |100.71     |-0.70  |
|EMERGING.MARKETS | 33.3  |2014-08-29 |2014-09-30 | 44.42      | 40.97     |-2.59  |
|REAL.ESTATE      | 33.3  |2014-08-29 |2014-09-30 | 75.44      | 70.88     |-2.01  |
|US.STOCKS        | 33.3  |2014-09-30 |2014-10-31 |100.71      |103.47     | 0.91  |
|US.10YR.GOV.BOND | 33.3  |2014-09-30 |2014-10-31 |103.08      |104.66     | 0.51  |
|CASH             | 33.3  |2014-09-30 |2014-10-31 | 81.43      | 82.02     | 0.24  |
|US.10YR.GOV.BOND | 33.3  |2014-10-31 |2014-11-28 |104.66      |106.01     | 0.43  |
|REAL.ESTATE      | 33.3  |2014-10-31 |2014-11-28 | 77.92      | 79.48     | 0.67  |
|CASH             | 33.3  |2014-10-31 |2014-11-28 | 82.02      | 82.70     | 0.28  |
|US.STOCKS        | 33.3  |2014-11-28 |2014-12-31 |106.04      |106.00     |-0.01  |
|REAL.ESTATE      | 33.3  |2014-11-28 |2014-12-31 | 79.48      | 81.00     | 0.64  |
|CASH             | 33.3  |2014-11-28 |2014-12-31 | 82.70      | 82.37     |-0.13  |
|US.10YR.GOV.BOND | 33.3  |2014-12-31 |2015-01-16 |105.99      |109.07     | 0.97  |
|REAL.ESTATE      | 33.3  |2014-12-31 |2015-01-16 | 81.00      | 87.34     | 2.61  |
|CASH             | 33.3  |2014-12-31 |2015-01-16 | 82.37      | 83.69     | 0.53  |
    




#Signals for strategy :
    




|           | US.STOCKS| FOREIGN.STOCKS| EMERGING.MARKETS| US.10YR.GOV.BOND| REAL.ESTATE| COMMODITIES| CASH|
|:----------|---------:|--------------:|----------------:|----------------:|-----------:|-----------:|----:|
|2013-05-30 |        33|             33|                0|                0|          33|           0|    0|
|2013-06-27 |        33|              0|                0|                0|           0|           0|   67|
|2013-07-30 |        33|              0|                0|                0|          33|           0|   33|
|2013-08-29 |        33|              0|                0|                0|           0|           0|   67|
|2013-09-27 |        33|             33|                0|                0|           0|           0|   33|
|2013-10-30 |        33|             33|               33|                0|           0|           0|    0|
|2013-11-27 |        33|             33|                0|                0|           0|           0|   33|
|2013-12-30 |        33|             33|                0|                0|           0|           0|   33|
|2014-01-30 |        33|              0|                0|                0|           0|           0|   67|
|2014-02-27 |        33|             33|                0|                0|          33|           0|    0|
|2014-03-28 |        33|             33|                0|                0|          33|           0|    0|
|2014-04-29 |        33|              0|                0|                0|          33|          33|    0|
|2014-05-29 |        33|             33|                0|                0|          33|           0|    0|
|2014-06-27 |        33|             33|                0|                0|          33|           0|    0|
|2014-07-30 |        33|              0|               33|                0|          33|           0|    0|
|2014-08-28 |        33|              0|               33|                0|          33|           0|    0|
|2014-09-29 |        33|              0|                0|               33|           0|           0|   33|
|2014-10-30 |         0|              0|                0|               33|          33|           0|   33|
|2014-11-26 |        33|              0|                0|                0|          33|           0|   33|
|2014-12-30 |         0|              0|                0|               33|          33|           0|   33|
    







For your convenience, the 
[Strategy-EAA](/public/images/Strategy-EAA/Strategy-EAA.pdf)
report can also be downloaded and viewed the pdf format.



*(this report was produced on: 2015-01-19)*
