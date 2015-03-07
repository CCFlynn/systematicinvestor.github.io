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
|2015-03-06 |    107.46|          48.47|            39.26|           105.49|       80.37|       17.57| 82.12|
    




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
    




|              |strategy          |
|:-------------|:-----------------|
|Period        |Jun1996 - Mar2015 |
|Cagr          |11.68             |
|Sharpe        |1.07              |
|DVR           |1.02              |
|R2            |0.96              |
|Volatility    |10.93             |
|MaxDD         |-14.17            |
|Exposure      |99.51             |
|Win.Percent   |63.17             |
|Avg.Trade     |0.37              |
|Profit.Factor |2.04              |
|Num.Trades    |619               |
    


![plot of chunk plot-6](/public/images/Strategy-EAA/plot-6-2.png) 

#Monthly Results for strategy :
    




|     |Jan   |Feb   |Mar   |Apr   |May   |Jun   |Jul   |Aug   |Sep   |Oct   |Nov   |Dec   |Year  |MaxDD |
|:----|:-----|:-----|:-----|:-----|:-----|:-----|:-----|:-----|:-----|:-----|:-----|:-----|:-----|:-----|
|1996 |      |      |      |      |      |      |  0.0 | -0.3 |  1.7 |  2.1 |  1.9 | -0.9 |  4.5 | -1.9 |
|1997 |  0.2 |  0.2 | -1.2 |  1.6 |  0.9 |  1.1 |  2.6 | -3.6 |  7.3 | -3.9 | -1.0 |  1.7 |  5.7 | -8.0 |
|1998 |  0.1 |  2.3 |  3.3 |  1.5 | -0.6 |  1.2 | -0.7 | -4.0 |  2.5 | -0.7 |  0.2 |  2.3 |  7.4 | -6.2 |
|1999 |  1.8 | -3.0 |  1.6 |  6.7 | -3.3 |  3.7 | -1.7 |  1.4 |  1.9 |  1.5 |  7.0 | 10.5 | 30.7 | -5.8 |
|2000 | -2.1 |  4.8 | -0.2 | -5.0 |  2.6 |  3.5 |  1.0 |  2.4 | -0.9 | -1.5 |  3.0 |  0.8 |  8.4 |-11.2 |
|2001 |  0.6 | -0.5 | -1.4 | -0.6 |  1.0 |  2.2 |  0.9 |  1.8 | -0.3 |  1.9 | -1.7 |  0.1 |  4.2 | -4.3 |
|2002 |  1.0 |  1.3 |  0.7 |  0.3 |  0.1 | -1.3 | -0.9 |  2.9 |  2.8 | -0.9 | -0.5 |  4.1 |  9.9 | -7.0 |
|2003 |  2.6 |  3.0 | -2.8 | -0.4 |  4.6 |  0.5 |  0.6 |  4.3 |  0.4 |  5.3 |  1.1 |  6.9 | 28.8 | -5.3 |
|2004 |  3.1 |  4.3 |  2.5 | -8.4 |  1.9 | -0.4 |  0.2 |  2.7 |  2.1 |  3.2 |  4.7 |  1.4 | 18.0 |-12.6 |
|2005 | -3.5 |  6.6 | -2.0 | -3.1 |  1.7 |  3.0 |  6.3 |  1.0 |  3.8 | -4.7 |  3.1 |  3.8 | 16.3 | -7.8 |
|2006 |  8.9 | -3.6 |  3.6 |  2.4 | -4.8 |  1.0 |  2.8 |  0.5 | -0.8 |  3.3 |  3.4 |  2.0 | 19.7 |-11.9 |
|2007 |  3.2 | -2.3 | -0.6 |  1.4 |  0.9 | -1.6 | -1.4 |  0.7 |  4.0 |  8.9 | -1.4 |  1.7 | 13.8 |-12.2 |
|2008 |  2.5 |  4.1 |  0.2 |  1.0 |  2.7 |  0.7 | -3.0 | -1.4 | -3.7 | -2.3 |  3.9 |  5.1 |  9.9 |-13.8 |
|2009 | -2.7 | -0.7 |  1.1 | -0.6 | -0.3 | -1.3 |  4.5 |  0.8 |  7.4 | -2.6 |  5.7 |  2.0 | 13.5 | -7.7 |
|2010 | -7.1 |  2.8 |  8.2 |  1.7 | -7.6 | -0.3 |  3.7 | -0.4 |  5.4 |  3.0 | -1.1 |  3.6 | 11.3 |-14.2 |
|2011 |  2.9 |  4.2 |  0.5 |  4.4 | -1.7 | -2.7 |  3.1 | -0.5 | -4.0 | -0.4 | -1.3 |  1.4 |  5.6 |-11.2 |
|2012 |  2.6 |  0.6 |  0.6 |  1.5 | -2.7 | -0.1 |  1.5 |  0.0 |  0.1 | -1.1 |  0.5 |  1.3 |  4.7 | -5.0 |
|2013 |  1.9 |  0.3 |  2.4 |  3.2 | -3.7 | -2.4 |  2.1 | -3.7 |  2.0 |  2.8 |  0.9 |  1.2 |  6.9 | -9.4 |
|2014 | -2.5 |  1.9 |  0.5 |  1.5 |  1.0 |  1.8 | -1.2 |  3.3 | -5.4 |  1.6 |  1.3 |  0.4 |  4.0 | -6.7 |
|2015 |  4.5 | -2.5 | -2.0 |      |      |      |      |      |      |      |      |      | -0.2 | -4.8 |
|Avg  |  0.9 |  1.3 |  0.8 |  0.5 | -0.4 |  0.5 |  1.1 |  0.4 |  1.4 |  0.8 |  1.6 |  2.6 | 11.2 | -8.4 |
    


![plot of chunk plot-6](/public/images/Strategy-EAA/plot-6-3.png) ![plot of chunk plot-6](/public/images/Strategy-EAA/plot-6-4.png) 

#Trades for strategy :
    




|strategy         |weight |entry.date |exit.date  |nhold |entry.price |exit.price |return |
|:----------------|:------|:----------|:----------|:-----|:-----------|:----------|:------|
|EMERGING.MARKETS | 33.3  |2014-08-29 |2014-09-30 |32    | 44.42      | 40.97     |-2.59  |
|REAL.ESTATE      | 33.3  |2014-08-29 |2014-09-30 |32    | 75.44      | 70.88     |-2.01  |
|US.STOCKS        | 33.3  |2014-09-30 |2014-10-31 |31    |100.71      |103.47     | 0.91  |
|US.10YR.GOV.BOND | 33.3  |2014-09-30 |2014-10-31 |31    |102.75      |104.32     | 0.51  |
|CASH             | 33.3  |2014-09-30 |2014-10-31 |31    | 81.11      | 81.70     | 0.24  |
|US.10YR.GOV.BOND | 33.3  |2014-10-31 |2014-11-28 |28    |104.32      |105.67     | 0.43  |
|REAL.ESTATE      | 33.3  |2014-10-31 |2014-11-28 |28    | 77.93      | 79.49     | 0.67  |
|CASH             | 33.3  |2014-10-31 |2014-11-28 |28    | 81.70      | 82.37     | 0.28  |
|US.STOCKS        | 33.3  |2014-11-28 |2014-12-31 |33    |106.04      |106.00     |-0.01  |
|REAL.ESTATE      | 33.3  |2014-11-28 |2014-12-31 |33    | 79.49      | 81.00     | 0.63  |
|CASH             | 33.3  |2014-11-28 |2014-12-31 |33    | 82.37      | 82.05     |-0.13  |
|US.10YR.GOV.BOND | 33.3  |2014-12-31 |2015-01-30 |30    |105.65      |110.20     | 1.43  |
|REAL.ESTATE      | 33.3  |2014-12-31 |2015-01-30 |30    | 81.00      | 86.55     | 2.28  |
|CASH             | 33.3  |2014-12-31 |2015-01-30 |30    | 82.05      | 84.02     | 0.80  |
|US.10YR.GOV.BOND | 33.3  |2015-01-30 |2015-02-27 |28    |110.20      |107.47     |-0.82  |
|REAL.ESTATE      | 33.3  |2015-01-30 |2015-02-27 |28    | 86.55      | 83.37     |-1.22  |
|CASH             | 33.3  |2015-01-30 |2015-02-27 |28    | 84.02      | 82.92     |-0.44  |
|US.STOCKS        | 33.3  |2015-02-27 |2015-03-06 | 7    |109.02      |107.46     |-0.48  |
|REAL.ESTATE      | 33.3  |2015-02-27 |2015-03-06 | 7    | 83.37      | 80.37     |-1.20  |
|CASH             | 33.3  |2015-02-27 |2015-03-06 | 7    | 82.92      | 82.12     |-0.32  |
    




#Signals for strategy :
    




|           | US.STOCKS| FOREIGN.STOCKS| EMERGING.MARKETS| US.10YR.GOV.BOND| REAL.ESTATE| COMMODITIES| CASH|
|:----------|---------:|--------------:|----------------:|----------------:|-----------:|-----------:|----:|
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
|2015-01-29 |         0|              0|                0|               33|          33|           0|   33|
|2015-02-26 |        33|              0|                0|                0|          33|           0|   33|
    







For your convenience, the 
[Strategy-EAA](/public/images/Strategy-EAA/Strategy-EAA.pdf)
report can also be downloaded and viewed the pdf format.



*(this report was produced on: 2015-03-07)*
