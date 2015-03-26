---
layout: page
title: Quantitative Approach To Tactical Asset Allocation Strategy
---


To install [Systematic Investor Toolbox (SIT)](https://github.com/systematicinvestor/SIT) please visit [About](/about) page.





The [Quantitative Approach To Tactical Asset Allocation Strategy(QATAA) by Mebane T. Faber](http://mebfaber.com/timing-model/)
backtest and live signal. For more details please see [SSRN paper](http://papers.ssrn.com/sol3/papers.cfm?abstract_id=962461)

The [QATAA Strategy](http://mebfaber.com/timing-model/)
allocates 20% across 5 asset classes:

* US Stocks
* Foreign Stocks
* US 10YR Government Bonds
* Real Estate
* Commodities

If asset is above it's 10 month moving average it gets 20% allocation; otherwise, it's weight is
allocated to cash. The re-balancing process is done Monthly.

Following report is based on Monthly re-balancing, 
signal is generated one day before the month end,
and execution is done at close at the month end.





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



|           | US.STOCKS| FOREIGN.STOCKS| US.10YR.GOV.BOND| REAL.ESTATE| COMMODITIES|  CASH|
|:----------|---------:|--------------:|----------------:|-----------:|-----------:|-----:|
|2015-03-25 |    106.79|          49.34|           108.31|       84.19|       17.32| 83.37|
    




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
sma = bt.apply.matrix(prices, SMA, 200)

# If asset is above it's 10 month moving average it gets 20% allocation
weight = iif(prices > sma, 20/100, 0)

# otherwise, it's weight is allocated to cash
weight$CASH = 1 - rowSums(weight)

obj$weights$strategy = weight[period.ends,]
{% endhighlight %}


![plot of chunk plot-6](/public/images/Strategy-TAA/plot-6-1.png) 

#Strategy Performance:
    




|              |strategy          |
|:-------------|:-----------------|
|Period        |Jun1996 - Mar2015 |
|Cagr          |9.89              |
|Sharpe        |1.21              |
|DVR           |1.17              |
|R2            |0.97              |
|Volatility    |8.13              |
|MaxDD         |-13.48            |
|Exposure      |99.51             |
|Win.Percent   |65.15             |
|Avg.Trade     |0.21              |
|Profit.Factor |2.08              |
|Num.Trades    |924               |
    


![plot of chunk plot-6](/public/images/Strategy-TAA/plot-6-2.png) 

#Monthly Results for strategy :
    




|     |Jan   |Feb   |Mar   |Apr   |May   |Jun   |Jul   |Aug   |Sep   |Oct   |Nov   |Dec   |Year  |MaxDD |
|:----|:-----|:-----|:-----|:-----|:-----|:-----|:-----|:-----|:-----|:-----|:-----|:-----|:-----|:-----|
|1996 |      |      |      |      |      |      |  0.0 | -0.3 |  1.7 |  2.1 |  1.9 | -0.9 |  4.5 | -1.9 |
|1997 |  0.2 |  0.2 | -1.2 |  1.5 |  4.0 |  2.2 |  3.8 | -2.2 |  5.0 | -1.8 | -0.1 |  1.5 | 13.6 | -5.0 |
|1998 |  1.0 |  2.1 |  2.4 |  0.3 |  0.0 |  1.1 | -0.2 | -4.7 |  2.3 | -0.6 |  0.3 |  2.0 |  6.0 | -7.1 |
|1999 |  1.5 | -2.8 |  1.9 |  2.5 | -1.9 |  3.1 | -0.6 |  1.2 |  1.2 |  0.4 |  3.2 |  4.8 | 15.1 | -3.6 |
|2000 | -1.2 |  3.1 |  1.8 | -1.2 |  0.9 |  3.4 |  0.3 |  2.1 | -1.2 | -1.2 |  2.5 |  2.2 | 11.9 | -4.2 |
|2001 |  1.1 |  0.1 | -0.6 |  0.0 |  0.9 |  1.5 |  1.4 |  1.6 |  0.2 |  0.9 | -1.5 | -0.3 |  5.3 | -3.4 |
|2002 |  0.1 |  1.2 | -0.3 | -0.1 |  0.8 |  0.8 | -2.5 |  2.1 |  1.2 | -0.8 | -0.4 |  3.4 |  5.6 | -5.2 |
|2003 |  1.6 |  2.3 | -1.8 |  0.7 |  5.4 |  1.2 |  1.1 |  2.3 |  1.3 |  2.9 |  1.7 |  4.4 | 25.6 | -3.9 |
|2004 |  2.4 |  3.1 |  1.7 | -5.1 |  1.0 |  0.3 | -0.4 |  2.8 |  1.3 |  2.5 |  2.6 |  1.8 | 14.5 | -7.1 |
|2005 | -2.0 |  3.0 | -0.6 | -0.7 |  1.1 |  2.1 |  3.4 |  1.2 |  1.0 | -3.0 |  2.5 |  2.4 | 10.6 | -4.4 |
|2006 |  4.4 | -0.9 |  2.5 |  1.7 | -2.3 |  0.7 |  1.5 |  1.3 |  0.2 |  2.8 |  2.5 | -0.4 | 14.8 | -7.4 |
|2007 |  1.7 | -0.4 |  0.5 |  1.8 |  1.0 | -2.0 | -0.2 |  0.6 |  4.0 |  3.7 | -0.9 |  1.2 | 11.3 | -5.6 |
|2008 |  0.2 |  2.5 |  0.3 |  0.5 |  0.5 | -1.5 | -1.8 | -0.6 | -2.4 | -2.5 |  4.7 |  5.1 |  4.7 |-13.1 |
|2009 | -2.5 | -0.6 |  1.5 | -0.2 |  0.1 |  0.0 |  4.5 |  3.5 |  3.4 | -0.8 |  4.5 |  1.4 | 15.6 | -4.9 |
|2010 | -4.4 |  2.9 |  4.3 |  2.4 | -6.3 | -1.1 |  2.6 |  1.3 |  0.8 |  3.3 | -1.4 |  5.1 |  9.4 | -9.8 |
|2011 |  1.9 |  3.1 |  0.2 |  4.0 | -1.4 | -2.2 |  0.9 | -3.3 | -2.1 | -0.2 | -0.8 |  1.3 |  0.9 |-13.5 |
|2012 |  2.7 |  0.3 |  0.7 |  0.3 | -6.2 |  1.8 |  1.3 |  1.6 |  0.6 | -1.3 |  0.6 |  1.5 |  3.8 | -7.7 |
|2013 |  2.5 | -0.7 |  1.5 |  2.9 | -2.4 | -2.1 |  1.5 | -2.8 |  2.9 |  2.0 | -0.6 |  0.4 |  4.9 | -8.2 |
|2014 | -0.9 |  2.3 |  0.1 |  1.3 |  1.3 |  1.4 | -1.7 |  2.2 | -3.0 |  3.1 |  1.4 |  0.2 |  7.8 | -3.1 |
|2015 |  2.6 | -0.7 |  0.2 |      |      |      |      |      |      |      |      |      |  2.2 | -3.4 |
|Avg  |  0.7 |  1.1 |  0.8 |  0.7 | -0.2 |  0.6 |  0.8 |  0.5 |  1.0 |  0.6 |  1.2 |  2.0 |  9.4 | -6.1 |
    


![plot of chunk plot-6](/public/images/Strategy-TAA/plot-6-3.png) ![plot of chunk plot-6](/public/images/Strategy-TAA/plot-6-4.png) 

#Trades for strategy :
    




|strategy         |weight |entry.date |exit.date  |nhold |entry.price |exit.price |return |
|:----------------|:------|:----------|:----------|:-----|:-----------|:----------|:------|
|US.10YR.GOV.BOND | 20    |2014-10-31 |2014-11-28 |28    |104.32      |105.67     | 0.26  |
|REAL.ESTATE      | 20    |2014-10-31 |2014-11-28 |28    | 77.47      | 79.02     | 0.40  |
|CASH             | 40    |2014-10-31 |2014-11-28 |28    | 81.70      | 82.37     | 0.33  |
|US.STOCKS        | 20    |2014-11-28 |2014-12-31 |33    |105.55      |105.51     |-0.01  |
|US.10YR.GOV.BOND | 20    |2014-11-28 |2014-12-31 |33    |105.67      |105.65     | 0.00  |
|REAL.ESTATE      | 20    |2014-11-28 |2014-12-31 |33    | 79.02      | 80.52     | 0.38  |
|CASH             | 40    |2014-11-28 |2014-12-31 |33    | 82.37      | 82.05     |-0.16  |
|US.STOCKS        | 20    |2014-12-31 |2015-01-30 |30    |105.51      |102.62     |-0.55  |
|US.10YR.GOV.BOND | 20    |2014-12-31 |2015-01-30 |30    |105.65      |110.20     | 0.86  |
|REAL.ESTATE      | 20    |2014-12-31 |2015-01-30 |30    | 80.52      | 86.04     | 1.37  |
|CASH             | 40    |2014-12-31 |2015-01-30 |30    | 82.05      | 84.02     | 0.96  |
|US.STOCKS        | 20    |2015-01-30 |2015-02-27 |28    |102.62      |108.51     | 1.15  |
|US.10YR.GOV.BOND | 20    |2015-01-30 |2015-02-27 |28    |110.20      |107.47     |-0.49  |
|REAL.ESTATE      | 20    |2015-01-30 |2015-02-27 |28    | 86.04      | 82.88     |-0.73  |
|CASH             | 40    |2015-01-30 |2015-02-27 |28    | 84.02      | 82.92     |-0.52  |
|US.STOCKS        | 20    |2015-02-27 |2015-03-25 |26    |108.51      |106.79     |-0.32  |
|FOREIGN.STOCKS   | 20    |2015-02-27 |2015-03-25 |26    | 49.43      | 49.34     |-0.04  |
|US.10YR.GOV.BOND | 20    |2015-02-27 |2015-03-25 |26    |107.47      |108.31     | 0.16  |
|REAL.ESTATE      | 20    |2015-02-27 |2015-03-25 |26    | 82.88      | 84.19     | 0.32  |
|CASH             | 20    |2015-02-27 |2015-03-25 |26    | 82.92      | 83.37     | 0.11  |
    




#Signals for strategy :
    




|           | US.STOCKS| FOREIGN.STOCKS| US.10YR.GOV.BOND| REAL.ESTATE| COMMODITIES| CASH|
|:----------|---------:|--------------:|----------------:|-----------:|-----------:|----:|
|2013-07-30 |        20|             20|                0|          20|           0|   40|
|2013-08-29 |        20|             20|                0|           0|           0|   60|
|2013-09-27 |        20|             20|                0|           0|           0|   60|
|2013-10-30 |        20|             20|                0|          20|           0|   40|
|2013-11-27 |        20|             20|                0|           0|           0|   60|
|2013-12-30 |        20|             20|                0|           0|           0|   60|
|2014-01-30 |        20|             20|               20|           0|           0|   40|
|2014-02-27 |        20|             20|               20|          20|          20|    0|
|2014-03-28 |        20|             20|               20|          20|          20|    0|
|2014-04-29 |        20|             20|               20|          20|          20|    0|
|2014-05-29 |        20|             20|               20|          20|          20|    0|
|2014-06-27 |        20|             20|               20|          20|          20|    0|
|2014-07-30 |        20|             20|               20|          20|           0|   20|
|2014-08-28 |        20|             20|               20|          20|           0|   20|
|2014-09-29 |        20|              0|               20|          20|           0|   40|
|2014-10-30 |        20|              0|               20|          20|           0|   40|
|2014-11-26 |        20|              0|               20|          20|           0|   40|
|2014-12-30 |        20|              0|               20|          20|           0|   40|
|2015-01-29 |        20|              0|               20|          20|           0|   40|
|2015-02-26 |        20|             20|               20|          20|           0|   20|
    







For your convenience, the 
[Strategy-TAA](/public/images/Strategy-TAA/Strategy-TAA.pdf)
report can also be downloaded and viewed the pdf format.





















*(this report was produced on: 2015-03-26)*
