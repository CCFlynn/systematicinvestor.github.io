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
|2015-01-14 |    103.71|             46|           108.95|       86.39|       17.46| 83.57|
    




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
    




|           |strategy          |
|:----------|:-----------------|
|Period     |Jul1996 - Jan2015 |
|Cagr       |10.72             |
|Sharpe     |1.29              |
|DVR        |1.25              |
|Volatility |8.16              |
|MaxDD      |-13.34            |
|AvgDD      |-1.14             |
|VaR        |-0.75             |
|CVaR       |-1.19             |
|Exposure   |99.98             |
    


![plot of chunk plot-6](/public/images/Strategy-TAA/plot-6-2.png) 

#Monthly Results for strategy :
    




|     |Jan   |Feb   |Mar   |Apr   |May   |Jun   |Jul   |Aug   |Sep   |Oct   |Nov   |Dec   |Year  |MaxDD |
|:----|:-----|:-----|:-----|:-----|:-----|:-----|:-----|:-----|:-----|:-----|:-----|:-----|:-----|:-----|
|1996 |      |      |      |      |      |      |      | -0.2 |  1.7 |  2.2 |  1.9 | -0.9 |  4.6 | -1.9 |
|1997 |  0.2 |  0.2 | -1.2 |  1.6 |  4.1 |  2.2 |  3.8 | -2.1 |  5.1 | -1.7 | -0.1 |  1.6 | 14.3 | -5.0 |
|1998 |  1.1 |  2.1 |  2.5 |  0.3 |  0.1 |  1.1 | -0.1 | -4.7 |  2.3 | -0.6 |  0.4 |  2.0 |  6.6 | -7.0 |
|1999 |  1.5 | -2.7 |  2.0 |  2.6 | -1.8 |  3.1 | -0.6 |  1.2 |  1.2 |  0.4 |  3.3 |  4.8 | 15.9 | -3.5 |
|2000 | -1.2 |  3.2 |  1.9 | -1.2 |  0.9 |  3.5 |  0.3 |  2.2 | -1.1 | -1.2 |  2.5 |  2.3 | 12.7 | -4.1 |
|2001 |  1.1 |  0.1 | -0.5 |  0.0 |  0.9 |  1.6 |  1.5 |  1.6 |  0.2 |  1.0 | -1.5 | -0.2 |  5.8 | -3.4 |
|2002 |  0.2 |  1.2 | -0.2 |  0.0 |  0.8 |  0.8 | -2.4 |  2.1 |  1.3 | -0.7 | -0.3 |  3.4 |  6.3 | -5.2 |
|2003 |  1.6 |  2.4 | -1.7 |  0.8 |  5.4 |  1.3 |  1.2 |  2.4 |  1.4 |  2.9 |  1.7 |  4.5 | 26.4 | -3.9 |
|2004 |  2.5 |  3.1 |  1.8 | -5.0 |  1.0 |  0.3 | -0.4 |  2.8 |  1.4 |  2.5 |  2.7 |  1.9 | 15.2 | -7.0 |
|2005 | -1.9 |  3.0 | -0.5 | -0.6 |  1.2 |  2.1 |  3.4 |  1.2 |  1.0 | -2.9 |  2.5 |  2.4 | 11.4 | -4.3 |
|2006 |  4.5 | -0.8 |  2.5 |  1.7 | -2.2 |  0.7 |  1.6 |  1.4 |  0.2 |  2.9 |  2.6 | -0.3 | 15.5 | -7.4 |
|2007 |  1.8 | -0.3 |  0.5 |  1.8 |  1.0 | -2.0 | -0.2 |  0.6 |  4.0 |  3.7 | -0.8 |  1.3 | 12.0 | -5.5 |
|2008 |  0.2 |  2.5 |  0.3 |  0.5 |  0.6 | -1.5 | -1.8 | -0.5 | -2.3 | -2.5 |  4.7 |  5.1 |  5.1 |-12.9 |
|2009 | -2.4 | -0.6 |  1.5 | -0.2 |  0.1 |  0.1 |  4.6 |  3.6 |  3.4 | -0.7 |  4.6 |  1.4 | 16.2 | -4.9 |
|2010 | -4.3 |  3.0 |  4.4 |  2.5 | -6.3 | -1.0 |  2.6 |  1.3 |  0.9 |  3.4 | -1.4 |  5.2 | 10.1 | -9.8 |
|2011 |  2.0 |  3.1 |  0.2 |  4.0 | -1.4 | -2.2 |  0.9 | -3.2 | -2.1 | -0.2 | -0.7 |  1.3 |  1.6 |-13.3 |
|2012 |  2.7 |  0.4 |  0.8 |  0.3 | -6.1 |  1.9 |  1.3 |  1.7 |  0.7 | -1.3 |  0.7 |  1.6 |  4.5 | -7.6 |
|2013 |  2.6 | -0.6 |  1.5 |  2.9 | -2.4 | -2.1 |  1.6 | -2.7 |  3.0 |  2.0 | -0.6 |  0.5 |  5.5 | -8.1 |
|2014 | -0.9 |  2.4 |  0.2 |  1.3 |  1.4 |  1.5 | -1.7 |  2.2 | -2.9 |  3.1 |  1.5 |  0.2 |  8.5 | -3.1 |
|2015 |  2.0 |      |      |      |      |      |      |      |      |      |      |      |  2.0 |  0.0 |
|Avg  |  0.7 |  1.2 |  0.9 |  0.8 | -0.2 |  0.6 |  0.9 |  0.6 |  1.0 |  0.6 |  1.2 |  2.0 | 10.0 | -5.9 |
    


![plot of chunk plot-6](/public/images/Strategy-TAA/plot-6-3.png) 

#Trades for strategy :
    




|symbol           |weight |entry.date |exit.date  |entry.price |exit.price |return |
|:----------------|:------|:----------|:----------|:-----------|:----------|:------|
|FOREIGN.STOCKS   | 20    |2014-08-29 |2014-09-30 | 51.35      | 48.85     |-0.97  |
|US.10YR.GOV.BOND | 20    |2014-08-29 |2014-09-30 |104.17      |103.08     |-0.21  |
|REAL.ESTATE      | 20    |2014-08-29 |2014-09-30 | 75.44      | 70.88     |-1.21  |
|CASH             | 20    |2014-08-29 |2014-09-30 | 81.90      | 81.43     |-0.11  |
|US.STOCKS        | 20    |2014-09-30 |2014-10-31 |100.71      |103.47     | 0.55  |
|US.10YR.GOV.BOND | 20    |2014-09-30 |2014-10-31 |103.08      |104.66     | 0.31  |
|REAL.ESTATE      | 20    |2014-09-30 |2014-10-31 | 70.88      | 77.92     | 1.99  |
|CASH             | 40    |2014-09-30 |2014-10-31 | 81.43      | 82.02     | 0.29  |
|US.STOCKS        | 20    |2014-10-31 |2014-11-28 |103.47      |106.04     | 0.50  |
|US.10YR.GOV.BOND | 20    |2014-10-31 |2014-11-28 |104.66      |106.01     | 0.26  |
|REAL.ESTATE      | 20    |2014-10-31 |2014-11-28 | 77.92      | 79.48     | 0.40  |
|CASH             | 40    |2014-10-31 |2014-11-28 | 82.02      | 82.70     | 0.33  |
|US.STOCKS        | 20    |2014-11-28 |2014-12-31 |106.04      |106.00     |-0.01  |
|US.10YR.GOV.BOND | 20    |2014-11-28 |2014-12-31 |106.01      |105.99     | 0.00  |
|REAL.ESTATE      | 20    |2014-11-28 |2014-12-31 | 79.48      | 81.00     | 0.38  |
|CASH             | 40    |2014-11-28 |2014-12-31 | 82.70      | 82.37     |-0.16  |
|US.STOCKS        | 20    |2014-12-31 |2015-01-14 |106.00      |103.71     |-0.43  |
|US.10YR.GOV.BOND | 20    |2014-12-31 |2015-01-14 |105.99      |108.95     | 0.56  |
|REAL.ESTATE      | 20    |2014-12-31 |2015-01-14 | 81.00      | 86.39     | 1.33  |
|CASH             | 40    |2014-12-31 |2015-01-14 | 82.37      | 83.57     | 0.58  |
    




#Signals for strategy :
    




|           | US.STOCKS| FOREIGN.STOCKS| US.10YR.GOV.BOND| REAL.ESTATE| COMMODITIES| CASH|
|:----------|---------:|--------------:|----------------:|-----------:|-----------:|----:|
|2013-05-30 |        20|             20|                0|          20|           0|   40|
|2013-06-27 |        20|              0|                0|          20|           0|   60|
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
    






```

For your convenience, the 
[Strategy-TAA](/public/images/Strategy-TAA/Strategy-TAA.pdf)
report can also be downloaded and viewed the pdf format.





















*(this report was produced on: 2015-01-15)*
