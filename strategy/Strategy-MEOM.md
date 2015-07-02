---
layout: page
title: Monthly End-of-the-Month Strategy
---


To install [Systematic Investor Toolbox (SIT)](https://github.com/systematicinvestor/SIT) please visit [About](/about) page.





The [Monthly End-of-the-Month Strategy (MEOM) by Quanting Dutchman](http://quantingdutchman.wordpress.com/2010/06/30/strategy-2-monthly-end-of-the-month-meom/)
backtest and live signal.

The [MEOM Strategy](http://quantingdutchman.wordpress.com/2010/06/30/strategy-2-monthly-end-of-the-month-meom/)
invests into the top 2 ETFs that are trading above a medium term moving avarage (WMA89) 
from the universe of 26 ETFs: 


* [DIA](http://finance.yahoo.com/q/hl?s=DIA)
* [EEM](http://finance.yahoo.com/q/hl?s=EEM)
* [EFA](http://finance.yahoo.com/q/hl?s=EFA)
* [EWH](http://finance.yahoo.com/q/hl?s=EWH)
* [EWJ](http://finance.yahoo.com/q/hl?s=EWJ)
* [EWT](http://finance.yahoo.com/q/hl?s=EWT)
* [EWZ](http://finance.yahoo.com/q/hl?s=EWZ)
* [FXI](http://finance.yahoo.com/q/hl?s=FXI)
* [GLD](http://finance.yahoo.com/q/hl?s=GLD)
* [GSG](http://finance.yahoo.com/q/hl?s=GSG)
* [IEF](http://finance.yahoo.com/q/hl?s=IEF)
* [ILF](http://finance.yahoo.com/q/hl?s=ILF)
* [IWM](http://finance.yahoo.com/q/hl?s=IWM)
* [IYR](http://finance.yahoo.com/q/hl?s=IYR)
* [QQQ](http://finance.yahoo.com/q/hl?s=QQQ)
* [SPY](http://finance.yahoo.com/q/hl?s=SPY)
* [VNQ](http://finance.yahoo.com/q/hl?s=VNQ)
* [XLB](http://finance.yahoo.com/q/hl?s=XLB)
* [XLE](http://finance.yahoo.com/q/hl?s=XLE)
* [XLF](http://finance.yahoo.com/q/hl?s=XLF)
* [XLI](http://finance.yahoo.com/q/hl?s=XLI)
* [XLP](http://finance.yahoo.com/q/hl?s=XLP)
* [XLU](http://finance.yahoo.com/q/hl?s=XLU)
* [XLV](http://finance.yahoo.com/q/hl?s=XLV)
* [XLY](http://finance.yahoo.com/q/hl?s=XLY)
* [XLK](http://finance.yahoo.com/q/hl?s=XLK)

The best asset is selected using following ranking criteria:

> Rank = MA( C/Ref(C,-2), 5 ) * Ref( MA( C/Ref(C,-2), 10 ), -5 )


Following report is based on Monthly re-balancing.
Signal is generated one day prior to the month end,
and execution is done at close at the month end.

The strategy enters positions in the top 2 ranked ETFs
on the last day of the month at the close, and
exits positions two days later at the close.





Load historical data from Yahoo Finance:



{% highlight r %}
#*****************************************************************
# Load historical data
#*****************************************************************
library(SIT)
load.packages('quantmod')

tickers = 'DIA,EEM,EFA,EWH,EWJ,EWT,EWZ,FXI,GLD,GSG,IEF,ILF,IWM,IYR,QQQ,SPY,VNQ,XLB,XLE,XLF,XLI,XLP,XLU,XLV,XLY,XLK'

data <- new.env()

getSymbols.extra(tickers, src = 'yahoo', from = '1970-01-01', env = data, auto.assign = T, set.symbolnames = T, getSymbols.fn = getSymbols.fn, calendar=calendar)
  for(i in data$symbolnames) data[[i]] = adjustOHLC(data[[i]], use.Adjusted=T)
bt.prep(data, align='keep.all', dates='1995::', fill.gaps=T)

print(last(data$prices))
{% endhighlight %}



|           |   DIA|   EEM|  EFA|   EWH|  EWJ|   EWT|   EWZ|   FXI|    GLD|   GSG|   IEF|   ILF|    IWM|   IYR|    QQQ|    SPY|   VNQ|   XLB|  XLE|   XLF|   XLI|   XLP|   XLU|   XLV|   XLY|   XLK|
|:----------|-----:|-----:|----:|-----:|----:|-----:|-----:|-----:|------:|-----:|-----:|-----:|------:|-----:|------:|------:|-----:|-----:|----:|-----:|-----:|-----:|-----:|-----:|-----:|-----:|
|2015-07-01 | 177.4| 39.62| 63.9| 22.59| 12.9| 16.01| 32.26| 45.73| 111.98| 20.77| 104.3| 29.37| 125.23| 72.39| 107.92| 207.57| 75.92| 48.65| 74.2| 24.75| 54.33| 48.21| 41.71| 75.19| 77.35| 41.68|
    




{% highlight r %}
#*****************************************************************
# Setup
#*****************************************************************
data$universe = data$prices > 0
prices = data$prices * data$universe
	n = ncol(prices)
  nperiods = nrow(prices)
{% endhighlight %}





Code Strategy Rules:



{% highlight r %}
#*****************************************************************
# Code Strategy
#******************************************************************
# BuyRule = C > WMA(C, 89)
buy.rule = prices > bt.apply.matrix(prices, function(x) { WMA(x, 89) } )		
  buy.rule = ifna(buy.rule, F)

# 2-day returns
ret2 = ifna(prices / mlag(prices, 2), 0)

# Rank2 = MA( C/Ref(C,-2), 5 ) * Ref( MA( C/Ref(C,-2), 10 ), -5 )
position.score = bt.apply.matrix(ret2, SMA, 5) * mlag( bt.apply.matrix(ret2, SMA, 10), 5)
	position.score[!buy.rule] = NA

period.ends2 = iif(period.ends + 2 > nperiods, nperiods, period.ends + 2)

weight = ntop(position.score[period.ends,], 2)

models = list()

data$weight[] = NA
	data$weight[period.ends,] = 	weight
	data$weight[period.ends2,] = 0		
models$strategy = bt.run.share(data, clean.signal=F, trade.summary=T, silent=T, do.lag = 1 + out$delayLen)		
{% endhighlight %}


![plot of chunk plot-7](/public/images/Strategy-MEOM/plot-7-1.png) 

#Strategy Performance:
    




|              |strategy          |
|:-------------|:-----------------|
|Period        |Apr1996 - Jul2015 |
|Cagr          |6.19              |
|Sharpe        |0.92              |
|DVR           |0.86              |
|R2            |0.94              |
|Volatility    |6.83              |
|MaxDD         |-10.43            |
|Exposure      |8.61              |
|Win.Percent   |61.31             |
|Avg.Trade     |0.3               |
|Profit.Factor |2.01              |
|Num.Trades    |398               |
    


![plot of chunk plot-7](/public/images/Strategy-MEOM/plot-7-2.png) 

#Monthly Results for strategy :
    




|     |Jan   |Feb   |Mar   |Apr   |May   |Jun   |Jul   |Aug   |Sep   |Oct   |Nov   |Dec   |Year  |MaxDD |
|:----|:-----|:-----|:-----|:-----|:-----|:-----|:-----|:-----|:-----|:-----|:-----|:-----|:-----|:-----|
|1996 |      |      |      |      |  0.0 |  0.0 |  0.0 |  0.0 |  0.0 |  1.8 |  0.0 | -0.8 |  1.0 | -0.8 |
|1997 |  0.4 |  0.0 |  0.0 |  0.0 |  0.0 |  1.6 |  0.7 |  0.0 |  0.0 |  0.0 |  0.0 |  0.0 |  2.7 | -2.2 |
|1998 |  0.0 |  0.0 |  1.7 | -4.0 |  0.0 |  0.1 |  0.7 | -4.1 |  0.0 |  0.0 |  2.2 | -1.7 | -5.1 |-10.4 |
|1999 | -0.1 | -0.7 |  0.4 |  2.5 |  1.8 |  0.3 |  1.0 |  0.0 | -0.1 |  2.3 | -0.9 |  4.9 | 11.9 | -0.9 |
|2000 | -3.3 |  2.4 | -1.5 |  0.0 | -1.2 |  2.2 |  0.0 |  1.9 | -0.6 | -0.4 | -1.2 | -0.2 | -2.2 | -4.8 |
|2001 | -0.4 | -4.1 |  0.8 |  0.0 |  1.0 |  0.1 |  1.0 |  1.0 |  1.5 |  0.0 |  2.2 |  3.1 |  6.1 | -5.0 |
|2002 |  3.5 | -2.4 |  4.2 | -1.2 | -1.5 | -0.4 | -1.7 |  0.0 | -1.0 |  0.0 |  1.8 | -1.5 | -0.3 | -7.1 |
|2003 |  3.9 |  0.3 | -0.2 |  5.5 |  0.9 |  1.2 |  2.0 | -1.0 |  3.1 |  2.9 |  1.0 |  2.1 | 23.8 | -1.0 |
|2004 |  7.4 | -0.2 |  0.3 |  2.4 |  0.7 | -0.2 | -2.0 | -0.3 |  2.1 |  4.3 | -0.5 |  1.0 | 15.7 | -2.7 |
|2005 | -5.6 |  1.4 | -1.2 | -0.4 |  0.2 |  2.4 | -0.7 |  1.6 |  0.9 | -1.7 |  1.0 |  1.9 | -0.6 | -6.3 |
|2006 |  3.7 | -3.1 |  2.3 |  0.0 |  2.0 | -0.4 | -0.3 |  0.2 |  1.2 |  0.0 |  1.2 |  1.5 |  8.4 | -3.1 |
|2007 | -1.3 |  0.6 | -0.8 |  1.3 |  1.1 |  2.6 |  2.8 | -0.6 |  0.9 |  4.1 | -3.4 | -0.3 |  6.9 | -4.5 |
|2008 |  3.7 | -1.7 |  0.6 |  5.3 |  2.3 | -3.4 |  1.9 | -0.4 |  1.3 | -1.0 |  0.0 | -1.1 |  7.4 | -4.0 |
|2009 | -1.3 |  0.1 | -2.8 |  7.4 |  4.8 |  3.4 | -1.9 |  7.1 | -4.4 | -2.8 |  1.9 |  2.3 | 13.6 | -7.7 |
|2010 |  1.6 |  1.2 |  1.0 |  2.1 |  0.8 |  0.2 | -1.4 |  2.4 | -0.5 |  1.5 |  1.2 |  3.2 | 14.1 | -4.0 |
|2011 |  0.7 |  0.4 |  2.5 |  1.7 |  0.0 |  0.1 |  1.4 |  1.7 | -2.0 | -1.2 | -0.8 |  0.0 |  4.3 | -6.0 |
|2012 | -1.6 |  2.2 | -0.1 |  0.6 |  0.8 |  0.1 |  1.0 | -1.0 |  0.2 |  0.9 |  1.1 |  0.2 |  4.4 | -1.6 |
|2013 |  2.1 | -0.1 |  1.2 |  1.4 | -0.5 | -0.4 |  0.2 |  1.6 |  0.2 |  0.6 |  0.3 | -1.3 |  5.3 | -1.8 |
|2014 | -1.4 | -0.6 |  1.7 |  2.5 | -0.9 |  0.4 | -0.6 |  0.8 |  1.0 | -0.4 | -0.4 | -0.4 |  1.7 | -2.3 |
|2015 | -1.3 |  1.9 |  0.3 |  2.0 |  0.3 | -0.1 |  1.0 |      |      |      |      |      |  4.1 | -1.3 |
|Avg  |  0.6 | -0.1 |  0.5 |  1.5 |  0.6 |  0.5 |  0.3 |  0.6 |  0.2 |  0.6 |  0.3 |  0.7 |  6.2 | -3.9 |
    


![plot of chunk plot-7](/public/images/Strategy-MEOM/plot-7-3.png) ![plot of chunk plot-7](/public/images/Strategy-MEOM/plot-7-4.png) 

#Trades for strategy :
    




|strategy |weight |entry.date |exit.date  |nhold |entry.price |exit.price |return |
|:--------|:------|:----------|:----------|:-----|:-----------|:----------|:------|
|ILF      | 50    |2014-08-29 |2014-09-03 |5     | 41.70      | 42.27     | 0.68  |
|IEF      | 50    |2014-09-30 |2014-10-02 |2     |101.95      |102.50     | 0.27  |
|XLV      | 50    |2014-09-30 |2014-10-02 |2     | 63.28      | 62.49     |-0.63  |
|IWM      | 50    |2014-10-31 |2014-11-04 |4     |115.77      |115.04     |-0.32  |
|XLI      | 50    |2014-10-31 |2014-11-04 |4     | 54.38      | 54.28     |-0.09  |
|XLB      | 50    |2014-11-28 |2014-12-02 |4     | 48.45      | 48.23     |-0.22  |
|XLI      | 50    |2014-11-28 |2014-12-02 |4     | 56.06      | 55.81     |-0.22  |
|IWM      | 50    |2014-12-31 |2015-01-05 |5     |119.26      |116.98     |-0.95  |
|XLU      | 50    |2014-12-31 |2015-01-05 |5     | 46.43      | 46.07     |-0.38  |
|EWH      | 50    |2015-01-30 |2015-02-03 |4     | 21.39      | 21.56     | 0.39  |
|EWT      | 50    |2015-01-30 |2015-02-03 |4     | 15.23      | 15.68     | 1.48  |
|EWJ      | 50    |2015-02-27 |2015-03-03 |4     | 12.29      | 12.26     |-0.12  |
|XLY      | 50    |2015-02-27 |2015-03-03 |4     | 75.48      | 76.17     | 0.46  |
|EWH      | 50    |2015-03-31 |2015-04-02 |2     | 21.65      | 21.90     | 0.59  |
|FXI      | 50    |2015-03-31 |2015-04-02 |2     | 44.17      | 45.43     | 1.42  |
|EWT      | 50    |2015-04-30 |2015-05-04 |4     | 16.44      | 16.57     | 0.40  |
|EWZ      | 50    |2015-04-30 |2015-05-04 |4     | 35.56      | 35.48     |-0.11  |
|QQQ      | 50    |2015-05-29 |2015-06-02 |4     |109.80      |109.82     | 0.01  |
|XLV      | 50    |2015-05-29 |2015-06-02 |4     | 74.68      | 74.49     |-0.13  |
|XLV      |100    |2015-06-30 |2015-07-01 |1     | 74.42      | 75.19     | 1.03  |
    




#Signals for strategy :
    




|           | DIA| EEM| EFA| EWH| EWJ| EWT| EWZ| FXI| GLD| GSG| IEF| ILF| IWM| IYR| QQQ| SPY| VNQ| XLB| XLE| XLF| XLI| XLP| XLU| XLV| XLY| XLK|
|:----------|---:|---:|---:|---:|---:|---:|---:|---:|---:|---:|---:|---:|---:|---:|---:|---:|---:|---:|---:|---:|---:|---:|---:|---:|---:|---:|
|2013-11-27 |   0|   0|   0|   0|   0|   0|   0|   0|   0|   0|   0|   0|  50|   0|   0|   0|   0|   0|   0|  50|   0|   0|   0|   0|   0|   0|
|2013-12-30 |   0|   0|  50|   0|   0|   0|   0|   0|   0|   0|   0|   0|   0|   0|   0|   0|   0|  50|   0|   0|   0|   0|   0|   0|   0|   0|
|2014-01-30 |   0|   0|   0|   0|   0|   0|   0|   0|   0|   0|  50|   0|   0|   0|   0|   0|   0|   0|   0|   0|   0|   0|  50|   0|   0|   0|
|2014-02-27 |   0|   0|   0|   0|   0|   0|   0|   0|   0|   0|   0|   0|  50|   0|   0|   0|   0|  50|   0|   0|   0|   0|   0|   0|   0|   0|
|2014-03-28 |   0|   0|   0|   0|   0|   0|  50|   0|   0|   0|   0|  50|   0|   0|   0|   0|   0|   0|   0|   0|   0|   0|   0|   0|   0|   0|
|2014-04-29 |   0|   0|   0|   0|   0|   0|   0|   0|   0|   0|   0|   0|   0|   0|   0|   0|   0|   0|   0|   0|   0|  50|  50|   0|   0|   0|
|2014-05-29 |   0|   0|   0|   0|  50|   0|   0|   0|   0|   0|   0|   0|   0|   0|  50|   0|   0|   0|   0|   0|   0|   0|   0|   0|   0|   0|
|2014-06-27 |   0|   0|   0|   0|   0|   0|  50|   0|  50|   0|   0|   0|   0|   0|   0|   0|   0|   0|   0|   0|   0|   0|   0|   0|   0|   0|
|2014-07-30 |   0|   0|   0|  50|   0|   0|   0|   0|   0|   0|   0|  50|   0|   0|   0|   0|   0|   0|   0|   0|   0|   0|   0|   0|   0|   0|
|2014-08-28 |   0|   0|   0|   0|   0|   0|  50|   0|   0|   0|   0|  50|   0|   0|   0|   0|   0|   0|   0|   0|   0|   0|   0|   0|   0|   0|
|2014-09-29 |   0|   0|   0|   0|   0|   0|   0|   0|   0|   0|  50|   0|   0|   0|   0|   0|   0|   0|   0|   0|   0|   0|   0|  50|   0|   0|
|2014-10-30 |   0|   0|   0|   0|   0|   0|   0|   0|   0|   0|   0|   0|  50|   0|   0|   0|   0|   0|   0|   0|  50|   0|   0|   0|   0|   0|
|2014-11-26 |   0|   0|   0|   0|   0|   0|   0|   0|   0|   0|   0|   0|   0|   0|   0|   0|   0|  50|   0|   0|  50|   0|   0|   0|   0|   0|
|2014-12-30 |   0|   0|   0|   0|   0|   0|   0|   0|   0|   0|   0|   0|  50|   0|   0|   0|   0|   0|   0|   0|   0|   0|  50|   0|   0|   0|
|2015-01-29 |   0|   0|   0|  50|   0|  50|   0|   0|   0|   0|   0|   0|   0|   0|   0|   0|   0|   0|   0|   0|   0|   0|   0|   0|   0|   0|
|2015-02-26 |   0|   0|   0|   0|  50|   0|   0|   0|   0|   0|   0|   0|   0|   0|   0|   0|   0|   0|   0|   0|   0|   0|   0|   0|  50|   0|
|2015-03-30 |   0|   0|   0|  50|   0|   0|   0|  50|   0|   0|   0|   0|   0|   0|   0|   0|   0|   0|   0|   0|   0|   0|   0|   0|   0|   0|
|2015-04-29 |   0|   0|   0|   0|   0|  50|  50|   0|   0|   0|   0|   0|   0|   0|   0|   0|   0|   0|   0|   0|   0|   0|   0|   0|   0|   0|
|2015-05-28 |   0|   0|   0|   0|   0|   0|   0|   0|   0|   0|   0|   0|   0|   0|  50|   0|   0|   0|   0|   0|   0|   0|   0|  50|   0|   0|
|2015-06-29 |   0|   0|   0|   0|   0|   0|   0|   0|   0|   0|   0|   0|   0|   0|   0|   0|   0|   0|   0|   0|   0|   0|   0| 100|   0|   0|
    






For your convenience, the 
[Strategy-MEOM](/public/images/Strategy-MEOM/Strategy-MEOM.pdf)
report can also be downloaded and viewed the pdf format.


For more details please check the
[MEOM strategy code at bt.meom.test() function in bt.test.r at github](https://github.com/systematicinvestor/SIT/blob/master/R/bt.test.r)



*(this report was produced on: 2015-07-02)*
