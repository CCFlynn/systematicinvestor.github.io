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



|           |   DIA|   EEM|   EFA|   EWH|   EWJ|   EWT|   EWZ|   FXI|    GLD|   GSG|    IEF|   ILF|    IWM|   IYR|    QQQ| SPY|   VNQ|   XLB|   XLE|   XLF|   XLI|   XLP|  XLU|   XLV|   XLY|   XLK|
|:----------|-----:|-----:|-----:|-----:|-----:|-----:|-----:|-----:|------:|-----:|------:|-----:|------:|-----:|------:|---:|-----:|-----:|-----:|-----:|-----:|-----:|----:|-----:|-----:|-----:|
|2016-02-19 | 163.7| 30.24| 53.92| 17.81| 10.83| 12.66| 19.81| 30.46| 117.58| 12.81| 110.49| 20.45| 100.45| 69.85| 101.63| 192| 75.43| 40.75| 56.81| 20.98| 51.32| 50.84| 46.2| 65.93| 72.74| 40.66|
    




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
|Period        |Jan1995 - Feb2016 |
|Cagr          |6.95              |
|Sharpe        |1.05              |
|DVR           |1                 |
|R2            |0.95              |
|Volatility    |6.64              |
|MaxDD         |-10.26            |
|Exposure      |8.98              |
|Win.Percent   |63.33             |
|Avg.Trade     |0.33              |
|Profit.Factor |2.16              |
|Num.Trades    |450               |
    


![plot of chunk plot-7](/public/images/Strategy-MEOM/plot-7-2.png) 

#Monthly Results for strategy :
    




|     |Jan  |Feb  |Mar  |Apr  |May  |Jun  |Jul  |Aug  |Sep  |Oct  |Nov  |Dec  |Year |MaxDD |
|:----|:----|:----|:----|:----|:----|:----|:----|:----|:----|:----|:----|:----|:----|:-----|
|1995 |     | 0.0 | 0.0 | 0.0 | 0.0 |-0.2 | 0.7 |-0.4 | 1.4 |-0.4 | 1.4 | 1.4 | 4.0 |-0.5  |
|1996 | 1.3 | 0.0 | 2.2 | 1.4 |-1.5 | 1.0 | 0.5 | 0.0 | 0.7 | 1.5 | 0.2 |-1.2 | 6.1 |-1.7  |
|1997 | 1.0 | 0.9 | 0.1 | 0.0 | 1.6 | 1.7 | 0.4 |-0.1 | 0.0 | 1.9 | 0.0 | 2.0 |10.0 |-1.3  |
|1998 | 0.7 | 2.1 | 1.0 |-1.0 | 0.9 | 0.3 | 1.0 |-4.2 | 0.0 | 0.0 | 2.1 | 0.5 | 3.3 |-7.7  |
|1999 | 1.4 |-0.3 |-0.1 | 2.9 | 1.8 | 0.3 | 1.7 | 0.2 | 0.3 | 2.5 |-0.9 | 4.4 |14.9 |-1.0  |
|2000 |-3.3 | 2.4 |-1.5 | 1.2 |-1.2 | 2.2 |-0.6 | 1.9 |-2.2 |-0.4 |-1.2 |-0.2 |-3.1 |-5.9  |
|2001 |-2.1 |-3.0 | 0.8 | 0.0 |-1.8 | 0.1 | 1.0 | 0.7 | 1.5 | 0.0 | 2.2 | 3.1 | 2.4 |-6.1  |
|2002 | 1.6 |-2.0 | 4.2 |-1.1 | 1.3 |-0.1 |-1.7 | 0.0 |-1.0 | 0.0 | 2.7 |-2.6 | 1.2 |-4.5  |
|2003 | 3.9 | 0.3 | 0.1 | 5.5 | 0.9 | 1.2 | 4.6 |-1.0 | 3.1 | 2.9 | 1.0 | 2.1 |27.3 |-1.0  |
|2004 | 4.9 |-0.2 | 0.7 | 2.2 | 1.3 |-0.2 |-2.4 | 0.5 | 2.1 | 3.2 |-0.2 |-2.4 | 9.6 |-3.2  |
|2005 |-5.6 | 2.1 |-0.9 |-0.4 | 0.2 | 1.5 |-0.7 | 3.0 | 1.6 |-1.5 | 1.0 | 1.9 | 1.9 |-5.8  |
|2006 | 3.7 |-2.9 | 2.8 | 1.1 | 2.0 |-0.4 | 0.2 | 0.2 | 1.2 | 0.1 | 1.1 | 1.1 |10.5 |-2.9  |
|2007 |-0.4 | 0.6 |-0.8 | 0.7 | 1.1 | 2.6 | 3.2 |-0.6 | 0.0 | 4.8 |-4.4 |-0.3 | 6.4 |-5.0  |
|2008 | 2.5 |-1.7 | 0.6 | 5.3 | 1.9 |-3.4 | 1.9 |-0.9 | 1.4 |-1.0 | 0.0 |-1.1 | 5.3 |-4.1  |
|2009 |-2.2 | 0.1 |-2.8 | 6.6 | 4.8 | 3.4 |-2.0 | 7.1 |-4.4 |-2.8 | 1.9 | 2.3 |11.7 |-7.7  |
|2010 | 1.6 | 1.2 | 1.0 | 2.1 | 0.8 | 0.2 |-1.4 | 2.4 |-0.5 | 1.5 | 1.1 | 3.9 |14.7 |-4.0  |
|2011 | 0.7 | 1.0 | 0.2 | 0.7 | 0.0 | 0.1 | 2.1 | 1.7 |-2.0 |-1.2 | 1.4 | 0.0 | 4.7 |-5.3  |
|2012 |-1.6 | 3.1 |-0.1 | 0.6 | 0.8 | 0.1 | 0.6 |-1.0 | 0.2 | 1.0 | 1.9 | 0.2 | 5.7 |-1.6  |
|2013 | 2.0 |-0.2 | 1.3 | 1.4 |-0.5 |-0.4 | 0.5 | 1.4 | 0.2 | 0.6 | 0.5 |-1.6 | 5.1 |-2.0  |
|2014 |-1.4 |-0.6 | 1.1 | 2.5 |-0.8 | 0.7 |-0.6 | 0.9 | 1.0 |-0.4 |-0.4 |-0.6 | 1.4 |-2.6  |
|2015 |-1.3 | 1.9 | 0.3 | 2.0 | 0.3 |-0.1 | 0.6 |-0.2 | 0.2 | 0.6 | 1.5 |-0.1 | 5.6 |-1.3  |
|2016 | 0.6 | 1.2 |     |     |     |     |     |     |     |     |     |     | 1.8 |-0.7  |
|Avg  | 0.4 | 0.3 | 0.5 | 1.6 | 0.7 | 0.5 | 0.5 | 0.6 | 0.2 | 0.6 | 0.6 | 0.6 | 6.8 |-3.5  |
    


![plot of chunk plot-7](/public/images/Strategy-MEOM/plot-7-3.png) ![plot of chunk plot-7](/public/images/Strategy-MEOM/plot-7-4.png) 

#Trades for strategy :
    




|strategy |weight |entry.date |exit.date  |nhold |entry.price |exit.price |return |
|:--------|:------|:----------|:----------|:-----|:-----------|:----------|:------|
|XLY      | 50    |2015-02-27 |2015-03-03 |4     | 74.88      | 75.57     | 0.46  |
|EWH      | 50    |2015-03-31 |2015-04-02 |2     | 21.41      | 21.67     | 0.59  |
|FXI      | 50    |2015-03-31 |2015-04-02 |2     | 43.23      | 44.45     | 1.42  |
|EWT      | 50    |2015-04-30 |2015-05-04 |4     | 15.94      | 16.07     | 0.40  |
|EWZ      | 50    |2015-04-30 |2015-05-04 |4     | 35.17      | 35.09     |-0.11  |
|QQQ      | 50    |2015-05-29 |2015-06-02 |4     |109.20      |109.22     | 0.01  |
|XLV      | 50    |2015-05-29 |2015-06-02 |4     | 74.10      | 73.91     |-0.13  |
|XLV      |100    |2015-06-30 |2015-07-02 |2     | 73.81      | 74.26     | 0.60  |
|QQQ      | 50    |2015-07-31 |2015-08-04 |4     |111.34      |110.78     |-0.25  |
|XLY      | 50    |2015-07-31 |2015-08-04 |4     | 79.59      | 79.66     | 0.04  |
|IEF      |100    |2015-08-31 |2015-09-02 |2     |105.55      |105.74     | 0.18  |
|IEF      |100    |2015-09-30 |2015-10-02 |2     |107.22      |107.82     | 0.56  |
|QQQ      | 50    |2015-10-30 |2015-11-03 |4     |112.98      |114.66     | 0.74  |
|XLK      | 50    |2015-10-30 |2015-11-03 |4     | 43.43      | 44.05     | 0.71  |
|IWM      | 50    |2015-11-30 |2015-12-02 |2     |118.59      |118.09     |-0.21  |
|XLY      | 50    |2015-11-30 |2015-12-02 |2     | 80.45      | 80.57     | 0.07  |
|VNQ      | 50    |2015-12-31 |2016-01-05 |5     | 79.73      | 80.29     | 0.35  |
|XLU      | 50    |2015-12-31 |2016-01-05 |5     | 43.28      | 43.50     | 0.25  |
|GLD      | 50    |2016-01-29 |2016-02-02 |4     |106.95      |108.09     | 0.53  |
|XLU      | 50    |2016-01-29 |2016-02-02 |4     | 45.42      | 46.05     | 0.69  |
    




#Signals for strategy :
    




|           | DIA| EEM| EFA| EWH| EWJ| EWT| EWZ| FXI| GLD| GSG| IEF| ILF| IWM| IYR| QQQ| SPY| VNQ| XLB| XLE| XLF| XLI| XLP| XLU| XLV| XLY| XLK|
|:----------|---:|---:|---:|---:|---:|---:|---:|---:|---:|---:|---:|---:|---:|---:|---:|---:|---:|---:|---:|---:|---:|---:|---:|---:|---:|---:|
|2014-06-27 |   0|   0|   0|   0|   0|   0|  50|   0|  50|   0|   0|   0|   0|   0|   0|   0|   0|   0|   0|   0|   0|   0|   0|   0|   0|   0|
|2014-07-30 |   0|   0|   0|  50|   0|   0|   0|  50|   0|   0|   0|   0|   0|   0|   0|   0|   0|   0|   0|   0|   0|   0|   0|   0|   0|   0|
|2014-08-28 |   0|   0|   0|   0|   0|   0|  50|   0|   0|   0|   0|  50|   0|   0|   0|   0|   0|   0|   0|   0|   0|   0|   0|   0|   0|   0|
|2014-09-29 |   0|   0|   0|   0|   0|   0|   0|   0|   0|   0|  50|   0|   0|   0|   0|   0|   0|   0|   0|   0|   0|   0|   0|  50|   0|   0|
|2014-10-30 |   0|   0|   0|   0|   0|   0|   0|   0|   0|   0|   0|   0|  50|   0|   0|   0|   0|   0|   0|   0|  50|   0|   0|   0|   0|   0|
|2014-11-26 |   0|   0|   0|   0|   0|   0|   0|   0|   0|   0|   0|   0|   0|   0|   0|   0|   0|  50|   0|   0|   0|   0|   0|   0|  50|   0|
|2014-12-30 |   0|   0|   0|   0|   0|   0|   0|   0|   0|   0|   0|   0|  50|   0|   0|   0|   0|   0|   0|   0|   0|   0|  50|   0|   0|   0|
|2015-01-29 |   0|   0|   0|  50|   0|  50|   0|   0|   0|   0|   0|   0|   0|   0|   0|   0|   0|   0|   0|   0|   0|   0|   0|   0|   0|   0|
|2015-02-26 |   0|   0|   0|   0|  50|   0|   0|   0|   0|   0|   0|   0|   0|   0|   0|   0|   0|   0|   0|   0|   0|   0|   0|   0|  50|   0|
|2015-03-30 |   0|   0|   0|  50|   0|   0|   0|  50|   0|   0|   0|   0|   0|   0|   0|   0|   0|   0|   0|   0|   0|   0|   0|   0|   0|   0|
|2015-04-29 |   0|   0|   0|   0|   0|  50|  50|   0|   0|   0|   0|   0|   0|   0|   0|   0|   0|   0|   0|   0|   0|   0|   0|   0|   0|   0|
|2015-05-28 |   0|   0|   0|   0|   0|   0|   0|   0|   0|   0|   0|   0|   0|   0|  50|   0|   0|   0|   0|   0|   0|   0|   0|  50|   0|   0|
|2015-06-29 |   0|   0|   0|   0|   0|   0|   0|   0|   0|   0|   0|   0|   0|   0|   0|   0|   0|   0|   0|   0|   0|   0|   0| 100|   0|   0|
|2015-07-30 |   0|   0|   0|   0|   0|   0|   0|   0|   0|   0|   0|   0|   0|   0|  50|   0|   0|   0|   0|   0|   0|   0|   0|   0|  50|   0|
|2015-08-28 |   0|   0|   0|   0|   0|   0|   0|   0|   0|   0| 100|   0|   0|   0|   0|   0|   0|   0|   0|   0|   0|   0|   0|   0|   0|   0|
|2015-09-29 |   0|   0|   0|   0|   0|   0|   0|   0|   0|   0| 100|   0|   0|   0|   0|   0|   0|   0|   0|   0|   0|   0|   0|   0|   0|   0|
|2015-10-29 |   0|   0|   0|   0|   0|   0|   0|   0|   0|   0|   0|   0|   0|   0|  50|   0|   0|   0|   0|   0|   0|   0|   0|   0|   0|  50|
|2015-11-27 |   0|   0|   0|   0|   0|   0|   0|   0|   0|   0|   0|   0|  50|   0|   0|   0|   0|   0|   0|   0|   0|   0|   0|   0|  50|   0|
|2015-12-30 |   0|   0|   0|   0|   0|   0|   0|   0|   0|   0|   0|   0|   0|   0|   0|   0|  50|   0|   0|   0|   0|   0|  50|   0|   0|   0|
|2016-01-28 |   0|   0|   0|   0|   0|   0|   0|   0|  50|   0|   0|   0|   0|   0|   0|   0|   0|   0|   0|   0|   0|   0|  50|   0|   0|   0|
    






For your convenience, the 
[Strategy-MEOM](/public/images/Strategy-MEOM/Strategy-MEOM.pdf)
report can also be downloaded and viewed the pdf format.


For more details please check the
[MEOM strategy code at bt.meom.test() function in bt.test.r at github](https://github.com/systematicinvestor/SIT/blob/master/R/bt.test.r)



*(this report was produced on: 2016-02-22)*
