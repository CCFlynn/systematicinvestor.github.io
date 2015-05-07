---
layout: page
title: New 60/40
---


To install [Systematic Investor Toolbox (SIT)](https://github.com/systematicinvestor/SIT) please visit [About](/about) page.




The [New 60/40](https://systematicinvestor.wordpress.com/2012/08/07/the-new-6040/)
backtest and live signal. For more details please see:

* [You're Looking at the Wrong Number](http://gestaltu.blogspot.ca/2012/07/youre-looking-at-wrong-number.html)
* [The New 60/40](https://systematicinvestor.wordpress.com/2012/08/07/the-new-6040/)

The [New 60/40 Strategy](https://systematicinvestor.wordpress.com/2012/08/07/the-new-6040/)
allocates 60% risk to equities and 40% risk to long-term treasuries.

I think this strategy is missing the go to cash filter. For example of using cash filter please read
the [Quantitative Approach To Tactical Asset Allocation Strategy(QATAA) by Mebane T. Faber](http://mebfaber.com/timing-model/)

Another interesting observation is a spike in risk-parity in 2009-2010. I think this is related
[Volatility and "Crashing Up"](http://blog.thinknewfound.com/volatility-crashing/)


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
STOCK = SPY + VTSMX + VFINX
BOND = TLT + VUSTX
CASH = SHY + TB3Y
'

# load saved Proxies Raw Data, data.proxy.raw
load('data.proxy.raw.Rdata')

data <- new.env()

getSymbols.extra(tickers, src = 'yahoo', from = '1970-01-01', env = data, raw.data = data.proxy.raw, auto.assign = T, set.symbolnames = T, getSymbols.fn = getSymbols.fn, calendar=calendar)
  for(i in data$symbolnames) data[[i]] = adjustOHLC(data[[i]], use.Adjusted=T)
bt.prep(data, align='remove.na', dates='::')

print(last(data$prices))
{% endhighlight %}



|           |  STOCK|   BOND|  CASH|
|:----------|------:|------:|-----:|
|2015-05-06 | 208.04| 120.56| 84.75|
    




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
# Traditional, Dollar Weighted 40% Bonds & 60% Stock
#******************************************************************
target.allocation = NA * prices[1,]
	target.allocation$STOCK = 60/100
 target.allocation$BOND = 40/100

obj$weights$dollar.w.60.40 = rep.row(target.allocation, len(period.ends))

#*****************************************************************
# Risk Weighted 40% Bonds & 60% Stock
#******************************************************************
ret = diff(log(prices))
hist.vol = bt.apply.matrix(ret, runSD, n = 20)

# risk-parity
weight.risk = 1 / hist.vol
	weight.risk = weight.risk / rowSums(weight.risk, na.rm=T)

obj$weights$risk.w.60.40 = weight.risk[period.ends,]

#*****************************************************************
# Cash Filter
#******************************************************************
# compute 10 month moving average
sma = bt.apply.matrix(prices, SMA, 200)

# go to cash if prices falls below 10 month moving average
go2cash = prices < sma
  go2cash = ifna(go2cash, T)[period.ends,]


weight = obj$weights$risk.w.60.40
	weight[go2cash] = 0
weight$CASH = 1 - rowSums(weight, na.rm=T)
obj$weights$risk.w.60.40.CASH = weight


weight[] = obj$weights$dollar.w.60.40
	weight[go2cash] = 0
weight$CASH = 1 - rowSums(weight, na.rm=T)
obj$weights$dollar.w.60.40.CASH = weight

#*****************************************************************
# Scale Risk Weighted 40% Bonds & 60% Stock strategy to have 6% volatility
#****************************************************************** 
models = get.back.test(data, obj, input)

weight = target.vol.strategy(models$risk.w.60.40, ifna(weight.risk,0),
		target=6/100, lookback.len=21, max.portfolio.leverage=100/100)

# invested not allocated to CASH
weight$CASH = 1 - rowSums(weight)

obj$weights$risk.w.60.40.target6.cash = weight[period.ends,]
{% endhighlight %}


![plot of chunk plot-6](/public/images/Strategy-NEW-60-40/plot-6-1.png) 

#Strategy Performance:
    




|              |dollar.w.60.40    |risk.w.60.40      |risk.w.60.40.CASH |dollar.w.60.40.CASH |risk.w.60.40.target6.cash |
|:-------------|:-----------------|:-----------------|:-----------------|:-------------------|:-------------------------|
|Period        |Dec1989 - May2015 |Dec1989 - May2015 |Dec1989 - May2015 |Dec1989 - May2015   |Dec1989 - May2015         |
|Cagr          |9.04              |9.74              |9.35              |9.8                 |8.22                      |
|Sharpe        |0.86              |1.14              |1.19              |1.16                |1.24                      |
|DVR           |0.78              |1.03              |1.12              |1.09                |1.16                      |
|R2            |0.91              |0.9               |0.94              |0.94                |0.94                      |
|Volatility    |10.77             |8.45              |7.76              |8.37                |6.56                      |
|MaxDD         |-31.76            |-18.66            |-15.04            |-10.31              |-9.96                     |
|Exposure      |99.83             |99.48             |99.83             |99.83               |99.19                     |
|Win.Percent   |62.17             |61.55             |66.21             |67.18               |64.47                     |
|Avg.Trade     |0.4               |0.42              |0.41              |0.44                |0.26                      |
|Profit.Factor |1.71              |1.81              |2.08              |2.22                |1.89                      |
|Num.Trades    |600               |606               |589               |579                 |833                       |
    


![plot of chunk plot-6](/public/images/Strategy-NEW-60-40/plot-6-2.png) 

#Monthly Results for risk.w.60.40.target6.cash :
    




|     |Jan  |Feb  |Mar  |Apr  |May  |Jun  |Jul  |Aug  |Sep  |Oct  |Nov  |Dec  |Year |MaxDD |
|:----|:----|:----|:----|:----|:----|:----|:----|:----|:----|:----|:----|:----|:----|:-----|
|1989 |     |     |     |     |     |     |     |     |     |     |     |     | 0.0 | 0.0  |
|1990 | 0.0 |-0.1 | 0.8 |-1.8 | 5.8 | 0.8 | 0.6 |-4.8 | 0.1 | 1.3 | 3.2 | 1.9 | 7.6 |-7.4  |
|1991 | 2.1 | 1.5 | 0.6 | 1.0 | 1.1 |-1.5 | 2.2 | 3.2 | 1.3 | 0.5 |-1.2 | 5.7 |17.6 |-2.7  |
|1992 |-2.7 | 0.7 |-1.4 | 1.4 | 1.6 | 0.2 | 3.8 |-0.5 | 1.4 |-0.7 | 1.6 | 2.5 | 8.1 |-4.4  |
|1993 | 1.4 | 1.4 | 0.6 |-0.4 | 0.9 | 2.3 | 0.5 | 4.0 |-0.2 | 1.1 |-1.7 | 0.7 |10.9 |-3.6  |
|1994 | 2.9 |-3.3 |-3.1 |-0.4 | 0.2 |-0.9 | 2.2 | 1.7 |-2.3 | 0.8 |-0.9 | 0.8 |-2.6 |-8.2  |
|1995 | 2.6 | 3.5 | 1.7 | 2.0 | 6.1 | 1.4 | 0.7 | 1.0 | 3.3 | 1.2 | 3.2 | 2.3 |32.8 |-1.9  |
|1996 | 1.3 |-2.2 |-0.6 |-0.4 | 0.4 | 1.2 |-2.1 | 0.1 | 2.8 | 2.9 | 4.0 |-2.5 | 4.7 |-6.5  |
|1997 | 1.2 | 0.2 |-2.4 | 2.7 | 1.5 | 2.1 | 4.9 |-2.6 | 2.4 | 1.2 | 1.3 | 1.7 |14.8 |-5.0  |
|1998 | 1.5 | 1.3 | 2.0 | 0.7 | 0.4 | 2.2 |-0.6 | 0.2 | 3.8 | 0.0 | 0.9 | 2.7 |16.2 |-4.4  |
|1999 | 1.1 |-3.8 | 0.9 | 0.8 |-1.4 | 0.7 |-0.9 |-0.4 | 0.3 | 1.2 |-0.1 | 0.9 |-0.8 |-5.3  |
|2000 |-1.0 | 1.3 | 3.3 |-0.8 |-0.3 | 2.0 | 0.5 | 2.6 |-2.5 | 0.9 | 1.5 | 1.8 | 9.5 |-4.0  |
|2001 | 1.2 |-1.2 |-1.2 |-0.4 | 0.2 |-0.2 | 1.9 | 0.2 |-1.6 | 2.8 |-2.1 |-0.7 |-1.1 |-6.6  |
|2002 | 0.1 | 0.2 |-1.9 | 0.5 | 0.1 |-0.7 |-0.2 | 3.4 | 0.4 |-0.4 | 0.8 | 0.3 | 2.4 |-5.9  |
|2003 |-0.9 | 1.4 |-0.9 | 3.4 | 6.1 |-0.3 |-3.0 | 0.8 | 1.5 | 0.8 | 0.7 | 3.2 |13.1 |-6.6  |
|2004 | 1.7 | 1.6 | 0.1 |-3.7 | 0.4 | 0.9 |-0.5 | 2.2 | 0.9 | 1.5 | 0.1 | 2.3 | 7.6 |-6.2  |
|2005 |-0.1 | 0.1 |-0.9 | 0.8 | 2.6 | 1.2 | 0.4 | 0.9 |-1.6 |-2.3 | 1.5 | 0.9 | 3.4 |-4.8  |
|2006 | 0.6 | 0.9 |-2.2 |-0.7 |-1.4 | 0.6 | 1.4 | 2.3 | 2.2 | 1.9 | 2.1 |-1.0 | 7.0 |-5.1  |
|2007 | 0.3 | 0.9 |-0.7 | 1.9 | 0.3 |-1.2 | 0.6 | 1.5 | 1.0 | 1.2 | 1.7 |-0.7 | 7.0 |-4.3  |
|2008 |-0.7 |-0.7 | 0.4 |-0.3 |-0.9 |-1.5 |-0.5 | 1.8 |-1.9 |-3.8 | 3.6 | 3.3 |-1.3 |-8.7  |
|2009 |-5.1 |-2.1 | 2.4 |-0.7 |-0.2 | 0.1 | 2.0 | 1.8 | 2.2 |-1.4 | 2.5 |-2.6 |-1.5 |-7.8  |
|2010 |-0.8 | 0.7 | 1.0 | 2.1 |-0.6 | 1.0 | 1.7 | 2.8 | 2.7 | 0.2 |-0.7 | 1.0 |11.8 |-3.7  |
|2011 | 0.8 | 2.5 |-0.1 | 2.5 | 0.7 |-2.0 | 1.1 | 1.5 | 1.8 | 2.1 | 0.7 | 1.8 |14.2 |-2.7  |
|2012 | 1.7 | 1.7 | 0.6 | 1.5 | 0.9 | 1.1 | 2.6 | 0.5 | 0.5 |-1.3 | 0.8 |-1.1 | 9.7 |-3.0  |
|2013 | 1.2 | 1.2 | 1.6 | 3.1 |-2.4 |-1.8 | 0.6 |-1.7 | 1.5 | 3.0 |-0.2 | 0.6 | 6.7 |-6.1  |
|2014 | 1.1 | 2.1 | 0.8 | 1.3 | 2.7 | 1.0 |-0.6 | 4.2 |-1.5 | 2.5 | 2.0 | 1.2 |18.0 |-2.4  |
|2015 | 2.9 |-0.3 |-0.4 |-0.6 |-1.7 |     |     |     |     |     |     |     |-0.1 |-3.4  |
|Avg  | 0.6 | 0.4 | 0.0 | 0.6 | 0.9 | 0.3 | 0.8 | 1.1 | 0.7 | 0.7 | 1.0 | 1.1 | 8.0 |-4.8  |
    


![plot of chunk plot-6](/public/images/Strategy-NEW-60-40/plot-6-3.png) ![plot of chunk plot-6](/public/images/Strategy-NEW-60-40/plot-6-4.png) 

#Trades for risk.w.60.40.target6.cash :
    




|risk.w.60.40.target6.cash |weight |entry.date |exit.date  |nhold |entry.price |exit.price |return |
|:-------------------------|:------|:----------|:----------|:-----|:-----------|:----------|:------|
|STOCK                     |55.9   |2014-09-30 |2014-10-31 |31    |195.07      |199.66     | 1.32  |
|BOND                      |44.1   |2014-09-30 |2014-10-31 |31    |114.50      |117.72     | 1.24  |
|STOCK                     |24.2   |2014-10-31 |2014-11-28 |28    |199.66      |205.15     | 0.67  |
|BOND                      |45.5   |2014-10-31 |2014-11-28 |28    |117.72      |121.22     | 1.35  |
|CASH                      |30.3   |2014-10-31 |2014-11-28 |28    | 84.61      | 84.70     | 0.03  |
|STOCK                     |58.8   |2014-11-28 |2014-12-31 |33    |205.15      |204.63     |-0.15  |
|BOND                      |41.2   |2014-11-28 |2014-12-31 |33    |121.22      |125.16     | 1.34  |
|STOCK                     |41.2   |2014-12-31 |2015-01-30 |30    |204.63      |198.56     |-1.22  |
|BOND                      |41.2   |2014-12-31 |2015-01-30 |30    |125.16      |137.44     | 4.04  |
|CASH                      |17.6   |2014-12-31 |2015-01-30 |30    | 84.42      | 84.95     | 0.11  |
|STOCK                     |50.0   |2015-01-30 |2015-02-27 |28    |198.56      |209.72     | 2.81  |
|BOND                      |50.0   |2015-01-30 |2015-02-27 |28    |137.44      |129.01     |-3.07  |
|STOCK                     |50.0   |2015-02-27 |2015-03-31 |32    |209.72      |206.43     |-0.79  |
|BOND                      |32.4   |2015-02-27 |2015-03-31 |32    |129.01      |130.42     | 0.35  |
|CASH                      |17.6   |2015-02-27 |2015-03-31 |32    | 84.67      | 84.88     | 0.04  |
|STOCK                     |26.5   |2015-03-31 |2015-04-30 |30    |206.43      |208.46     | 0.26  |
|BOND                      |23.5   |2015-03-31 |2015-04-30 |30    |130.42      |125.95     |-0.81  |
|CASH                      |50.0   |2015-03-31 |2015-04-30 |30    | 84.88      | 84.87     |-0.01  |
|STOCK                     |63.6   |2015-04-30 |2015-05-06 | 6    |208.46      |208.04     |-0.13  |
|BOND                      |36.4   |2015-04-30 |2015-05-06 | 6    |125.95      |120.56     |-1.56  |
    




#Signals for risk.w.60.40.target6.cash :
    




|           | STOCK| BOND| CASH|
|:----------|-----:|----:|----:|
|2013-09-27 |    55|   36|    9|
|2013-10-30 |    26|   38|   35|
|2013-11-27 |    45|   27|   27|
|2013-12-30 |    41|   41|   18|
|2014-01-30 |    39|   61|    0|
|2014-02-27 |    42|   58|    0|
|2014-03-28 |    55|   45|    0|
|2014-04-29 |    39|   61|    0|
|2014-05-29 |    56|   44|    0|
|2014-06-27 |    64|   36|    0|
|2014-07-30 |    59|   41|    0|
|2014-08-28 |    45|   39|   15|
|2014-09-29 |    56|   44|    0|
|2014-10-30 |    24|   45|   30|
|2014-11-26 |    59|   41|    0|
|2014-12-30 |    41|   41|   18|
|2015-01-29 |    50|   50|    0|
|2015-02-26 |    50|   32|   18|
|2015-03-30 |    26|   24|   50|
|2015-04-29 |    64|   36|    0|
    





For your convenience, the 
[Strategy-NEW-60-40](/public/images/Strategy-NEW-60-40/Strategy-NEW-60-40.pdf)
report can also be downloaded and viewed the pdf format.







*(this report was produced on: 2015-05-07)*
