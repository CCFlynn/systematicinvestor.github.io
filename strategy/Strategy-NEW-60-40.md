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



|           |  STOCK|  BOND|  CASH|
|:----------|------:|-----:|-----:|
|2016-03-04 | 200.43| 128.6| 84.76|
    




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
|Period        |May1986 - Mar2016 |May1986 - Mar2016 |May1986 - Mar2016 |May1986 - Mar2016   |May1986 - Mar2016         |
|Cagr          |8.98              |9.27              |8.48              |8.58                |7.95                      |
|Sharpe        |0.83              |1.07              |1.07              |0.97                |1.17                      |
|DVR           |0.76              |0.95              |0.98              |0.89                |1.09                      |
|R2            |0.92              |0.89              |0.92              |0.92                |0.93                      |
|Volatility    |11.16             |8.65              |7.88              |8.88                |6.74                      |
|MaxDD         |-31.76            |-18.66            |-15.04            |-21.19              |-11.39                    |
|Exposure      |99.88             |99.6              |99.88             |99.88               |99.31                     |
|Win.Percent   |61.26             |61.06             |65.01             |65.11               |64.11                     |
|Avg.Trade     |0.39              |0.4               |0.38              |0.39                |0.25                      |
|Profit.Factor |1.69              |1.8               |2.02              |2.01                |1.9                       |
|Num.Trades    |715               |714               |686               |685                 |989                       |
    


![plot of chunk plot-6](/public/images/Strategy-NEW-60-40/plot-6-2.png) 

#Monthly Results for risk.w.60.40.target6.cash :
    




|     |Jan   |Feb   |Mar   |Apr   |May   |Jun   |Jul   |Aug   |Sep   |Oct   |Nov   |Dec   |Year  |MaxDD |
|:----|:-----|:-----|:-----|:-----|:-----|:-----|:-----|:-----|:-----|:-----|:-----|:-----|:-----|:-----|
|1986 |      |      |      |      |      |  0.0 | -0.1 |  4.5 | -4.9 |  1.8 |  1.9 | -2.2 |  0.7 | -6.0 |
|1987 |  1.9 |  1.8 | -0.3 | -3.1 | -0.3 |  1.9 |  1.4 |  0.3 | -2.6 | -0.3 |  0.1 |  2.1 |  2.6 |-11.4 |
|1988 |  3.4 |  1.3 | -2.1 | -0.4 | -0.9 |  4.1 | -0.8 | -0.7 |  3.2 |  2.0 | -1.4 |  0.4 |  8.0 | -4.5 |
|1989 |  3.4 | -1.8 |  1.0 |  3.1 |  3.1 |  2.9 |  3.8 | -0.9 |  0.0 |  1.2 |  1.1 |  0.7 | 18.9 | -3.1 |
|1990 | -4.8 |  0.0 |  0.6 | -2.0 |  6.0 |  1.0 |  0.6 | -5.1 |  0.1 |  1.3 |  3.3 |  1.9 |  2.4 | -7.8 |
|1991 |  2.0 |  1.5 |  0.7 |  0.9 |  1.1 | -1.5 |  2.3 |  3.0 |  1.4 |  0.4 | -1.0 |  5.9 | 17.9 | -2.6 |
|1992 | -2.6 |  0.8 | -1.3 |  1.2 |  1.8 |  0.0 |  3.9 | -0.5 |  1.3 | -0.7 |  1.8 |  2.3 |  8.0 | -4.1 |
|1993 |  1.5 |  1.4 |  0.7 | -0.4 |  0.8 |  2.3 |  0.6 |  3.9 | -0.2 |  1.0 | -1.7 |  0.7 | 11.0 | -3.6 |
|1994 |  2.9 | -3.5 | -3.0 | -0.5 |  0.2 | -0.9 |  2.3 |  1.8 | -2.3 |  0.7 | -0.9 |  0.9 | -2.6 | -8.3 |
|1995 |  2.7 |  3.4 |  1.6 |  2.0 |  6.1 |  1.3 |  0.7 |  1.0 |  3.3 |  1.2 |  3.1 |  2.2 | 32.6 | -2.0 |
|1996 |  1.3 | -2.1 | -0.5 | -0.4 |  0.4 |  1.2 | -2.2 |  0.0 |  2.8 |  3.0 |  3.9 | -2.4 |  4.8 | -6.5 |
|1997 |  1.2 |  0.1 | -2.3 |  2.6 |  1.6 |  2.0 |  5.1 | -2.5 |  2.4 |  1.1 |  1.2 |  1.7 | 14.9 | -5.1 |
|1998 |  1.7 |  1.2 |  2.0 |  0.6 |  0.5 |  2.2 | -0.7 | -0.2 |  3.7 |  0.0 |  0.9 |  2.4 | 15.2 | -4.8 |
|1999 |  1.3 | -3.8 |  0.9 |  0.8 | -1.5 |  0.9 | -1.0 | -0.3 |  0.3 |  1.1 |  0.0 |  0.8 | -0.7 | -5.3 |
|2000 | -1.0 |  1.3 |  3.4 | -0.8 | -0.3 |  1.9 |  0.5 |  2.7 | -2.4 |  0.9 |  1.6 |  1.8 |  9.8 | -4.1 |
|2001 |  1.2 | -1.2 | -1.2 | -0.5 |  0.2 | -0.3 |  2.0 |  0.4 | -1.6 |  2.9 | -2.0 | -0.7 | -0.9 | -6.5 |
|2002 |  0.1 |  0.2 | -1.9 |  0.6 |  0.1 | -0.8 |  0.1 |  3.6 |  0.7 | -0.4 |  0.7 |  0.4 |  3.3 | -5.4 |
|2003 | -1.0 |  1.4 | -0.9 |  3.4 |  6.1 | -0.4 | -3.5 |  0.6 |  1.9 |  0.7 |  0.7 |  3.2 | 12.7 | -7.3 |
|2004 |  1.7 |  1.6 |  0.2 | -4.0 |  0.2 |  1.0 | -0.5 |  2.3 |  0.9 |  1.5 |  0.1 |  2.3 |  7.4 | -6.7 |
|2005 | -0.1 |  0.0 | -1.0 |  0.9 |  2.7 |  1.2 |  0.3 |  0.9 | -1.6 | -2.3 |  1.5 |  0.9 |  3.4 | -4.8 |
|2006 |  0.6 |  0.9 | -2.2 | -0.7 | -1.5 |  0.6 |  1.5 |  2.4 |  2.2 |  1.9 |  2.1 | -1.0 |  7.0 | -5.1 |
|2007 |  0.3 |  0.9 | -0.7 |  1.9 |  0.3 | -1.2 |  0.6 |  1.5 |  1.0 |  1.2 |  1.7 | -0.7 |  7.1 | -4.3 |
|2008 | -0.7 | -0.6 |  0.5 | -0.3 | -0.9 | -1.4 | -0.5 |  1.8 | -1.8 | -3.8 |  3.6 |  3.3 | -1.0 | -8.5 |
|2009 | -5.1 | -2.1 |  2.4 | -0.7 | -0.2 |  0.1 |  2.0 |  1.9 |  2.2 | -1.4 |  2.5 | -2.6 | -1.4 | -7.8 |
|2010 | -0.8 |  0.7 |  1.0 |  2.1 | -0.6 |  1.0 |  1.7 |  2.8 |  2.7 |  0.2 | -0.7 |  1.0 | 11.8 | -3.7 |
|2011 |  0.8 |  2.4 | -0.1 |  2.5 |  0.7 | -2.0 |  1.1 |  1.5 |  1.8 |  2.1 |  0.7 |  1.8 | 14.2 | -2.7 |
|2012 |  1.7 |  1.7 |  0.6 |  1.5 |  0.9 |  1.1 |  2.6 |  0.5 |  0.5 | -1.3 |  0.8 | -1.1 |  9.7 | -3.0 |
|2013 |  1.2 |  1.2 |  1.6 |  3.1 | -2.4 | -1.8 |  0.6 | -1.8 |  1.5 |  3.0 | -0.2 |  0.6 |  6.7 | -6.1 |
|2014 |  1.1 |  2.1 |  0.8 |  1.3 |  2.7 |  1.0 | -0.6 |  4.2 | -1.5 |  2.5 |  2.0 |  1.2 | 18.0 | -2.4 |
|2015 |  2.9 | -0.3 | -0.4 | -0.6 | -0.1 | -1.8 |  2.8 | -3.4 |  0.2 |  2.8 | -0.3 | -0.8 |  1.0 | -5.6 |
|2016 |  0.6 |  1.4 |  0.3 |      |      |      |      |      |      |      |      |      |  2.3 | -0.9 |
|Avg  |  0.6 |  0.4 |  0.0 |  0.5 |  0.9 |  0.5 |  0.9 |  0.9 |  0.5 |  0.8 |  0.9 |  0.9 |  7.9 | -5.2 |
    


![plot of chunk plot-6](/public/images/Strategy-NEW-60-40/plot-6-3.png) ![plot of chunk plot-6](/public/images/Strategy-NEW-60-40/plot-6-4.png) 

#Trades for risk.w.60.40.target6.cash :
    




|risk.w.60.40.target6.cash |weight |entry.date |exit.date  |nhold |entry.price |exit.price |return |
|:-------------------------|:------|:----------|:----------|:-----|:-----------|:----------|:------|
|BOND                      |29.4   |2015-08-31 |2015-09-30 |30    |119.91      |122.27     | 0.58  |
|CASH                      |52.9   |2015-08-31 |2015-09-30 |30    | 84.57      | 84.82     | 0.16  |
|STOCK                     |36.4   |2015-09-30 |2015-10-30 |30    |190.50      |206.70     | 3.09  |
|BOND                      |48.5   |2015-09-30 |2015-10-30 |30    |122.27      |121.77     |-0.20  |
|CASH                      |15.2   |2015-09-30 |2015-10-30 |30    | 84.82      | 84.70     |-0.02  |
|STOCK                     |45.5   |2015-10-30 |2015-11-30 |31    |206.70      |207.46     | 0.17  |
|BOND                      |48.5   |2015-10-30 |2015-11-30 |31    |121.77      |120.71     |-0.42  |
|CASH                      | 6.1   |2015-10-30 |2015-11-30 |31    | 84.70      | 84.48     |-0.02  |
|STOCK                     |35.3   |2015-11-30 |2015-12-31 |31    |207.46      |203.87     |-0.61  |
|BOND                      |50.0   |2015-11-30 |2015-12-31 |31    |120.71      |120.35     |-0.15  |
|CASH                      |14.7   |2015-11-30 |2015-12-31 |31    | 84.48      | 84.32     |-0.03  |
|STOCK                     |24.2   |2015-12-31 |2016-01-29 |29    |203.87      |193.72     |-1.21  |
|BOND                      |27.3   |2015-12-31 |2016-01-29 |29    |120.35      |127.06     | 1.52  |
|CASH                      |48.5   |2015-12-31 |2016-01-29 |29    | 84.32      | 84.87     | 0.32  |
|STOCK                     |27.3   |2016-01-29 |2016-02-29 |31    |193.72      |193.35     |-0.05  |
|BOND                      |48.5   |2016-01-29 |2016-02-29 |31    |127.06      |130.98     | 1.50  |
|CASH                      |24.2   |2016-01-29 |2016-02-29 |31    | 84.87      | 84.96     | 0.03  |
|STOCK                     |29.4   |2016-02-29 |2016-03-04 | 4    |193.35      |200.43     | 1.08  |
|BOND                      |41.2   |2016-02-29 |2016-03-04 | 4    |130.98      |128.60     |-0.75  |
|CASH                      |29.4   |2016-02-29 |2016-03-04 | 4    | 84.96      | 84.76     |-0.07  |
    




#Signals for risk.w.60.40.target6.cash :
    




|           | STOCK| BOND| CASH|
|:----------|-----:|----:|----:|
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
|2015-05-28 |    38|   24|   38|
|2015-06-29 |    58|   33|    9|
|2015-07-30 |    50|   41|    9|
|2015-08-28 |    18|   29|   53|
|2015-09-29 |    36|   48|   15|
|2015-10-29 |    45|   48|    6|
|2015-11-27 |    35|   50|   15|
|2015-12-30 |    24|   27|   48|
|2016-01-28 |    27|   48|   24|
|2016-02-26 |    29|   41|   29|
    





For your convenience, the 
[Strategy-NEW-60-40](/public/images/Strategy-NEW-60-40/Strategy-NEW-60-40.pdf)
report can also be downloaded and viewed the pdf format.







*(this report was produced on: 2016-03-06)*
