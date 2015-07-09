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
|2015-07-08 | 204.53| 120.29| 84.98|
    




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
{% endhighlight %}



{% highlight text %}
## Error in NextMethod(.Generic): number of items to replace is not a multiple of replacement length
{% endhighlight %}



{% highlight r %}
# go to cash if prices falls below 10 month moving average
go2cash = prices < sma
{% endhighlight %}



{% highlight text %}
## Error in eval(expr, envir, enclos): object 'sma' not found
{% endhighlight %}



{% highlight r %}
  go2cash = ifna(go2cash, T)[period.ends,]
{% endhighlight %}



{% highlight text %}
## Error in len(cond): object 'go2cash' not found
{% endhighlight %}



{% highlight r %}
weight = obj$weights$risk.w.60.40
	weight[go2cash] = 0
{% endhighlight %}



{% highlight text %}
## Error in `[.xts`(x, i, which.i = TRUE): object 'go2cash' not found
{% endhighlight %}



{% highlight r %}
weight$CASH = 1 - rowSums(weight, na.rm=T)
obj$weights$risk.w.60.40.CASH = weight


weight[] = obj$weights$dollar.w.60.40
	weight[go2cash] = 0
{% endhighlight %}



{% highlight text %}
## Error in `[.xts`(x, i, which.i = TRUE): object 'go2cash' not found
{% endhighlight %}



{% highlight r %}
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
|Period        |Dec1989 - Jul2015 |Dec1989 - Jul2015 |Dec1989 - Jul2015 |Dec1989 - Jul2015   |Dec1989 - Jul2015         |
|Cagr          |3.11              |8.09              |8.06              |3.14                |6.4                       |
|Sharpe        |0.68              |0.73              |0.73              |0.68                |0.89                      |
|DVR           |0.67              |0.69              |0.69              |0.67                |0.88                      |
|R2            |0.98              |0.94              |0.94              |0.98                |0.99                      |
|Volatility    |4.67              |11.51             |11.51             |4.71                |7.26                      |
|MaxDD         |-11.14            |-26.4             |-26.4             |-11.25              |-11.57                    |
|Exposure      |99.83             |99.49             |99.83             |99.83               |99.19                     |
|Win.Percent   |60.66             |60.86             |60.66             |60.46               |66.16                     |
|Avg.Trade     |0.27              |0.72              |0.71              |0.28                |0.29                      |
|Profit.Factor |1.77              |1.85              |1.84              |1.77                |1.99                      |
|Num.Trades    |305               |304               |305               |306                 |588                       |
    


![plot of chunk plot-6](/public/images/Strategy-NEW-60-40/plot-6-2.png) 

#Monthly Results for risk.w.60.40.target6.cash :
    




|     |Jan   |Feb   |Mar   |Apr   |May   |Jun   |Jul   |Aug   |Sep   |Oct   |Nov   |Dec   |Year  |MaxDD |
|:----|:-----|:-----|:-----|:-----|:-----|:-----|:-----|:-----|:-----|:-----|:-----|:-----|:-----|:-----|
|1989 |      |      |      |      |      |      |      |      |      |      |      |      |  0.0 |  0.0 |
|1990 |  0.0 | -0.1 |  0.0 | -1.8 |  4.1 |  1.7 |  0.7 | -3.2 |  0.9 |  1.9 |  3.3 |  1.8 |  9.5 | -5.7 |
|1991 |  1.2 |  0.5 |  0.1 |  1.3 |  0.2 | -0.8 |  1.4 |  3.7 |  2.3 | -0.1 |  0.5 |  5.2 | 16.7 | -2.9 |
|1992 | -3.0 |  0.4 | -1.1 |  0.2 |  2.3 |  1.3 |  3.7 |  0.9 |  1.5 | -1.9 |  0.1 |  3.1 |  7.6 | -6.7 |
|1993 |  1.8 |  1.8 |  0.0 |  0.7 |  0.1 |  3.7 |  1.2 |  4.1 |  0.3 |  0.4 | -2.2 |  0.3 | 12.9 | -4.6 |
|1994 |  2.1 | -3.2 | -3.7 | -1.1 | -0.3 | -0.3 |  2.3 | -0.4 | -2.2 | -0.3 |  0.4 |  0.9 | -5.8 |-10.5 |
|1995 |  2.2 |  2.6 |  0.6 |  1.5 |  7.6 |  1.0 | -0.8 |  1.5 |  1.4 |  2.5 |  2.3 |  2.8 | 28.1 | -3.0 |
|1996 |  0.3 | -3.9 | -1.5 | -0.9 | -0.4 |  1.6 |  0.1 | -0.5 |  2.2 |  3.3 |  2.7 | -2.5 |  0.1 | -7.8 |
|1997 | -0.2 |  0.0 | -2.0 |  2.5 |  0.8 |  1.9 |  4.6 | -2.6 |  2.0 |  2.5 |  0.8 |  1.7 | 12.4 | -4.1 |
|1998 |  1.7 | -0.5 |  0.2 |  0.4 |  1.3 |  1.7 | -0.5 |  4.8 |  3.3 | -1.1 |  0.0 |  0.1 | 12.0 | -4.7 |
|1999 |  0.7 | -3.7 |  0.0 |  0.2 | -1.4 | -0.5 | -0.3 | -0.5 |  0.9 |  0.1 | -0.5 | -1.0 | -6.0 | -7.2 |
|2000 |  0.8 |  2.2 |  2.1 | -0.6 | -0.1 |  2.1 |  1.3 |  1.7 | -0.9 |  1.6 |  2.9 |  2.4 | 16.6 | -4.3 |
|2001 |  0.5 |  1.3 | -0.1 | -2.0 |  0.3 |  0.6 |  3.1 |  2.0 |  1.1 |  3.3 | -5.1 | -1.0 |  3.8 | -8.1 |
|2002 |  0.4 |  0.9 | -3.5 |  2.9 |  0.3 |  1.7 |  2.4 |  3.6 |  2.4 | -1.9 | -0.7 |  2.3 | 11.2 | -4.3 |
|2003 | -0.5 |  1.6 | -0.9 |  0.6 |  4.7 | -0.8 | -4.8 |  0.5 |  2.8 | -1.7 |  0.1 |  1.2 |  2.4 | -8.0 |
|2004 |  1.1 |  1.4 |  1.1 | -4.1 | -0.2 |  0.6 |  1.0 |  2.9 |  0.7 |  1.2 | -2.0 |  1.8 |  5.5 | -7.2 |
|2005 |  1.9 | -1.2 | -0.3 |  2.3 |  2.1 |  1.8 | -2.2 |  2.3 | -2.9 | -1.5 |  0.6 |  1.9 |  4.7 | -5.5 |
|2006 | -1.0 |  1.0 | -4.6 | -2.0 | -0.1 |  0.9 |  1.8 |  2.5 |  1.9 |  0.8 |  1.8 | -2.3 |  0.5 | -8.0 |
|2007 | -0.9 |  3.3 | -1.3 |  0.9 | -2.3 | -1.0 |  2.1 |  1.4 |  0.3 |  1.1 |  4.2 | -0.2 |  7.7 | -7.5 |
|2008 |  1.8 |  0.4 |  1.0 | -1.4 | -1.6 |  1.6 |  0.0 |  1.6 |  1.2 |  0.3 |  5.9 |  3.3 | 14.6 | -4.4 |
|2009 | -4.2 | -0.5 |  1.3 | -2.0 | -1.4 |  0.2 |  0.2 |  0.9 |  1.0 | -1.1 |  0.8 | -4.1 | -8.7 | -9.1 |
|2010 |  1.7 | -0.1 | -1.2 |  2.1 |  3.0 |  2.0 | -0.3 |  3.9 | -0.7 | -1.2 | -0.9 | -1.2 |  7.1 | -5.2 |
|2011 | -0.9 |  0.6 | -0.1 |  1.3 |  2.0 | -1.5 |  1.9 |  4.3 |  2.3 | -0.8 |  0.5 |  0.9 | 10.8 | -2.5 |
|2012 | -0.1 | -1.2 | -1.7 |  2.0 |  3.3 | -0.9 |  1.5 | -0.6 | -1.2 | -0.2 |  0.5 | -1.4 | -0.2 | -4.6 |
|2013 | -1.8 |  0.6 | -0.2 |  2.8 | -3.2 | -1.3 | -0.7 | -0.6 |  0.4 |  0.7 | -1.9 | -0.9 | -6.0 | -7.9 |
|2014 |  3.9 |  0.3 |  0.4 |  1.1 |  2.0 | -0.2 |  0.4 |  2.5 | -1.5 |  1.6 |  1.8 |  2.5 | 15.7 | -3.4 |
|2015 |  1.7 | -0.3 | -0.4 | -0.6 | -0.1 | -1.7 |  0.5 |      |      |      |      |      | -0.9 | -3.5 |
|Avg  |  0.4 |  0.2 | -0.6 |  0.2 |  0.9 |  0.6 |  0.8 |  1.5 |  0.8 |  0.4 |  0.6 |  0.7 |  6.4 | -5.6 |
    


![plot of chunk plot-6](/public/images/Strategy-NEW-60-40/plot-6-3.png) ![plot of chunk plot-6](/public/images/Strategy-NEW-60-40/plot-6-4.png) 

#Trades for risk.w.60.40.target6.cash :
    




|risk.w.60.40.target6.cash |weight |entry.date |exit.date  |nhold |entry.price |exit.price |return |
|:-------------------------|:------|:----------|:----------|:-----|:-----------|:----------|:------|
|CASH                      | 21.2  |2014-11-28 |2014-12-31 |33    | 84.49      | 84.24     |-0.06  |
|STOCK                     | 20.6  |2014-12-31 |2015-01-30 |30    |203.64      |197.60     |-0.61  |
|BOND                      | 20.6  |2014-12-31 |2015-01-30 |30    |124.36      |136.57     | 2.02  |
|CASH                      | 58.8  |2014-12-31 |2015-01-30 |30    | 84.24      | 84.77     | 0.37  |
|STOCK                     | 50.0  |2015-01-30 |2015-02-27 |28    |197.60      |208.71     | 2.81  |
|BOND                      | 50.0  |2015-01-30 |2015-02-27 |28    |136.57      |128.18     |-3.07  |
|STOCK                     | 50.0  |2015-02-27 |2015-03-31 |32    |208.71      |205.43     |-0.79  |
|BOND                      | 32.4  |2015-02-27 |2015-03-31 |32    |128.18      |129.58     | 0.35  |
|CASH                      | 17.6  |2015-02-27 |2015-03-31 |32    | 84.52      | 84.73     | 0.04  |
|STOCK                     | 26.5  |2015-03-31 |2015-04-30 |30    |205.43      |207.45     | 0.26  |
|BOND                      | 23.5  |2015-03-31 |2015-04-30 |30    |129.58      |125.15     |-0.81  |
|CASH                      | 50.0  |2015-03-31 |2015-04-30 |30    | 84.73      | 84.76     | 0.02  |
|STOCK                     | 63.6  |2015-04-30 |2015-05-29 |29    |207.45      |210.12     | 0.82  |
|BOND                      | 36.4  |2015-04-30 |2015-05-29 |29    |125.15      |122.18     |-0.86  |
|STOCK                     | 38.2  |2015-05-29 |2015-06-30 |32    |210.12      |205.89     |-0.77  |
|BOND                      | 23.5  |2015-05-29 |2015-06-30 |32    |122.18      |117.27     |-0.94  |
|CASH                      | 38.2  |2015-05-29 |2015-06-30 |32    | 84.80      | 84.82     | 0.01  |
|STOCK                     | 57.6  |2015-06-30 |2015-07-08 | 8    |205.89      |204.53     |-0.38  |
|BOND                      | 33.3  |2015-06-30 |2015-07-08 | 8    |117.27      |120.29     | 0.86  |
|CASH                      |  9.1  |2015-06-30 |2015-07-08 | 8    | 84.82      | 84.98     | 0.02  |
    




#Signals for risk.w.60.40.target6.cash :
    




|           | STOCK| BOND| CASH|
|:----------|-----:|----:|----:|
|2013-11-27 |     0|   44|   56|
|2013-12-30 |     0|   61|   39|
|2014-01-30 |     0|   68|   32|
|2014-02-27 |     0|   61|   39|
|2014-03-28 |     0|   52|   48|
|2014-04-29 |     0|   68|   32|
|2014-05-29 |     0|   59|   41|
|2014-06-27 |     0|   61|   39|
|2014-07-30 |     0|   52|   48|
|2014-08-28 |     0|   67|   33|
|2014-09-29 |     0|   52|   48|
|2014-10-30 |     0|   61|   39|
|2014-11-26 |     0|   79|   21|
|2014-12-30 |    21|   21|   59|
|2015-01-29 |    50|   50|    0|
|2015-02-26 |    50|   32|   18|
|2015-03-30 |    26|   24|   50|
|2015-04-29 |    64|   36|    0|
|2015-05-28 |    38|   24|   38|
|2015-06-29 |    58|   33|    9|
    





For your convenience, the 
[Strategy-NEW-60-40](/public/images/Strategy-NEW-60-40/Strategy-NEW-60-40.pdf)
report can also be downloaded and viewed the pdf format.







*(this report was produced on: 2015-07-09)*
