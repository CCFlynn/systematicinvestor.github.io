---
layout: post
title: Channel Breakout
comments: true
---


To install [Systematic Investor Toolbox (SIT)](https://github.com/systematicinvestor/SIT) please visit [About](',base.url,'about) page.





David Varadi did a few posts on Channel Breakout systems:

* [Percentile Channels: A New Twist On a Trend-Following Favorite](https://cssanalytics.wordpress.com/2015/01/21/percentile-channels-a-new-twist-on-a-trend-following-favorite/)
* [A Simple Tactical Asset Allocation Portfolio with Percentile Channels](https://cssanalytics.wordpress.com/2015/01/26/a-simple-tactical-asset-allocation-portfolio-with-percentile-channels/)

Below I will try to adapt a code from the posts:



{% highlight r %}
#*****************************************************************
# First, reproduce S&P 500 chart
#*****************************************************************
library(SIT)
load.packages('quantmod')
tickers = 'SP=^GSPC'

data <- new.env()
getSymbols.extra(tickers, src = 'yahoo', from = '1970-01-01', env = data, auto.assign = T)
for(i in ls(data)) data[[i]] = adjustOHLC(data[[i]], use.Adjusted=T)

# compute Donchian channels
d.high.channel = runMax(Hi(data$SP), 55)
d.low.channel = runMin(Lo(data$SP), 55)

# compute Percentile channels
p.high.channel = runQuantile(Cl(data$SP), 55, probs=0.75)
p.low.channel = runQuantile(Cl(data$SP), 55, probs=0.25)

# compute Average
sma = SMA(Cl(data$SP), 55)

# make plot
plota(data$SP['2013:10::2014',],type='l', col='lightblue', lwd=2)
  plota.legend('SP')
plota.lines(sma, col='green', lwd=2)
plota.lines(d.high.channel, col='red', lwd=2)
plota.lines(d.low.channel, col='gray', lwd=2)
plota.lines(p.high.channel, col='yellow', lwd=2)
plota.lines(p.low.channel, col='blue', lwd=2)
{% endhighlight %}

![plot of chunk plot-2](/public/images/2015-02-02-Channel-Breakout/plot-2-1.png) 

{% highlight r %}
#*****************************************************************
# Load historical data
#*****************************************************************
library(SIT)
load.packages('quantmod')
tickers = 'DBC+CRB'

# load saved Proxies Raw Data, data.proxy.raw
# please see http://systematicinvestor.github.io/Data-Proxy/ for more details
load('data/data.proxy.raw.Rdata')

data <- new.env()
getSymbols.extra(tickers, src = 'yahoo', from = '1970-01-01', env = data, raw.data = data.proxy.raw, auto.assign = T)
for(i in ls(data)) data[[i]] = adjustOHLC(data[[i]], use.Adjusted=T)
bt.prep(data, align='remove.na')

plota(data$DBC,type='l')
  plota.legend('DBC', 'black', data$DBC)
{% endhighlight %}

![plot of chunk plot-2](/public/images/2015-02-02-Channel-Breakout/plot-2-2.png) 

{% highlight r %}
#*****************************************************************
# Helper functions
#*****************************************************************
donchian.channel.breakout.strategy = function(data, lockback.len, long.only=F, use.close=F) {
  if(use.close) {
    high.channel = bt.apply.matrix(data$prices, runMax, lockback.len)
    low.channel = bt.apply.matrix(data$prices, runMin, lockback.len)
   } else {
    phigh = bt.apply(data, Hi)
    plow = bt.apply(data, Lo)

    high.channel = bt.apply.matrix(phigh, runMax, lockback.len)
    low.channel = bt.apply.matrix(plow, runMin, lockback.len)
  }

  data$weight[] = NA
    if(use.close)
      data$weight[] = iif(data$prices == high.channel, 1, iif(data$prices == low.channel, -1, NA))
    else
      data$weight[] = iif(phigh == high.channel, 1, iif(plow == low.channel, iif(long.only,0,-1), NA)) 
  bt.run.share(data, clean.signal=T, silent=T)
}

percentile.channel.breakout.strategy = function(data, lockback.len, long.only=F) {
  high.channel = bt.apply.matrix(data$prices, runQuantile, lockback.len, probs=0.75)
  low.channel = bt.apply.matrix(data$prices, runQuantile, lockback.len, probs=0.25)

  data$weight[] = NA
    data$weight[] = iif(cross.up(prices, high.channel), 1, iif(cross.dn(prices, low.channel), iif(long.only,0,-1), NA))
  bt.run.share(data, clean.signal=T, silent=T)
}

#*****************************************************************
# Setup
#*****************************************************************
prices = data$prices

models = list()

#*****************************************************************
# Donchian Channel Breakout strategy
#*****************************************************************
models$Donchian55 = donchian.channel.breakout.strategy(data, 55)
models$Donchian55.Long = donchian.channel.breakout.strategy(data, 55, long.only=T)

print('#Transition Map for Donchian Channel Breakout strategy using 55 day lookback:')
{% endhighlight %}



#Transition Map for Donchian Channel Breakout strategy using 55 day lookback:
    




{% highlight r %}
plotbt.transition.map(models$Donchian55$weight)
{% endhighlight %}

![plot of chunk plot-2](/public/images/2015-02-02-Channel-Breakout/plot-2-3.png) 

{% highlight r %}
#*****************************************************************
# Percentile Channel Breakout strategy
#*****************************************************************
models$Percentile55 = percentile.channel.breakout.strategy(data, 55)
models$Percentile55.Long = percentile.channel.breakout.strategy(data, 55, long.only=T)

print('#Transition Map for Percentile Channel Breakout strategy using 55 day lookback:')
{% endhighlight %}



#Transition Map for Percentile Channel Breakout strategy using 55 day lookback:
    




{% highlight r %}
plotbt.transition.map(models$Percentile55$weight)
{% endhighlight %}

![plot of chunk plot-2](/public/images/2015-02-02-Channel-Breakout/plot-2-4.png) 

{% highlight r %}
models$Donchian20 = donchian.channel.breakout.strategy(data, 20)
models$Percentile20 = percentile.channel.breakout.strategy(data, 20)

#*****************************************************************
# Report
#strategy.performance.snapshoot(models, T)
plotbt(models, plotX = T, log = 'y', LeftMargin = 3, main = NULL)	    	
	mtext('Cumulative Performance', side = 2, line = 1)
{% endhighlight %}

![plot of chunk plot-2](/public/images/2015-02-02-Channel-Breakout/plot-2-5.png) 

{% highlight r %}
print(plotbt.strategy.sidebyside(models, make.plot=F, return.table=T))
{% endhighlight %}



|           |Donchian55        |Donchian55.Long   |Percentile55      |Percentile55.Long |Donchian20        |Percentile20      |
|:----------|:-----------------|:-----------------|:-----------------|:-----------------|:-----------------|:-----------------|
|Period     |Jan1994 - Feb2015 |Jan1994 - Feb2015 |Jan1994 - Feb2015 |Jan1994 - Feb2015 |Jan1994 - Feb2015 |Jan1994 - Feb2015 |
|Cagr       |8.98              |8.82              |7.02              |7.89              |0.16              |5.42              |
|Sharpe     |0.63              |0.73              |0.51              |0.68              |0.09              |0.41              |
|DVR        |0.53              |0.67              |0.42              |0.63              |0                 |0.29              |
|Volatility |15.51             |12.8              |15.69             |12.36             |16.1              |16.17             |
|MaxDD      |-44.97            |-29.1             |-42.09            |-26.25            |-68.3             |-37.87            |
|AvgDD      |-3.13             |-2.8              |-3.88             |-3.05             |-4.75             |-3.55             |
|VaR        |-1.54             |-1.29             |-1.57             |-1.25             |-1.64             |-1.64             |
|CVaR       |-2.16             |-1.94             |-2.19             |-1.9              |-2.3              |-2.28             |
|Exposure   |98.62             |63.34             |98.79             |59.35             |99.62             |99.42             |
    

Unfortunately we cannot replicate results from original [source](https://cssanalytics.wordpress.com/2015/01/21/percentile-channels-a-new-twist-on-a-trend-following-favorite/)

I.e. Percentile vs Donchian there is no clear winner in our tests.

Next, let's examine the [A Simple Tactical Asset Allocation Portfolio with Percentile Channels](https://cssanalytics.wordpress.com/2015/01/26/a-simple-tactical-asset-allocation-portfolio-with-percentile-channels/)
in more details:



{% highlight r %}
#*****************************************************************
# Load historical data
#*****************************************************************
library(SIT)
load.packages('quantmod')

# load saved Proxies Raw Data, data.proxy.raw
# please see http://systematicinvestor.github.io/Data-Proxy/ for more details
load('data/data.proxy.raw.Rdata')

tickers = '
EQ = VTI +VTSMX # (or SPY)
RE = IYR + VGSIX # (or ICF)
CORP.FI = LQD + VWESX
COM = DBC + CRB
CASH = SHY + TB3Y
'

data <- new.env()
getSymbols.extra(tickers, src = 'yahoo', from = '1970-01-01', env = data, raw.data = data.proxy.raw, set.symbolnames = T, auto.assign = T)
for(i in data$symbolnames) data[[i]] = adjustOHLC(data[[i]], use.Adjusted=T)

#print(bt.start.dates(data))
bt.prep(data, align='remove.na', fill.gaps = T)

# Check data
plota.matplot(scale.one(data$prices),main='Asset Perfromance')
{% endhighlight %}

![plot of chunk plot-3](/public/images/2015-02-02-Channel-Breakout/plot-3-1.png) 

{% highlight r %}
#*****************************************************************
# Setup
#*****************************************************************
data$universe = data$prices > 0
  # do not allocate to CASH, or BENCH
  data$universe$CASH = NA

prices = data$prices * data$universe
  n = ncol(prices)
  nperiods = nrow(prices)


frequency = 'months'
# find period ends, can be 'weeks', 'months', 'quarters', 'years'
period.ends = endpoints(prices, frequency)
  period.ends = period.ends[period.ends > 0]

models = list()


# lag prices by 1 day
#prices = mlag(prices)

#*****************************************************************
# Equal Weight each re-balancing period
#******************************************************************
data$weight[] = NA
  data$weight[period.ends,] = ntop(prices[period.ends,], n)
models$ew = bt.run.share(data, clean.signal=F, silent=T)

#*****************************************************************
# Risk Parity each re-balancing period
#******************************************************************
ret = diff(log(prices))
hist.vol = sqrt(252) * bt.apply.matrix(ret, runSD, n = 20)

# risk-parity
weight = 1 / hist.vol
rp.weight = weight / rowSums(weight, na.rm=T)

data$weight[] = NA
  data$weight[period.ends,] = rp.weight[period.ends,]
models$rp = bt.run.share(data, clean.signal=F, silent=T)


#*****************************************************************
# Helper functions
#*****************************************************************
reallocate = function(allocation, data, period.ends,	lookback.len,
	prefix = '',
	min.risk.fns = 'min.var.portfolio',
	silent = F
) {
	allocation = ifna(allocation, 0)[period.ends,]

	# only allocate to selected assets
	obj = portfolio.allocation.helper(
   data$prices, 
   period.ends=period.ends,
   lookback.len = lookback.len,
   universe = allocation > 0,
   prefix = prefix,
   min.risk.fns = min.risk.fns,
   silent = silent
  )

  # rescale weights to be proportionate allocation
  for(i in names(obj$weights)) {
   weight = allocation * obj$weights[[i]]
   weight = ifna(rowSums(allocation) * weight / rowSums(weight), 0)
   weight$CASH = 1 - rowSums(weight)
   obj$weights[[i]] = weight
  }

	create.strategies(obj, data, silent = silent)$models
}



#*****************************************************************
# Strategy:
#
# 1) Use 60,120,180, 252-day percentile channels
# - corresponding to 3,6,9 and 12 months in the momentum literature- 
# (4 separate systems) with a .75 long entry and .25 exit threshold with 
# long triggered above .75 and holding through until exiting below .25 
# (just like in the previous post) - no shorts!!!
#
# 2) If the indicator shows that you should be in cash, hold SHY
#
# 3) Use 20-day historical volatility for risk parity position-sizing 
# among active assets (no leverage is used). This is 1/volatility (asset A) 
# divided by the sum of 1/volatility for all assets to determine the position size.
#******************************************************************

allocation = 0 * ifna(prices, 0)
for(lockback.len in c(60,120,180, 252)) {
  high.channel = bt.apply.matrix(data$prices, runQuantile, lockback.len, probs=0.75)
  low.channel = bt.apply.matrix(data$prices, runQuantile, lockback.len, probs=0.25)
  signal = iif(cross.up(prices, high.channel), 1, iif(cross.dn(prices, low.channel), 0, NA))
  allocation = allocation + ifna( bt.apply.matrix(signal, ifna.prev), 0) 
}

# convert to weights, i.e. 4 assets times 4 systems
allocation = allocation / (4 * 4)

# equal-weight
weight = allocation
weight$CASH = 1 - rowSums(weight)

data$weight[] = NA
  data$weight[period.ends,] = weight[period.ends,]
models$strategy.ew = bt.run.share(data, clean.signal=F, trade.summary=T, silent=T)

# risk-parity
weight = allocation * rp.weight
weight = rowSums(allocation, na.rm=T) * weight / rowSums(weight, na.rm=T)
weight$CASH = 1 - rowSums(weight, na.rm=T)

data$weight[] = NA
  data$weight[period.ends,] = ifna(weight[period.ends,], 0)
models$strategy.rp = bt.run.share(data, clean.signal=F, trade.summary=T, silent=T)


#*****************************************************************
# Alternative Way
#*****************************************************************
models = c(models, 
  reallocate(allocation, data, period.ends,	20,
   prefix = 'REP.STG.',
   min.risk.fns = list(
    EW=equal.weight.portfolio,
    RP=risk.parity.portfolio()
   ),
   silent = T
  )
)
{% endhighlight %}

Let's add another benchmark, for comparison we will use
the [Quantitative Approach To Tactical Asset Allocation Strategy(QATAA) by Mebane T. Faber](http://mebfaber.com/timing-model/)



{% highlight r %}
#*****************************************************************
#The [Quantitative Approach To Tactical Asset Allocation Strategy(QATAA) by Mebane T. Faber](http://mebfaber.com/timing-model/)
#[SSRN paper](http://papers.ssrn.com/sol3/papers.cfm?abstract_id=962461)
#******************************************************************
# compute 10 month moving average
sma = bt.apply.matrix(prices, SMA, 200)

# go to cash if prices falls below 10 month moving average
go2cash = prices < sma
  go2cash = ifna(go2cash, T)

# equal weight target allocation
target.allocation = ntop(prices,n)

# If asset is above it's 10 month moving average it gets allocation
qt.weight = iif(go2cash, 0, target.allocation)

# otherwise, it's weight is allocated to cash
weight = qt.weight
weight$CASH = 1 - rowSums(weight)

data$weight[] = NA
  data$weight[period.ends,] = weight[period.ends,]
models$QATAA.EW = bt.run.share(data, clean.signal=F, trade.summary=T, silent=T)

# same, but risk-parity
weight = qt.weight * rp.weight
weight = rowSums(qt.weight, na.rm=T) * weight / rowSums(weight, na.rm=T)
weight$CASH = 1 - rowSums(weight, na.rm=T)

data$weight[] = NA
  data$weight[period.ends,] = ifna(weight[period.ends,], 0)
models$QATAA.RP = bt.run.share(data, clean.signal=F, trade.summary=T, silent=T)

#*****************************************************************
# Alternative Way
#*****************************************************************
models = c(models, 
  reallocate(qt.weight, data, period.ends,	20,
   prefix = 'REP.QATAA.',
   min.risk.fns = list(
    EW=equal.weight.portfolio,
    RP=risk.parity.portfolio()
   ),
   silent = T
  )
)


#*****************************************************************
# Report
#*****************************************************************
#strategy.performance.snapshoot(models, T)
plotbt(models, plotX = T, log = 'y', LeftMargin = 3, main = NULL)
	mtext('Cumulative Performance', side = 2, line = 1)
{% endhighlight %}

![plot of chunk plot-4](/public/images/2015-02-02-Channel-Breakout/plot-4-1.png) 

{% highlight r %}
print(plotbt.strategy.sidebyside(models, make.plot=F, return.table=T))
{% endhighlight %}



|           |ew                |rp                |strategy.ew       |strategy.rp       |REP.STG.EW        |REP.STG.RP        |QATAA.EW          |QATAA.RP          |REP.QATAA.EW      |REP.QATAA.RP      |
|:----------|:-----------------|:-----------------|:-----------------|:-----------------|:-----------------|:-----------------|:-----------------|:-----------------|:-----------------|:-----------------|
|Period     |May1996 - Feb2015 |May1996 - Feb2015 |May1996 - Feb2015 |May1996 - Feb2015 |May1996 - Feb2015 |May1996 - Feb2015 |May1996 - Feb2015 |May1996 - Feb2015 |May1996 - Feb2015 |May1996 - Feb2015 |
|Cagr       |8.2               |7.56              |9.54              |8.36              |9.54              |8.36              |10.22             |9.07              |10.22             |9.07              |
|Sharpe     |0.68              |0.84              |1.38              |1.56              |1.38              |1.56              |1.36              |1.54              |1.36              |1.54              |
|DVR        |0.63              |0.78              |1.33              |1.53              |1.33              |1.53              |1.32              |1.51              |1.32              |1.51              |
|Volatility |12.74             |9.18              |6.81              |5.26              |6.81              |5.26              |7.38              |5.76              |7.38              |5.76              |
|MaxDD      |-47.38            |-40.01            |-11.53            |-7.98             |-11.53            |-7.99             |-12.47            |-8.77             |-12.47            |-8.78             |
|AvgDD      |-1.43             |-1.18             |-0.93             |-0.82             |-0.93             |-0.82             |-1.01             |-0.85             |-1.01             |-0.85             |
|VaR        |-1.07             |-0.76             |-0.64             |-0.5              |-0.64             |-0.5              |-0.7              |-0.56             |-0.7              |-0.56             |
|CVaR       |-1.93             |-1.34             |-0.99             |-0.76             |-0.99             |-0.76             |-1.08             |-0.83             |-1.08             |-0.83             |
|Exposure   |99.7              |99.28             |99.7              |99.7              |99.7              |99.7              |99.7              |99.7              |99.7              |99.7              |
    




{% highlight r %}
for(m in names(models)) {
  print('#', m)
  plotbt.transition.map(models[[m]]$weight, name=m)
    legend('topright', legend = m, bty = 'n')
  
  print('Last Trades:')  
  print(last.trades(models[m],make.plot=F, return.table=T))
                                              
  print('Current Allocation:')
  print(to.percent(last(models[[m]]$weight)))
}
{% endhighlight %}



# ew
    


![plot of chunk plot-4](/public/images/2015-02-02-Channel-Breakout/plot-4-2.png) 

Last Trades:
    

    




Current Allocation:
    




|           |EQ     |RE     |CORP.FI |COM    |CASH   |
|:----------|:------|:------|:-------|:------|:------|
|2015-02-04 |25.26% |24.75% |24.36%  |25.63% | 0.00% |
    




# rp
    


![plot of chunk plot-4](/public/images/2015-02-02-Channel-Breakout/plot-4-3.png) 

Last Trades:
    

    




Current Allocation:
    




|           |EQ     |RE     |CORP.FI |COM    |CASH   |
|:----------|:------|:------|:-------|:------|:------|
|2015-02-04 |16.20% |20.48% |48.50%  |14.81% | 0.00% |
    




# strategy.ew
    


![plot of chunk plot-4](/public/images/2015-02-02-Channel-Breakout/plot-4-4.png) 

Last Trades:
    




|strategy.ew |weight |entry.date |exit.date  |nhold |entry.price |exit.price |return |
|:-----------|:------|:----------|:----------|:-----|:-----------|:----------|:------|
|EQ          | 18.8  |2014-09-30 |2014-10-31 |31    |100.71      |103.47     | 0.51  |
|RE          | 18.8  |2014-09-30 |2014-10-31 |31    | 68.49      | 74.21     | 1.57  |
|CORP.FI     | 18.8  |2014-09-30 |2014-10-31 |31    |116.91      |118.35     | 0.23  |
|CASH        | 43.8  |2014-09-30 |2014-10-31 |31    | 84.42      | 84.64     | 0.11  |
|EQ          | 25.0  |2014-10-31 |2014-11-28 |28    |103.47      |106.04     | 0.62  |
|RE          | 25.0  |2014-10-31 |2014-11-28 |28    | 74.21      | 76.23     | 0.68  |
|CORP.FI     | 25.0  |2014-10-31 |2014-11-28 |28    |118.35      |119.43     | 0.23  |
|CASH        | 25.0  |2014-10-31 |2014-11-28 |28    | 84.64      | 84.73     | 0.03  |
|EQ          | 25.0  |2014-11-28 |2014-12-31 |33    |106.04      |106.00     |-0.01  |
|RE          | 25.0  |2014-11-28 |2014-12-31 |33    | 76.23      | 76.84     | 0.20  |
|CORP.FI     | 25.0  |2014-11-28 |2014-12-31 |33    |119.43      |119.41     | 0.00  |
|CASH        | 25.0  |2014-11-28 |2014-12-31 |33    | 84.73      | 84.45     |-0.08  |
|EQ          | 25.0  |2014-12-31 |2015-01-30 |30    |106.00      |103.10     |-0.68  |
|RE          | 25.0  |2014-12-31 |2015-01-30 |30    | 76.84      | 81.23     | 1.43  |
|CORP.FI     | 25.0  |2014-12-31 |2015-01-30 |30    |119.41      |123.89     | 0.94  |
|CASH        | 25.0  |2014-12-31 |2015-01-30 |30    | 84.45      | 84.98     | 0.16  |
|EQ          | 18.8  |2015-01-30 |2015-02-04 | 5    |103.10      |105.49     | 0.43  |
|RE          | 25.0  |2015-01-30 |2015-02-04 | 5    | 81.23      | 81.48     | 0.08  |
|CORP.FI     | 25.0  |2015-01-30 |2015-02-04 | 5    |123.89      |122.84     |-0.21  |
|CASH        | 31.2  |2015-01-30 |2015-02-04 | 5    | 84.98      | 84.86     |-0.04  |
    




Current Allocation:
    




|           |EQ     |RE     |CORP.FI |COM    |CASH   |
|:----------|:------|:------|:-------|:------|:------|
|2015-02-04 |19.18% |25.06% |24.67%  | 0.00% |31.09% |
    




# strategy.rp
    


![plot of chunk plot-4](/public/images/2015-02-02-Channel-Breakout/plot-4-5.png) 

Last Trades:
    




|strategy.rp |weight |entry.date |exit.date  |nhold |entry.price |exit.price |return |
|:-----------|:------|:----------|:----------|:-----|:-----------|:----------|:------|
|EQ          | 13.8  |2014-09-30 |2014-10-31 |31    |100.71      |103.47     | 0.38  |
|RE          | 10.0  |2014-09-30 |2014-10-31 |31    | 68.49      | 74.21     | 0.83  |
|CORP.FI     | 32.5  |2014-09-30 |2014-10-31 |31    |116.91      |118.35     | 0.40  |
|CASH        | 43.8  |2014-09-30 |2014-10-31 |31    | 84.42      | 84.64     | 0.11  |
|EQ          |  9.8  |2014-10-31 |2014-11-28 |28    |103.47      |106.04     | 0.24  |
|RE          | 15.8  |2014-10-31 |2014-11-28 |28    | 74.21      | 76.23     | 0.43  |
|CORP.FI     | 49.4  |2014-10-31 |2014-11-28 |28    |118.35      |119.43     | 0.45  |
|CASH        | 25.0  |2014-10-31 |2014-11-28 |28    | 84.64      | 84.73     | 0.03  |
|EQ          | 25.6  |2014-11-28 |2014-12-31 |33    |106.04      |106.00     |-0.01  |
|RE          | 20.2  |2014-11-28 |2014-12-31 |33    | 76.23      | 76.84     | 0.16  |
|CORP.FI     | 29.2  |2014-11-28 |2014-12-31 |33    |119.43      |119.41     | 0.00  |
|CASH        | 25.0  |2014-11-28 |2014-12-31 |33    | 84.73      | 84.45     |-0.08  |
|EQ          | 12.7  |2014-12-31 |2015-01-30 |30    |106.00      |103.10     |-0.35  |
|RE          | 14.5  |2014-12-31 |2015-01-30 |30    | 76.84      | 81.23     | 0.83  |
|CORP.FI     | 47.8  |2014-12-31 |2015-01-30 |30    |119.41      |123.89     | 1.79  |
|CASH        | 25.0  |2014-12-31 |2015-01-30 |30    | 84.45      | 84.98     | 0.16  |
|EQ          | 10.0  |2015-01-30 |2015-02-04 | 5    |103.10      |105.49     | 0.23  |
|RE          | 17.2  |2015-01-30 |2015-02-04 | 5    | 81.23      | 81.48     | 0.05  |
|CORP.FI     | 41.5  |2015-01-30 |2015-02-04 | 5    |123.89      |122.84     |-0.35  |
|CASH        | 31.2  |2015-01-30 |2015-02-04 | 5    | 84.98      | 84.86     |-0.04  |
    




Current Allocation:
    




|           |EQ     |RE     |CORP.FI |COM    |CASH   |
|:----------|:------|:------|:-------|:------|:------|
|2015-02-04 |10.30% |17.36% |41.11%  | 0.00% |31.23% |
    




# REP.STG.EW
    


![plot of chunk plot-4](/public/images/2015-02-02-Channel-Breakout/plot-4-6.png) 

Last Trades:
    

    




Current Allocation:
    




|           |EQ     |RE     |CORP.FI |COM    |CASH   |
|:----------|:------|:------|:-------|:------|:------|
|2015-02-04 |19.18% |25.06% |24.67%  | 0.00% |31.09% |
    




# REP.STG.RP
    


![plot of chunk plot-4](/public/images/2015-02-02-Channel-Breakout/plot-4-7.png) 

Last Trades:
    

    




Current Allocation:
    




|           |EQ     |RE     |CORP.FI |COM    |CASH   |
|:----------|:------|:------|:-------|:------|:------|
|2015-02-04 |10.31% |17.35% |41.11%  | 0.00% |31.23% |
    




# QATAA.EW
    


![plot of chunk plot-4](/public/images/2015-02-02-Channel-Breakout/plot-4-8.png) 

Last Trades:
    




|QATAA.EW |weight |entry.date |exit.date  |nhold |entry.price |exit.price |return |
|:--------|:------|:----------|:----------|:-----|:-----------|:----------|:------|
|EQ       | 25    |2014-09-30 |2014-10-31 |31    |100.71      |103.47     | 0.69  |
|RE       | 25    |2014-09-30 |2014-10-31 |31    | 68.49      | 74.21     | 2.09  |
|CORP.FI  | 25    |2014-09-30 |2014-10-31 |31    |116.91      |118.35     | 0.31  |
|CASH     | 25    |2014-09-30 |2014-10-31 |31    | 84.42      | 84.64     | 0.07  |
|EQ       | 25    |2014-10-31 |2014-11-28 |28    |103.47      |106.04     | 0.62  |
|RE       | 25    |2014-10-31 |2014-11-28 |28    | 74.21      | 76.23     | 0.68  |
|CORP.FI  | 25    |2014-10-31 |2014-11-28 |28    |118.35      |119.43     | 0.23  |
|CASH     | 25    |2014-10-31 |2014-11-28 |28    | 84.64      | 84.73     | 0.03  |
|EQ       | 25    |2014-11-28 |2014-12-31 |33    |106.04      |106.00     |-0.01  |
|RE       | 25    |2014-11-28 |2014-12-31 |33    | 76.23      | 76.84     | 0.20  |
|CORP.FI  | 25    |2014-11-28 |2014-12-31 |33    |119.43      |119.41     | 0.00  |
|CASH     | 25    |2014-11-28 |2014-12-31 |33    | 84.73      | 84.45     |-0.08  |
|EQ       | 25    |2014-12-31 |2015-01-30 |30    |106.00      |103.10     |-0.68  |
|RE       | 25    |2014-12-31 |2015-01-30 |30    | 76.84      | 81.23     | 1.43  |
|CORP.FI  | 25    |2014-12-31 |2015-01-30 |30    |119.41      |123.89     | 0.94  |
|CASH     | 25    |2014-12-31 |2015-01-30 |30    | 84.45      | 84.98     | 0.16  |
|EQ       | 25    |2015-01-30 |2015-02-04 | 5    |103.10      |105.49     | 0.58  |
|RE       | 25    |2015-01-30 |2015-02-04 | 5    | 81.23      | 81.48     | 0.08  |
|CORP.FI  | 25    |2015-01-30 |2015-02-04 | 5    |123.89      |122.84     |-0.21  |
|CASH     | 25    |2015-01-30 |2015-02-04 | 5    | 84.98      | 84.86     |-0.04  |
    




Current Allocation:
    




|           |EQ     |RE     |CORP.FI |COM    |CASH   |
|:----------|:------|:------|:-------|:------|:------|
|2015-02-04 |25.53% |25.02% |24.62%  | 0.00% |24.83% |
    




# QATAA.RP
    


![plot of chunk plot-4](/public/images/2015-02-02-Channel-Breakout/plot-4-9.png) 

Last Trades:
    




|QATAA.RP |weight |entry.date |exit.date  |nhold |entry.price |exit.price |return |
|:--------|:------|:----------|:----------|:-----|:-----------|:----------|:------|
|EQ       | 18.4  |2014-09-30 |2014-10-31 |31    |100.71      |103.47     | 0.50  |
|RE       | 13.3  |2014-09-30 |2014-10-31 |31    | 68.49      | 74.21     | 1.11  |
|CORP.FI  | 43.3  |2014-09-30 |2014-10-31 |31    |116.91      |118.35     | 0.53  |
|CASH     | 25.0  |2014-09-30 |2014-10-31 |31    | 84.42      | 84.64     | 0.07  |
|EQ       |  9.8  |2014-10-31 |2014-11-28 |28    |103.47      |106.04     | 0.24  |
|RE       | 15.8  |2014-10-31 |2014-11-28 |28    | 74.21      | 76.23     | 0.43  |
|CORP.FI  | 49.4  |2014-10-31 |2014-11-28 |28    |118.35      |119.43     | 0.45  |
|CASH     | 25.0  |2014-10-31 |2014-11-28 |28    | 84.64      | 84.73     | 0.03  |
|EQ       | 25.6  |2014-11-28 |2014-12-31 |33    |106.04      |106.00     |-0.01  |
|RE       | 20.2  |2014-11-28 |2014-12-31 |33    | 76.23      | 76.84     | 0.16  |
|CORP.FI  | 29.2  |2014-11-28 |2014-12-31 |33    |119.43      |119.41     | 0.00  |
|CASH     | 25.0  |2014-11-28 |2014-12-31 |33    | 84.73      | 84.45     |-0.08  |
|EQ       | 12.7  |2014-12-31 |2015-01-30 |30    |106.00      |103.10     |-0.35  |
|RE       | 14.5  |2014-12-31 |2015-01-30 |30    | 76.84      | 81.23     | 0.83  |
|CORP.FI  | 47.8  |2014-12-31 |2015-01-30 |30    |119.41      |123.89     | 1.79  |
|CASH     | 25.0  |2014-12-31 |2015-01-30 |30    | 84.45      | 84.98     | 0.16  |
|EQ       | 13.9  |2015-01-30 |2015-02-04 | 5    |103.10      |105.49     | 0.32  |
|RE       | 17.9  |2015-01-30 |2015-02-04 | 5    | 81.23      | 81.48     | 0.06  |
|CORP.FI  | 43.2  |2015-01-30 |2015-02-04 | 5    |123.89      |122.84     |-0.37  |
|CASH     | 25.0  |2015-01-30 |2015-02-04 | 5    | 84.98      | 84.86     |-0.04  |
    




Current Allocation:
    




|           |EQ     |RE     |CORP.FI |COM    |CASH   |
|:----------|:------|:------|:-------|:------|:------|
|2015-02-04 |14.27% |18.04% |42.72%  | 0.00% |24.96% |
    




# REP.QATAA.EW
    


![plot of chunk plot-4](/public/images/2015-02-02-Channel-Breakout/plot-4-10.png) 

Last Trades:
    

    




Current Allocation:
    




|           |EQ     |RE     |CORP.FI |COM    |CASH   |
|:----------|:------|:------|:-------|:------|:------|
|2015-02-04 |25.53% |25.02% |24.62%  | 0.00% |24.83% |
    




# REP.QATAA.RP
    


![plot of chunk plot-4](/public/images/2015-02-02-Channel-Breakout/plot-4-11.png) 

Last Trades:
    

    




Current Allocation:
    




|           |EQ     |RE     |CORP.FI |COM    |CASH   |
|:----------|:------|:------|:-------|:------|:------|
|2015-02-04 |14.29% |18.04% |42.72%  | 0.00% |24.96% |
    

Unfortunately we cannot match the numbers from original [source](https://cssanalytics.wordpress.com/2015/01/26/a-simple-tactical-asset-allocation-portfolio-with-percentile-channels/)

But overall, this concept is a very robust allocation framework.


ToDo: take the posts about Permanent Portfolio and apply here
systematicinvestor.wordpress.com/?s=Permanent+Portfolio










*(this report was produced on: 2015-02-05)*
