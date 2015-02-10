---
layout: post
title: Channel Breakout - Second Attempt
comments: true
---


To install [Systematic Investor Toolbox (SIT)](https://github.com/systematicinvestor/SIT) please visit [About](',base.url,'about) page.





David Varadi clarified the Channel Breakout system in the [A 'Simple' Tactical Asset Allocation Portfolio with Percentile Channels (for Dummies)](https://cssanalytics.wordpress.com/2015/02/08/a-simple-tactical-asset-allocation-portfolio-with-percentile-channels-for-dummies/)
post.

Below I will try to adapt a code from the post:



{% highlight r %}
#*****************************************************************
# Load historical data
#*****************************************************************
library(SIT)
load.packages('quantmod')

# load saved Proxies Raw Data, data.proxy.raw, to extend DBC and SHY
# please see http://systematicinvestor.github.io/Data-Proxy/ for more details
load('data/data.proxy.raw.Rdata')

tickers = '
LQD + VWESX
DBC + CRB
VTI +VTSMX # (or SPY)
ICF + VGSIX # (or IYR)
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

![plot of chunk plot-2](/public/images/2015-02-09-Channel-Breakout2/plot-2-1.png) 

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
hist.vol = bt.apply.matrix(ret, runSD, n = 20)

# risk-parity
weight = 1 / hist.vol
rp.weight = weight / rowSums(weight, na.rm=T)

data$weight[] = NA
  data$weight[period.ends,] = rp.weight[period.ends,]
models$rp = bt.run.share(data, clean.signal=F, silent=T)

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
  signal = iif(cross.up(prices, high.channel), 1, iif(cross.dn(prices, low.channel), -1, NA))
  allocation = allocation + ifna( bt.apply.matrix(signal, ifna.prev), 0) 
}

# (A) Channel score
allocation = ifna(allocation / 4, 0)

# equal-weight
weight = abs(allocation) / rowSums(abs(allocation))
	weight[allocation < 0] = 0
weight$CASH = 1 - rowSums(weight, na.rm=T)

data$weight[] = NA
  data$weight[period.ends,] = weight[period.ends,]
models$channel.ew = bt.run.share(data, clean.signal=F, trade.summary=T, silent=T)


# risk-parity: (C)
weight = allocation * 1 / hist.vol
weight = abs(weight) / rowSums(abs(weight), na.rm=T)
	weight[allocation < 0] = 0
weight$CASH = 1 - rowSums(weight, na.rm=T)

data$weight[] = NA
  data$weight[period.ends,] = ifna(weight[period.ends,], 0)
models$channel.rp = bt.run.share(data, clean.signal=F, trade.summary=T, silent=T)

# let's verify
last.period = last(period.ends)
print(allocation[last.period,])
{% endhighlight %}



|           | LQD| DBC| VTI| ICF| CASH|
|:----------|---:|---:|---:|---:|----:|
|2015-02-09 |   1|  -1|   1|   1|    0|
    




{% highlight r %}
print(to.percent(last(hist.vol[last.period,])))
{% endhighlight %}



|           |LQD   |DBC   |VTI   |ICF   |CASH |
|:----------|:-----|:-----|:-----|:-----|:----|
|2015-02-09 |0.47% |1.45% |0.97% |1.10% |     |
    




{% highlight r %}
print(to.percent(last(weight[last.period,])))
{% endhighlight %}



|           |LQD    |DBC    |VTI    |ICF    |CASH   |
|:----------|:------|:------|:------|:------|:------|
|2015-02-09 |44.79% | 0.00% |21.68% |19.07% |14.46% |
    

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
weight = iif(go2cash, 0, target.allocation)

# otherwise, it's weight is allocated to cash
weight$CASH = 1 - rowSums(weight)

data$weight[] = NA
  data$weight[period.ends,] = weight[period.ends,]
models$QATAA = bt.run.share(data, clean.signal=F, trade.summary=T, silent=T)


#*****************************************************************
# Report
#*****************************************************************
#strategy.performance.snapshoot(models, T)
plotbt(models, plotX = T, log = 'y', LeftMargin = 3, main = NULL)
	mtext('Cumulative Performance', side = 2, line = 1)
{% endhighlight %}

![plot of chunk plot-3](/public/images/2015-02-09-Channel-Breakout2/plot-3-1.png) 

{% highlight r %}
print(plotbt.strategy.sidebyside(models, make.plot=F, return.table=T))
{% endhighlight %}



|           |ew                |rp                |channel.ew        |channel.rp        |QATAA             |
|:----------|:-----------------|:-----------------|:-----------------|:-----------------|:-----------------|
|Period     |May1996 - Feb2015 |May1996 - Feb2015 |May1996 - Feb2015 |May1996 - Feb2015 |May1996 - Feb2015 |
|Cagr       |8.46              |7.68              |11.1              |9.94              |10.04             |
|Sharpe     |0.69              |0.85              |1.4               |1.54              |1.29              |
|DVR        |0.63              |0.79              |1.32              |1.46              |1.25              |
|Volatility |13.15             |9.2               |7.77              |6.29              |7.65              |
|MaxDD      |-48.55            |-40.24            |-11.5             |-6.87             |-13.54            |
|AvgDD      |-1.51             |-1.17             |-1.21             |-1.06             |-1.04             |
|VaR        |-1.07             |-0.76             |-0.74             |-0.59             |-0.71             |
|CVaR       |-2                |-1.34             |-1.13             |-0.9              |-1.13             |
|Exposure   |99.7              |99.28             |99.7              |99.7              |99.7              |
    




{% highlight r %}
for(m in names(models)) {
  print('#', m, 'strategy:')
  plotbt.transition.map(models[[m]]$weight, name=m)
    legend('topright', legend = m, bty = 'n')
                
  print(plotbt.monthly.table(models[[m]]$equity, make.plot = F))
   
  print(to.percent(last(models[[m]]$weight)))
}
{% endhighlight %}



# ew strategy:
    


![plot of chunk plot-3](/public/images/2015-02-09-Channel-Breakout2/plot-3-2.png) 

|     |Jan   |Feb   |Mar   |Apr   |May   |Jun   |Jul   |Aug   |Sep   |Oct   |Nov   |Dec   |Year  |MaxDD |
|:----|:-----|:-----|:-----|:-----|:-----|:-----|:-----|:-----|:-----|:-----|:-----|:-----|:-----|:-----|
|1996 |      |      |      |      |      |  1.3 | -1.6 |  2.8 |  3.0 |  1.7 |  5.1 |  2.6 | 15.7 | -3.2 |
|1997 |  1.4 | -0.3 | -0.9 |  1.2 |  3.4 |  1.8 |  4.8 | -1.5 |  4.9 | -0.7 |  0.2 |  0.3 | 15.2 | -4.9 |
|1998 |  0.0 |  0.1 |  2.0 | -1.0 | -1.3 |  0.5 | -3.7 | -7.4 |  6.0 | -0.1 |  0.3 |  0.9 | -4.1 |-13.4 |
|1999 |  0.5 | -3.3 |  4.3 |  4.2 | -1.4 |  2.3 | -1.4 |  0.9 | -0.3 |  0.2 |  1.2 |  3.3 | 10.7 | -4.8 |
|2000 |  0.5 |  1.3 |  2.9 | -0.2 |  1.2 |  3.4 |  1.1 |  3.5 | -0.5 | -1.7 | -0.3 |  2.4 | 14.5 | -4.6 |
|2001 |  1.8 | -2.8 | -2.8 |  3.3 |  0.0 |  0.2 | -0.1 |  0.2 | -5.4 | -0.4 |  3.1 | -0.1 | -3.5 |-11.1 |
|2002 | -0.3 |  1.2 |  4.3 | -0.4 |  0.4 | -0.6 | -3.1 |  2.3 | -2.4 |  0.5 |  3.2 |  1.4 |  6.4 | -9.9 |
|2003 |  0.4 |  2.1 | -1.2 |  3.2 |  4.8 |  0.9 |  1.1 |  2.3 |  1.5 |  2.3 |  1.7 |  4.1 | 25.5 | -3.4 |
|2004 |  2.7 |  3.1 |  1.9 | -5.4 |  3.2 | -0.1 |  0.8 |  2.8 |  2.0 |  2.6 |  2.0 |  1.5 | 18.2 | -7.2 |
|2005 | -2.0 |  2.9 | -0.2 |  0.0 |  1.6 |  2.4 |  4.0 |  0.6 |  0.2 | -2.7 |  2.4 |  1.8 | 11.4 | -4.3 |
|2006 |  3.8 | -0.5 |  2.2 |  1.0 | -1.7 |  1.1 |  2.0 |  1.2 |  0.3 |  3.0 |  4.0 | -1.1 | 16.3 | -5.3 |
|2007 |  2.4 |  0.7 | -0.6 |  1.7 |  0.1 | -3.2 | -2.4 |  2.1 |  4.8 |  3.0 | -3.5 | -0.1 |  4.9 | -7.6 |
|2008 | -0.4 |  1.0 |  1.3 |  4.7 |  1.6 | -2.5 | -1.8 | -0.7 | -7.9 |-19.3 |-10.2 |  6.8 |-26.6 |-44.6 |
|2009 | -8.1 |-11.3 |  4.0 | 11.5 |  6.9 | -0.9 |  6.2 |  4.0 |  3.3 | -0.3 |  4.9 |  1.8 | 21.7 |-24.8 |
|2010 | -4.1 |  3.4 |  4.3 |  3.8 | -6.0 | -2.4 |  6.4 | -1.5 |  5.6 |  3.1 | -0.9 |  5.2 | 17.4 |-10.8 |
|2011 |  2.5 |  3.4 |  0.3 |  4.0 | -0.8 | -2.6 |  1.6 | -2.9 | -8.3 |  9.1 | -2.0 |  1.5 |  4.9 |-14.7 |
|2012 |  4.2 |  2.5 |  1.2 |  0.5 | -5.3 |  3.1 |  3.1 |  2.0 |  0.3 | -1.4 |  0.4 |  0.9 | 11.6 | -7.5 |
|2013 |  2.4 | -0.5 |  1.8 |  1.8 | -2.2 | -2.3 |  2.6 | -2.0 |  1.0 |  2.4 | -1.0 |  1.0 |  4.8 | -7.9 |
|2014 |  0.1 |  4.0 |  0.3 |  1.6 |  1.2 |  1.3 | -1.6 |  2.0 | -4.2 |  2.7 | -0.6 | -2.0 |  4.6 | -5.4 |
|2015 |  0.6 |  0.8 |      |      |      |      |      |      |      |      |      |      |  1.4 | -1.0 |
|Avg  |  0.4 |  0.4 |  1.4 |  2.0 |  0.3 |  0.2 |  0.9 |  0.6 |  0.2 |  0.2 |  0.5 |  1.7 |  8.6 | -9.8 |
    




|           |LQD    |DBC    |VTI    |ICF    |CASH   |
|:----------|:------|:------|:------|:------|:------|
|2015-02-09 |24.27% |25.70% |25.59% |24.44% | 0.00% |
    




# rp strategy:
    


![plot of chunk plot-3](/public/images/2015-02-09-Channel-Breakout2/plot-3-3.png) 

|     |Jan   |Feb   |Mar   |Apr   |May   |Jun   |Jul   |Aug   |Sep   |Oct   |Nov   |Dec   |Year  |MaxDD |
|:----|:-----|:-----|:-----|:-----|:-----|:-----|:-----|:-----|:-----|:-----|:-----|:-----|:-----|:-----|
|1996 |      |      |      |      |      |  0.0 | -1.4 |  3.1 |  3.0 |  2.0 |  5.0 |  3.1 | 15.6 | -3.2 |
|1997 |  1.1 | -0.3 | -0.6 |  1.0 |  3.0 |  2.2 |  4.7 | -1.4 |  5.4 | -0.6 | -0.4 |  0.2 | 14.9 | -4.7 |
|1998 |  0.0 | -0.7 |  2.1 | -0.8 | -1.0 |  0.7 | -3.2 | -5.0 |  4.9 | -1.4 | -1.1 |  0.6 | -5.2 |-10.1 |
|1999 |  0.3 | -3.1 |  3.2 |  3.9 | -1.5 |  1.6 | -1.6 |  0.4 | -0.2 | -0.5 |  0.3 |  2.4 |  5.1 | -4.5 |
|2000 |  0.4 |  1.1 |  2.6 |  1.0 |  1.2 |  3.1 |  1.2 |  2.2 | -0.2 | -1.2 |  1.2 |  3.2 | 17.1 | -3.5 |
|2001 |  1.9 | -1.6 | -1.8 |  1.7 | -0.2 |  1.2 |  0.3 |  1.2 | -4.5 |  0.5 |  2.3 |  0.3 |  1.1 | -7.9 |
|2002 | -0.1 |  1.4 |  2.8 |  0.3 |  0.7 |  0.2 | -2.5 |  3.2 | -0.2 | -0.9 |  2.0 |  2.5 |  9.5 | -7.3 |
|2003 | -0.4 |  2.3 | -0.4 |  2.6 |  4.5 |  0.6 | -0.1 |  2.3 |  1.9 |  1.5 |  1.6 |  3.7 | 21.7 | -2.6 |
|2004 |  2.8 |  2.4 |  1.5 | -5.5 |  1.9 |  0.0 |  0.5 |  2.5 |  1.5 |  2.3 |  1.4 |  1.5 | 13.3 | -7.3 |
|2005 | -1.6 |  1.5 | -0.6 | -0.1 |  1.5 |  1.9 |  3.1 |  0.7 | -0.6 | -2.1 |  1.5 |  1.6 |  6.9 | -3.6 |
|2006 |  2.7 | -0.1 |  0.3 |  0.6 | -1.3 |  0.5 |  1.8 |  1.7 |  0.8 |  2.4 |  3.1 | -0.7 | 12.5 | -3.3 |
|2007 |  1.4 |  0.6 | -0.5 |  1.4 |  0.0 | -1.5 | -1.6 |  1.8 |  4.1 |  2.7 | -2.1 |  0.2 |  6.4 | -4.5 |
|2008 |  0.4 |  1.2 |  0.2 |  3.3 |  0.7 | -3.1 | -0.8 | -0.5 | -9.5 |-19.0 | -6.3 |  9.8 |-23.7 |-39.8 |
|2009 | -4.6 | -7.8 |  2.6 |  5.6 |  5.2 |  0.9 |  5.5 |  2.5 |  2.5 | -0.3 |  3.8 | -0.3 | 15.8 |-17.2 |
|2010 | -2.5 |  2.1 |  2.9 |  2.9 | -4.5 |  0.4 |  4.1 |  0.7 |  4.2 |  2.3 | -1.1 |  3.6 | 15.8 | -6.2 |
|2011 |  2.2 |  2.7 |  0.0 |  3.4 |  0.1 | -1.6 |  1.9 | -1.8 | -5.9 |  5.6 | -2.4 |  2.2 |  6.0 | -9.2 |
|2012 |  3.5 |  2.3 |  0.3 |  0.6 | -3.4 |  2.2 |  3.2 |  0.9 |  0.4 | -0.5 | -0.1 |  0.4 | 10.0 | -4.8 |
|2013 |  1.3 |  0.0 |  1.0 |  2.1 | -2.7 | -2.5 |  2.6 | -1.2 |  0.8 |  2.4 | -0.6 |  0.9 |  4.0 | -7.1 |
|2014 |  0.1 |  3.3 |  0.2 |  1.4 |  1.1 |  1.1 | -1.5 |  1.8 | -3.3 |  1.7 |  0.0 | -0.6 |  5.1 | -4.2 |
|2015 |  1.9 | -0.4 |      |      |      |      |      |      |      |      |      |      |  1.6 | -1.1 |
|Avg  |  0.6 |  0.4 |  0.9 |  1.4 |  0.3 |  0.4 |  0.9 |  0.8 |  0.3 | -0.2 |  0.4 |  1.8 |  7.7 | -7.6 |
    




|           |LQD    |DBC    |VTI    |ICF    |CASH   |
|:----------|:------|:------|:------|:------|:------|
|2015-02-09 |49.40% |15.18% |16.78% |18.63% | 0.00% |
    




# channel.ew strategy:
    


![plot of chunk plot-3](/public/images/2015-02-09-Channel-Breakout2/plot-3-4.png) 

|     |Jan   |Feb   |Mar   |Apr   |May   |Jun   |Jul   |Aug   |Sep   |Oct   |Nov   |Dec   |Year  |MaxDD |
|:----|:-----|:-----|:-----|:-----|:-----|:-----|:-----|:-----|:-----|:-----|:-----|:-----|:-----|:-----|
|1996 |      |      |      |      |      |  0.9 |  0.3 |  0.1 |  1.6 |  0.5 |  6.2 |  1.2 | 11.1 | -2.3 |
|1997 |  1.6 | -0.6 | -1.4 |  0.5 |  1.8 |  0.9 |  6.2 | -2.0 |  3.7 |  0.1 | -0.8 |  0.4 | 10.5 | -4.8 |
|1998 |  0.6 |  1.5 |  1.9 |  0.1 |  0.1 |  1.4 | -0.5 | -2.5 |  2.4 | -0.4 |  0.4 |  1.9 |  6.9 | -4.1 |
|1999 |  1.4 | -2.5 |  1.8 |  2.4 | -1.5 |  2.7 | -1.1 |  1.0 |  0.7 | -1.0 |  2.0 |  2.9 |  8.8 | -4.2 |
|2000 |  0.4 |  1.9 |  1.9 | -2.7 |  3.7 |  3.6 | -1.5 |  4.1 | -1.7 | -0.4 |  2.5 |  1.2 | 13.5 | -6.8 |
|2001 |  1.5 |  0.1 | -0.5 | -0.2 |  0.7 |  2.7 |  1.2 |  2.1 | -0.1 |  1.5 | -1.4 | -0.1 |  7.8 | -3.5 |
|2002 |  0.2 |  1.1 |  0.3 | -0.2 |  1.1 |  1.3 | -0.4 |  2.3 |  1.7 | -0.2 |  0.0 |  3.8 | 11.4 | -4.0 |
|2003 |  3.0 |  2.3 | -2.5 |  1.3 |  4.7 |  0.9 |  1.1 |  2.4 |  0.5 |  2.3 |  1.7 |  4.1 | 23.9 | -4.7 |
|2004 |  2.7 |  3.1 |  1.9 | -5.8 |  2.4 | -2.0 |  0.0 |  3.4 |  1.7 |  2.6 |  2.0 |  1.5 | 13.9 | -6.8 |
|2005 | -2.7 |  2.9 |  0.0 | -3.4 |  1.3 |  2.3 |  4.0 |  0.6 |  0.2 | -3.1 |  0.2 |  1.3 |  3.2 | -5.8 |
|2006 |  4.8 | -0.6 |  2.1 |  1.1 | -1.6 |  0.8 |  2.1 |  0.2 |  0.9 |  3.3 |  2.8 | -1.1 | 15.7 | -5.2 |
|2007 |  2.4 | -0.8 | -0.8 |  1.4 |  0.4 | -0.4 | -0.1 |  0.1 |  0.6 |  3.3 | -1.4 |  1.5 |  6.4 | -5.8 |
|2008 |  2.2 |  3.0 | -0.3 |  1.8 |  1.4 |  3.3 | -3.0 | -0.5 |  0.5 |  1.1 |  1.1 |  0.6 | 11.6 | -5.4 |
|2009 | -0.8 | -1.4 |  0.5 | -0.2 |  0.1 |  2.0 |  3.8 |  5.0 |  4.5 | -2.3 |  4.9 |  1.8 | 19.1 | -5.4 |
|2010 | -4.1 |  3.3 |  4.8 |  3.2 | -6.0 |  0.4 |  2.2 |  0.6 |  1.3 |  3.1 | -0.9 |  6.3 | 14.6 | -9.6 |
|2011 |  3.3 |  4.2 |  0.4 |  4.2 | -0.8 | -2.3 |  2.1 | -2.4 |  0.0 |  0.6 | -1.2 |  0.6 |  8.7 |-11.5 |
|2012 |  2.1 |  1.3 |  1.7 |  0.7 | -3.3 |  1.8 |  2.0 |  0.7 |  0.3 | -1.4 |  0.4 | -0.1 |  6.1 | -5.7 |
|2013 |  3.1 | -1.0 |  2.1 |  2.8 | -1.8 | -0.9 |  1.5 | -1.1 |  1.3 |  1.3 |  0.0 |  0.8 |  8.2 | -6.3 |
|2014 | -0.5 |  2.0 |  0.4 |  1.6 |  1.2 |  1.3 | -1.6 |  2.3 | -2.4 |  1.1 |  1.5 |  0.4 |  7.3 | -3.1 |
|2015 |  2.2 | -0.9 |      |      |      |      |      |      |      |      |      |      |  1.3 | -1.9 |
|Avg  |  1.2 |  1.0 |  0.8 |  0.5 |  0.2 |  1.1 |  1.0 |  0.9 |  0.9 |  0.6 |  1.0 |  1.5 | 10.5 | -5.4 |
    




|           |LQD    |DBC    |VTI    |ICF    |CASH   |
|:----------|:------|:------|:------|:------|:------|
|2015-02-09 |28.15% | 0.00% |14.84% |28.35% |28.66% |
    




# channel.rp strategy:
    


![plot of chunk plot-3](/public/images/2015-02-09-Channel-Breakout2/plot-3-5.png) 

|     |Jan  |Feb  |Mar  |Apr  |May  |Jun  |Jul  |Aug  |Sep  |Oct  |Nov  |Dec  |Year |MaxDD |
|:----|:----|:----|:----|:----|:----|:----|:----|:----|:----|:----|:----|:----|:----|:-----|
|1996 |     |     |     |     |     | 0.9 | 0.3 | 0.1 | 1.6 | 0.7 | 6.4 | 0.9 |11.2 |-2.2  |
|1997 | 1.3 |-0.5 |-1.3 | 0.4 | 1.8 | 1.1 | 5.8 |-1.9 | 3.8 | 0.3 |-1.2 | 0.5 |10.3 |-4.6  |
|1998 | 0.6 | 0.6 | 2.0 |-0.1 | 0.4 | 1.3 |-0.6 |-0.9 | 2.8 |-1.0 | 0.6 | 1.5 | 7.4 |-3.9  |
|1999 | 1.2 |-2.5 | 1.3 | 1.4 |-1.5 | 2.0 |-1.2 | 0.6 | 0.7 |-0.8 | 1.2 | 1.9 | 4.3 |-4.1  |
|2000 | 0.4 | 1.6 | 1.6 |-2.1 | 2.1 | 3.6 |-0.7 | 2.6 |-1.2 |-0.1 | 2.0 | 1.4 |11.8 |-4.6  |
|2001 | 2.0 | 0.0 |-0.3 |-0.5 | 0.8 | 3.6 | 1.3 | 2.3 |-0.2 | 2.2 |-1.5 | 0.2 |10.1 |-3.8  |
|2002 | 0.2 | 1.4 | 0.4 | 0.4 | 1.1 | 1.2 |-1.1 | 3.2 | 1.9 |-0.3 | 0.4 | 3.9 |13.3 |-5.0  |
|2003 | 0.9 | 2.3 |-1.1 | 1.8 | 4.4 | 0.6 |-0.1 | 2.5 | 0.8 | 1.5 | 1.6 | 3.7 |20.3 |-3.7  |
|2004 | 2.8 | 2.4 | 1.5 |-5.8 | 1.9 |-1.4 |-0.3 | 2.8 | 1.3 | 2.3 | 1.4 | 1.6 |10.5 |-6.9  |
|2005 |-1.8 | 1.3 |-0.6 |-2.4 | 1.3 | 1.8 | 3.1 | 0.7 |-0.6 |-2.4 | 0.3 | 1.1 | 1.7 |-4.7  |
|2006 | 3.8 |-0.1 | 0.1 | 0.8 |-1.1 | 0.5 | 1.5 | 0.4 | 1.1 | 2.5 | 2.7 |-0.7 |11.8 |-2.6  |
|2007 | 1.4 |-0.4 |-0.7 | 1.2 | 0.1 |-0.7 | 0.1 | 0.2 | 0.7 | 2.8 |-0.7 | 1.0 | 5.0 |-3.4  |
|2008 | 2.4 | 2.1 |-0.4 | 1.5 | 0.3 | 2.3 |-1.5 |-0.2 | 0.5 | 1.1 | 1.1 | 0.6 |10.2 |-3.9  |
|2009 |-1.2 |-2.9 | 0.5 |-0.2 | 0.1 | 2.5 | 4.5 | 3.0 | 3.0 |-1.4 | 3.8 |-0.3 |11.9 |-5.8  |
|2010 |-2.5 | 1.9 | 2.8 | 2.6 |-4.5 | 2.2 | 1.9 | 2.0 | 0.9 | 2.3 |-1.1 | 4.8 |13.7 |-6.0  |
|2011 | 2.8 | 4.1 | 0.2 | 3.7 | 0.1 |-1.5 | 2.3 |-1.4 | 0.2 | 1.6 |-2.3 | 1.7 |11.8 |-6.9  |
|2012 | 2.1 | 1.7 | 0.5 | 0.8 |-1.6 | 1.3 | 2.8 | 0.3 | 0.4 |-0.4 |-0.1 |-0.2 | 7.7 |-3.9  |
|2013 | 1.9 |-0.8 | 1.7 | 2.9 |-2.6 |-0.8 | 1.4 |-1.1 | 1.2 | 1.7 | 0.2 | 0.9 | 6.7 |-5.1  |
|2014 |-0.1 | 1.4 | 0.3 | 1.4 | 1.1 | 1.1 |-1.5 | 2.1 |-2.1 | 1.1 | 1.3 | 0.3 | 6.4 |-2.2  |
|2015 | 2.9 |-1.4 |     |     |     |     |     |     |     |     |     |     | 1.4 |-1.5  |
|Avg  | 1.1 | 0.6 | 0.5 | 0.4 | 0.2 | 1.1 | 0.9 | 0.9 | 0.9 | 0.7 | 0.8 | 1.3 | 9.4 |-4.2  |
    




|           |LQD    |DBC    |VTI    |ICF    |CASH   |
|:----------|:------|:------|:------|:------|:------|
|2015-02-09 |54.27% | 0.00% | 9.22% |20.47% |16.04% |
    




# QATAA strategy:
    


![plot of chunk plot-3](/public/images/2015-02-09-Channel-Breakout2/plot-3-6.png) 

|     |Jan   |Feb   |Mar   |Apr   |May   |Jun   |Jul   |Aug   |Sep   |Oct   |Nov   |Dec   |Year  |MaxDD |
|:----|:-----|:-----|:-----|:-----|:-----|:-----|:-----|:-----|:-----|:-----|:-----|:-----|:-----|:-----|
|1996 |      |      |      |      |      |  0.9 |  0.3 |  0.1 |  1.2 |  1.6 |  1.1 | -0.5 |  4.7 | -1.0 |
|1997 |  0.4 |  0.0 | -0.9 |  1.2 |  3.4 |  1.8 |  4.8 | -1.5 |  4.9 | -0.7 |  0.2 |  1.7 | 16.1 | -4.9 |
|1998 |  0.4 |  1.3 |  2.0 | -0.3 | -0.2 |  1.5 | -0.5 | -2.5 |  2.4 | -0.3 |  1.8 |  1.9 |  7.8 | -4.1 |
|1999 |  1.4 | -2.5 |  1.4 |  1.9 | -1.2 |  2.6 | -1.1 |  1.0 | -0.3 | -0.9 |  1.7 |  2.6 |  6.6 | -3.1 |
|2000 |  0.0 |  1.8 |  2.2 | -0.2 |  1.2 |  3.0 |  1.1 |  3.5 | -0.5 | -1.7 |  2.6 |  2.4 | 16.7 | -4.6 |
|2001 |  1.2 | -0.2 | -0.9 |  0.2 | -0.1 |  1.8 |  1.3 |  2.1 | -0.1 |  0.8 | -1.3 | -0.2 |  4.7 | -3.6 |
|2002 |  0.3 |  1.0 | -0.2 | -0.4 |  0.9 |  1.4 | -0.4 |  2.2 |  0.3 | -0.2 |  0.1 |  3.1 |  8.4 | -3.3 |
|2003 |  1.9 |  2.1 | -1.9 |  1.1 |  4.8 |  0.9 |  1.1 |  2.3 |  1.5 |  2.3 |  1.7 |  4.1 | 23.9 | -3.7 |
|2004 |  2.7 |  3.1 |  1.9 | -5.4 |  1.3 | -0.1 |  0.5 |  2.8 |  1.5 |  2.6 |  2.0 |  1.5 | 15.2 | -6.5 |
|2005 | -2.0 |  2.9 | -0.2 |  0.0 |  1.6 |  2.4 |  4.0 |  0.6 |  0.2 | -2.7 |  2.4 |  1.7 | 11.1 | -4.3 |
|2006 |  3.8 | -0.7 |  2.2 |  1.1 | -1.6 |  1.2 |  1.8 |  1.2 |  0.3 |  2.9 |  2.5 | -1.1 | 14.4 | -5.3 |
|2007 |  2.4 | -0.4 | -0.6 |  1.7 |  0.1 | -3.2 |  0.0 |  0.4 |  3.6 |  2.9 | -0.6 |  1.3 |  7.9 | -5.3 |
|2008 |  2.2 |  3.0 | -0.3 |  1.4 |  1.0 | -0.1 | -2.1 | -1.3 | -2.7 |  1.1 |  1.1 |  0.6 |  3.7 | -9.3 |
|2009 | -0.8 | -1.4 |  0.5 |  0.6 |  0.6 |  0.8 |  3.6 |  4.0 |  3.3 | -0.3 |  4.9 |  1.8 | 18.8 | -5.1 |
|2010 | -4.1 |  2.4 |  4.3 |  3.8 | -6.0 | -1.9 |  3.2 |  0.6 |  1.2 |  3.1 | -0.9 |  5.2 | 10.9 | -9.9 |
|2011 |  2.5 |  3.4 |  0.3 |  4.0 | -0.8 | -2.6 |  1.6 | -2.9 | -6.5 |  0.6 | -1.8 |  0.9 | -1.6 |-13.5 |
|2012 |  3.3 |  1.1 |  1.2 |  0.5 | -5.3 |  2.5 |  1.7 |  0.6 |  0.3 | -1.4 | -0.1 |  0.0 |  4.2 | -7.1 |
|2013 |  2.4 | -0.5 |  1.6 |  2.7 | -1.8 | -0.8 |  1.6 | -2.5 |  1.1 |  1.1 |  0.7 |  0.7 |  6.3 | -6.0 |
|2014 | -0.2 |  1.5 |  0.3 |  1.6 |  1.2 |  1.3 | -1.6 |  2.3 | -2.4 |  3.8 |  1.5 |  0.4 |  9.9 | -2.7 |
|2015 |  2.2 | -0.5 |      |      |      |      |      |      |      |      |      |      |  1.7 | -1.5 |
|Avg  |  1.1 |  0.9 |  0.7 |  0.9 | -0.1 |  0.7 |  1.1 |  0.7 |  0.5 |  0.8 |  1.0 |  1.5 |  9.6 | -5.2 |
    




|           |LQD    |DBC    |VTI    |ICF    |CASH   |
|:----------|:------|:------|:------|:------|:------|
|2015-02-09 |24.51% | 0.00% |25.85% |24.69% |24.96% |
    


Finnally, let's zoom in on the recent perfomance strating in 2010:


{% highlight r %}
models.2010 = bt.trim(models, dates = '2010::')

plotbt(models.2010, plotX = T, log = 'y', LeftMargin = 3, main = NULL)
	mtext('Cumulative Performance', side = 2, line = 1)
{% endhighlight %}

![plot of chunk plot-4](/public/images/2015-02-09-Channel-Breakout2/plot-4-1.png) 

{% highlight r %}
print(plotbt.strategy.sidebyside(models.2010, make.plot=F, return.table=T))
{% endhighlight %}



|           |ew                |rp                |channel.ew        |channel.rp        |QATAA             |
|:----------|:-----------------|:-----------------|:-----------------|:-----------------|:-----------------|
|Period     |Jan2010 - Feb2015 |Jan2010 - Feb2015 |Jan2010 - Feb2015 |Jan2010 - Feb2015 |Jan2010 - Feb2015 |
|Cagr       |8.46              |8.05              |8.82              |9.12              |5.85              |
|Sharpe     |0.78              |1.09              |1.02              |1.42              |0.69              |
|DVR        |0.73              |1.02              |0.97              |1.35              |0.53              |
|Volatility |11.45             |7.57              |8.83              |6.47              |9.14              |
|MaxDD      |-14.65            |-9.15             |-11.5             |-6.86             |-13.54            |
|AvgDD      |-1.58             |-1.15             |-1.45             |-1.07             |-1.53             |
|VaR        |-1.1              |-0.71             |-0.82             |-0.59             |-0.92             |
|CVaR       |-1.75             |-1.12             |-1.38             |-0.99             |-1.45             |
|Exposure   |100               |100               |100               |100               |100               |
    


We are able to match the sharpe ratio of about 1.5 using ETF data as reported in the source
[source](https://cssanalytics.wordpress.com/2015/02/08/a-simple-tactical-asset-allocation-portfolio-with-percentile-channels-for-dummies/)

Thank you David for this concept; it is a very robust allocation framework.



*(this report was produced on: 2015-02-10)*
