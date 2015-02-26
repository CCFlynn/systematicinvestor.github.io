---
layout: post
title: Walk Forward Optimization
comments: true
---


To install [Systematic Investor Toolbox (SIT)](https://github.com/systematicinvestor/SIT) please visit [About](',base.url,'about) page.





There is an interesting article about Walk Forward Optimization
at [The Logical-Invest "Universal Investment Strategy"A Walk Forward Process on SPY and TLT](https://quantstrattrader.wordpress.com/2015/02/23/the-logical-invest-universal-investment-strategy-a-walk-forward-process-on-spy-and-tlt/)

The strategy is based on the concept presented in the [The SPY-TLT Universal Investment Strategy (UIS)](http://www.logical-invest.com/universal-investment-strategy) article.

Below I will try to adapt a code from the posts:


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
EQ = SPY + VFINX # S&P 500
FI = TLT + VUSTX # 20 Year Treasury
'

# uncomment if you want to use same data as in the source
#tickers = 'EQ=SPY, FI=TLT'

data <- new.env()
getSymbols.extra(tickers, src = 'yahoo', from = '1970-01-01', env = data, raw.data = data.proxy.raw, set.symbolnames = T, auto.assign = T)
  for(i in data$symbolnames) data[[i]] = adjustOHLC(data[[i]], use.Adjusted=T)
bt.prep(data, align='remove.na')

# Check data
plota.matplot(scale.one(data$prices),main='Asset Perfromance')
{% endhighlight %}

![plot of chunk plot-2](/public/images/2015-02-24-Walk-Forward-Optimization/plot-2-1.png) 

{% highlight r %}
#*****************************************************************
# Setup
#*****************************************************************
prices = data$prices

frequency = 'months'
period.ends = endpoints(prices, frequency)
  period.ends = period.ends[period.ends > 0]

# all possible combinations
choices = expand.grid(
  EQ = seq(0, 100, by = 5),
  FI = seq(0, 100, by = 5),
KEEP.OUT.ATTRS=F)

# only select ones that sum up to 1
choices = choices[choices$EQ + choices$FI == 100,]
  choices = choices[sort.list(choices$EQ),]
  index = 1:nrow(choices)

# run back test over all combinations
result = rep.col(prices[,1], nrow(choices))
  colnames(result) = index

for(i in 1:nrow(choices)) {
  data$weight[] = NA
    data$weight$EQ[period.ends] = choices$EQ[i]/100
    data$weight$FI[period.ends] = choices$FI[i]/100
  model = bt.run.weight.fast(data)
  # uncomment if you want to get same results as in the source
  #model = bt.run.share(data, clean.signal=F, silent=T)
  result[,i] = model$equity
}

plota.matplot(result,main='Strategy Perfromance')
{% endhighlight %}

![plot of chunk plot-2](/public/images/2015-02-24-Walk-Forward-Optimization/plot-2-2.png) 

{% highlight r %}
#*****************************************************************
# Pick Strategy based on modified Sharpe over 72 days
#*****************************************************************
sd.factor = 2.5
lookback.len = 72

lookback.return = (result / mlag(result,lookback.len))^(252/lookback.len) - 1

lookback.sd = bt.apply.matrix(result / mlag(result)-1, runSD, lookback.len)*sqrt(252)

mod.sharpe = lookback.return / lookback.sd ^ sd.factor
  mod.sharpe = mod.sharpe[period.ends,]

# pick best one
best.sharpe = ntop(mod.sharpe, 1)

# map back to original weights
weight = t(apply(best.sharpe, 1, function(x) 
  colMeans(choices[index[x!=0],,drop=F])
)) / 100

weight = make.xts(weight, data$dates[period.ends])
#*****************************************************************
# Test strategy
#*****************************************************************
models = list()

data$weight[] = NA
	data$weight$EQ = 1
models$SP500 = bt.run.share(data, clean.signal=T)
{% endhighlight %}

Latest weights :


|           |  EQ| FI|
|:----------|---:|--:|
|2015-02-25 | 100|  0|
    



Performance summary :
	CAGR	Best	Worst	
	9.5	14.5	-9.8	



{% highlight r %}
data$weight[] = NA
  data$weight[period.ends,] = as.matrix(weight)
models$UIS = bt.run.share(data, clean.signal=F)
{% endhighlight %}

Latest weights :


|           |    EQ|    FI|
|:----------|-----:|-----:|
|2015-02-25 | 53.06| 46.94|
    



Performance summary :
	CAGR	Best	Worst	
	12.4	5.2	-4.2	



{% highlight r %}
#*****************************************************************
# Create Report
#*****************************************************************
plotbt(models, plotX = T, log = 'y', LeftMargin = 3, main = NULL)
	mtext('Cumulative Performance', side = 2, line = 1)
{% endhighlight %}

![plot of chunk plot-2](/public/images/2015-02-24-Walk-Forward-Optimization/plot-2-3.png) 

{% highlight r %}
print(plotbt.strategy.sidebyside(models, make.plot=F, return.table=T, perfromance.fn=engineering.returns.kpi))
{% endhighlight %}



|           |SP500             |UIS               |
|:----------|:-----------------|:-----------------|
|Period     |Dec1989 - Feb2015 |Dec1989 - Feb2015 |
|Cagr       |9.55              |12.35             |
|Sharpe     |0.59              |1.25              |
|DVR        |0.48              |1.02              |
|R2         |0.81              |0.82              |
|Volatility |18.41             |9.75              |
|MaxDD      |-55.19            |-17.07            |
|Exposure   |99.98             |98.83             |
    




{% highlight r %}
print(last.trades(models$UIS, make.plot=F, return.table=T))
{% endhighlight %}

    




{% highlight r %}
print(plotbt.monthly.table(models$UIS$equity, make.plot = F))
{% endhighlight %}



|     |Jan   |Feb   |Mar   |Apr   |May   |Jun   |Jul   |Aug   |Sep   |Oct   |Nov   |Dec   |Year  |MaxDD |
|:----|:-----|:-----|:-----|:-----|:-----|:-----|:-----|:-----|:-----|:-----|:-----|:-----|:-----|:-----|
|1989 |      |      |      |      |      |      |      |      |      |      |      |      |  0.0 |  0.0 |
|1990 |  0.0 |  0.0 |  0.0 | -2.5 |  9.7 |  0.2 |  0.2 | -5.4 |  1.1 |  2.2 |  4.7 |  2.0 | 12.0 | -9.0 |
|1991 |  1.5 |  2.1 |  1.0 |  0.8 |  1.5 | -2.9 |  1.4 |  3.6 |  1.6 |  0.2 | -0.8 |  6.7 | 17.8 | -4.5 |
|1992 | -2.7 |  0.6 | -1.5 |  1.8 |  0.5 |  0.4 |  3.8 |  0.1 |  1.5 | -1.3 |  1.3 |  2.3 |  6.9 | -4.7 |
|1993 |  0.4 |  1.3 |  1.8 | -1.2 |  0.7 |  3.1 |  1.4 |  4.1 |  0.3 |  1.0 | -1.7 |  0.9 | 12.7 | -4.0 |
|1994 |  3.5 | -3.0 | -4.2 |  1.1 |  1.6 | -2.3 |  3.2 |  3.8 | -2.5 |  2.8 | -4.0 |  0.7 |  0.4 | -8.5 |
|1995 |  2.5 |  3.0 |  1.4 |  2.4 |  5.2 |  1.5 |  0.6 |  1.1 |  3.6 |  0.0 |  3.5 |  2.2 | 30.6 | -2.8 |
|1996 |  0.9 | -3.1 |  0.8 |  1.1 |  2.3 |  0.9 | -3.8 | -1.2 |  2.7 |  3.7 |  5.1 | -2.4 |  6.7 | -6.9 |
|1997 |  1.5 |  0.5 | -3.8 |  6.1 |  1.9 |  3.1 |  5.3 | -2.7 |  2.9 |  3.1 |  1.4 |  1.7 | 22.5 | -8.8 |
|1998 |  1.6 |  0.4 |  1.6 |  0.8 | -1.0 |  3.2 | -0.6 |  3.0 |  3.6 | -0.7 |  1.3 |  1.3 | 15.5 | -5.3 |
|1999 |  1.8 | -4.2 |  1.6 |  2.4 | -2.1 |  3.9 | -3.0 | -0.5 | -2.2 |  0.2 |  1.7 |  1.8 |  1.0 |-11.3 |
|2000 | -1.9 |  1.9 |  2.9 | -0.9 | -0.5 |  2.4 |  1.2 |  2.6 | -1.5 |  1.7 |  2.9 |  2.4 | 13.8 | -5.7 |
|2001 |  0.6 |  0.1 | -1.1 | -2.5 |  0.2 |  0.8 |  0.8 |  0.9 |  0.9 |  5.0 | -4.5 | -1.6 | -0.6 | -9.0 |
|2002 |  0.1 | -0.7 |  3.3 | -3.0 |  0.4 |  1.8 |  2.2 |  5.0 |  2.8 | -2.5 |  0.8 |  1.5 | 12.1 | -7.1 |
|2003 | -1.1 |  1.8 | -1.0 |  2.9 |  6.1 | -0.9 | -5.3 |  1.9 | -1.1 |  5.4 |  0.9 |  3.4 | 13.0 |-11.1 |
|2004 |  1.9 |  1.8 | -0.1 | -4.3 |  1.7 |  1.9 | -3.2 |  4.2 |  1.0 |  1.6 |  0.0 |  2.8 |  9.3 | -7.4 |
|2005 |  0.4 |  0.3 | -1.1 |  3.9 |  3.2 |  1.9 | -1.2 |  1.0 | -1.2 | -2.3 |  4.4 |  0.3 |  9.6 | -5.1 |
|2006 |  0.3 |  0.8 | -1.8 |  0.7 | -3.0 |  0.3 |  0.4 |  3.0 |  2.1 |  1.5 |  2.1 | -0.7 |  5.9 | -8.3 |
|2007 |  0.5 |  0.2 | -0.6 |  3.2 | -0.3 | -1.2 | -2.5 |  1.8 |  0.6 |  1.7 |  3.0 | -0.8 |  5.6 | -6.5 |
|2008 | -0.3 | -0.9 |  1.8 | -1.4 | -0.6 | -2.3 | -0.9 |  2.7 |  0.9 | -4.8 | 14.3 | 12.4 | 21.1 |-10.6 |
|2009 |-12.3 | -3.4 |  4.7 |  9.9 |  5.8 |  0.2 |  5.1 |  3.2 |  3.1 | -2.3 |  3.7 | -2.2 | 14.6 |-15.6 |
|2010 | -2.1 |  3.1 |  3.6 |  2.3 | -0.8 |  1.4 |  1.4 |  3.9 |  1.5 | -0.7 | -0.8 |  3.6 | 17.6 | -6.0 |
|2011 |  1.2 |  3.2 |  0.0 |  2.7 |  1.1 | -2.0 |  1.2 |  4.4 |  6.1 |  2.1 |  1.0 |  2.4 | 25.8 | -3.5 |
|2012 |  1.9 |  0.9 | -0.1 |  1.3 | -0.7 |  0.9 |  2.6 |  0.4 |  0.0 | -1.2 |  0.7 | -0.8 |  5.9 | -3.8 |
|2013 |  0.1 |  1.3 |  2.1 |  3.0 | -1.7 | -2.1 |  4.0 | -3.0 |  3.2 |  4.6 |  1.8 |  1.0 | 15.0 | -6.3 |
|2014 | -1.1 |  2.7 |  0.8 |  1.5 |  2.7 |  0.9 | -0.3 |  4.3 | -1.7 |  2.6 |  2.9 |  1.6 | 17.9 | -3.0 |
|2015 |  4.7 |  0.2 |      |      |      |      |      |      |      |      |      |      |  5.0 | -2.1 |
|Avg  |  0.2 |  0.4 |  0.5 |  1.3 |  1.4 |  0.6 |  0.6 |  1.7 |  1.2 |  0.9 |  1.8 |  1.7 | 11.8 | -6.5 |
    




{% highlight r %}
plota(weight$EQ, type='s', main='SP500 Allocation in UIS Model')
{% endhighlight %}

![plot of chunk plot-2](/public/images/2015-02-24-Walk-Forward-Optimization/plot-2-4.png) 

There a few more ideas you might try:

* select top N best performing combinations and average their weight
* do not consider portfolios with negative modified Sharpe
* if no portfolio is selected, invest into 100% TLT

It is very easy to modify code above to enforce these rules:


{% highlight r %}
#*****************************************************************
# Modify strategy
#*****************************************************************
# 1. pick top 5
best.sharpe = ntop(mod.sharpe, 5)

  # 2. only consider portfolios with sharpe > 0
  best.sharpe = iif(mod.sharpe > 1, best.sharpe, 0)

# map back to original weights
weight = t(apply(best.sharpe, 1, function(x) 
  colMeans(choices[index[x!=0],,drop=F])
)) / 100

weight = make.xts(weight, data$dates[period.ends])
  # 3. if no portfolio is selected, invest into 100% TLT
  weight = ifna(weight,0)
  weight$FI = weight$FI + 1 - rowSums(weight)


data$weight[] = NA
  data$weight[period.ends,] = as.matrix(weight)
models$UIS5 = bt.run.share(data, clean.signal=F)
{% endhighlight %}

Latest weights :


|           |    EQ|    FI|
|:----------|-----:|-----:|
|2015-02-25 | 48.04| 51.96|
    



Performance summary :
	CAGR	Best	Worst	
	10.1	4.7	-3.2	



{% highlight r %}
#*****************************************************************
# Create Report
#*****************************************************************
plotbt(models, plotX = T, log = 'y', LeftMargin = 3, main = NULL)
	mtext('Cumulative Performance', side = 2, line = 1)
{% endhighlight %}

![plot of chunk plot-3](/public/images/2015-02-24-Walk-Forward-Optimization/plot-3-1.png) 

{% highlight r %}
print(plotbt.strategy.sidebyside(models, make.plot=F, return.table=T, perfromance.fn=engineering.returns.kpi))
{% endhighlight %}



|           |SP500             |UIS               |UIS5              |
|:----------|:-----------------|:-----------------|:-----------------|
|Period     |Dec1989 - Feb2015 |Dec1989 - Feb2015 |Dec1989 - Feb2015 |
|Cagr       |9.55              |12.35             |10.05             |
|Sharpe     |0.59              |1.25              |1.06              |
|DVR        |0.48              |1.02              |0.93              |
|R2         |0.81              |0.82              |0.87              |
|Volatility |18.41             |9.75              |9.43              |
|MaxDD      |-55.19            |-17.07            |-18.81            |
|Exposure   |99.98             |98.83             |99.83             |
    

Unfortunately, the performance is not reflected in our modifications. Another idea we might try
was mentioned in the comment of [original article](https://quantstrattrader.wordpress.com/2015/02/23/the-logical-invest-universal-investment-strategy-a-walk-forward-process-on-spy-and-tlt/)
by CyTrader, to to make lookback period and the F-Factor adaptive, for example:

* lookback can be in 2-6 months range
* F-Factor can be in 1.5-3.5 range



{% highlight r %}
#*****************************************************************
# make lookback period and the F-Factor adaptive
#*****************************************************************
lookbacks = round(21 * seq(2, 6, by = 0.5))  # 2-6 months range
ffactors  = seq(1.5, 3.5, by = 0.5) # 1.5-3.5 range

# best sharpe across all parameters
best.weight = weight
  best.weight[] = 0
# average of best sharpes for each parameter 
avg.weight = weight
  avg.weight[] = 0
best.mod.sharpe = rep(-10e10, nrow(weight))

for(lookback.len in lookbacks) {
  lookback.return = (result / mlag(result,lookback.len))^(252/lookback.len) - 1
  lookback.sd = bt.apply.matrix(result / mlag(result)-1, runSD, lookback.len)*sqrt(252)
  
  for(sd.factor in ffactors) {
    mod.sharpe = lookback.return / lookback.sd ^ sd.factor
      mod.sharpe = mod.sharpe[period.ends,]

    best.sharpe = ntop(mod.sharpe, 1)

    # map back to original weights
    iweight = t(apply(best.sharpe, 1, function(x) 
      colMeans(choices[index[x!=0],,drop=F])
    )) / 100

    ibest.mod.sharpe = rowSums(best.sharpe * mod.sharpe)

    avg.weight = avg.weight +  iweight
    
    select.index =  ifna(ibest.mod.sharpe > best.mod.sharpe, F)
    best.mod.sharpe[select.index] = ibest.mod.sharpe[select.index]
    best.weight[select.index,] = iweight[select.index,]
  }
}

avg.weight = avg.weight / rowSums(avg.weight)
best.weight = best.weight / rowSums(best.weight)

data$weight[] = NA
  data$weight[period.ends,] = as.matrix(avg.weight)
models$UIS.A = bt.run.share(data, clean.signal=F)
{% endhighlight %}

Latest weights :


|           |    EQ|    FI|
|:----------|-----:|-----:|
|2015-02-25 | 47.15| 52.85|
    



Performance summary :
	CAGR	Best	Worst	
	11.1	4.2	-3.2	



{% highlight r %}
data$weight[] = NA
  data$weight[period.ends,] = as.matrix(best.weight)
models$UIS.B = bt.run.share(data, clean.signal=F)
{% endhighlight %}

Latest weights :


|           |    EQ|    FI|
|:----------|-----:|-----:|
|2015-02-25 | 53.06| 46.94|
    



Performance summary :
	CAGR	Best	Worst	
	10.3	4.7	-3.9	



{% highlight r %}
#*****************************************************************
# Create Report
#*****************************************************************
plotbt(models, plotX = T, log = 'y', LeftMargin = 3, main = NULL)
	mtext('Cumulative Performance', side = 2, line = 1)
{% endhighlight %}

![plot of chunk plot-4](/public/images/2015-02-24-Walk-Forward-Optimization/plot-4-1.png) 

{% highlight r %}
print(plotbt.strategy.sidebyside(models, make.plot=F, return.table=T, perfromance.fn=engineering.returns.kpi))
{% endhighlight %}



|           |SP500             |UIS               |UIS5              |UIS.A             |UIS.B             |
|:----------|:-----------------|:-----------------|:-----------------|:-----------------|:-----------------|
|Period     |Dec1989 - Feb2015 |Dec1989 - Feb2015 |Dec1989 - Feb2015 |Dec1989 - Feb2015 |Dec1989 - Feb2015 |
|Cagr       |9.55              |12.35             |10.05             |11.14             |10.35             |
|Sharpe     |0.59              |1.25              |1.06              |1.19              |1.17              |
|DVR        |0.48              |1.02              |0.93              |1.02              |1.05              |
|R2         |0.81              |0.82              |0.87              |0.85              |0.9               |
|Volatility |18.41             |9.75              |9.43              |9.23              |8.75              |
|MaxDD      |-55.19            |-17.07            |-18.81            |-16.67            |-17.85            |
|Exposure   |99.98             |98.83             |99.83             |97.84             |99.18             |
    




*(this report was produced on: 2015-02-26)*
