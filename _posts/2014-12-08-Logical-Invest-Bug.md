---
layout: post
title: Logical Invest Permanent Portfolio
---


To install [Systematic Investor Toolbox (SIT)](https://github.com/systematicinvestor/SIT) please visit [About](/about) page.





Look at Vangelis Maderakis's blog post:
[Will We Ever Kill The Bug?](http://www.logical-invest.com/will-ever-kill-bug/)

The Permanent Portfolio: Stocks, Bonds, Gold, Cash, 25% each


Load historical data for SPY,TLT,GLD,SHY.


{% highlight r %}
#*****************************************************************
# Load historical data
#*****************************************************************
library(SIT)
load.packages('quantmod')
tickers = spl('SPY,TLT,GLD,SHY')

data <- new.env()
getSymbols(tickers, src = 'yahoo', from = '1970-01-01', env = data, auto.assign = T)
extend.data.proxy(data)
for(i in ls(data)) data[[i]] = adjustOHLC(data[[i]], use.Adjusted=T)
bt.prep(data, align='remove.na')
{% endhighlight %}


Now we ready to back-test our strategy:


{% highlight r %}
#*****************************************************************
# Code Strategies
#*****************************************************************
prices = data$prices
	n = ncol(prices)
	nperiods = nrow(prices)
period.ends = endpoints(prices, 'years')


models = list()

#*****************************************************************
# Code Strategies, SPY - Buy & Hold
#*****************************************************************
data$weight[] = NA
	data$weight$SPY = 1
models$SPY = bt.run.share(data, clean.signal=T, silent=T)

#*****************************************************************
# Code Strategies, Equal Weight, re-balanced
#*****************************************************************
target.allocation = prices
	target.allocation[] = rep.row(rep(1/n,n), nperiods)

data$weight[] = NA
	data$weight[period.ends,] = target.allocation[period.ends,]
models$permanent.y = bt.run.share(data, clean.signal=F, silent=T)

#*****************************************************************
# Same monthly
#*****************************************************************
period.ends = endpoints(prices, 'months')

weight.equal = target.allocation

data$weight[] = NA
	data$weight[period.ends,] = weight.equal[period.ends,]
models$permanent.m = bt.run.share(data, clean.signal=F, silent=T)

#*****************************************************************
# Volatility Targeting
#*****************************************************************
ret.log = bt.apply.matrix(prices, ROC, type='continuous')
hist.vol = sqrt(252) * bt.apply.matrix(ret.log, runSD, n = 21)
weight.risk = weight.equal / hist.vol
	weight.risk = weight.risk / rowSums(weight.risk)

data$weight[] = NA
	data$weight[period.ends,] = weight.risk[period.ends,]
models$volatility = bt.run.share(data, clean.signal=F, silent=T)

#*****************************************************************
# Volatility Targeting 2
#*****************************************************************
weight.risk = iif(hist.vol > bt.apply.matrix(hist.vol, SMA, 21), 
				weight.equal / 2, weight.equal)
weight.risk$SHY = weight.risk$SHY + 1 - rowSums(weight.risk)

data$weight[] = NA
	data$weight[period.ends,] = weight.risk[period.ends,]
models$volatility2 = bt.run.share(data, clean.signal=F, silent=T)

#*****************************************************************
# Momentum
# Lets try by pulling 15% of equity from the worst asset.
# divide the proceeds in three and buy equal amounts
#*****************************************************************
momentum = prices / mlag(prices, 6*21) - 1
	worst.asset = ntop(prices, 1, F)
	penalty = 1/n * 0.15
weight.momentum = weight.equal - penalty * worst.asset + penalty * (1-worst.asset)/(n-1)
 
data$weight[] = NA
	data$weight[period.ends,] = weight.momentum[period.ends,]
models$momentum = bt.run.share(data, clean.signal=F, silent=T)


#*****************************************************************
# Mean Reversion
# sell shares of the best short-term performer and distribute the money to the others
#*****************************************************************
mean.reversion = prices / mlag(prices, 1*21) - 1
	best.asset = ntop(prices, 1)
	penalty = 1/n * 0.15
weight.mean.reversion = weight.equal - penalty * best.asset + penalty * (1-best.asset)/(n-1)
 
data$weight[] = NA
	data$weight[period.ends,] = weight.mean.reversion[period.ends,]
models$mean.reversion = bt.run.share(data, clean.signal=F, silent=T)

#*****************************************************************
# Timing
# assets price is below its own 200-day simple moving average then we sell it
#*****************************************************************
sma200 = bt.apply.matrix(prices, SMA, 200)
weight.timing = iif(prices > sma200, weight.equal, 0)
weight.timing$SHY = weight.timing$SHY + 1 - rowSums(weight.timing)
    
data$weight[] = NA
	data$weight[period.ends,] = weight.timing[period.ends,]
models$timing = bt.run.share(data, clean.signal=F, silent=T)


#*****************************************************************
# Create Report
#*****************************************************************
plotbt(models, plotX = T, log = 'y', LeftMargin = 3, main = NULL)
mtext('Cumulative Performance', side = 2, line = 1)
{% endhighlight %}

![plot of chunk plot-3](/public/images/2014-12-08-Logical-Invest-Bug/plot-3-1.png) 

{% highlight r %}
print(plotbt.strategy.sidebyside(models, make.plot=F, return.table=T))
{% endhighlight %}



|           |SPY               |permanent.y       |permanent.m       |volatility        |volatility2       |momentum          |mean.reversion    |timing            |
|:----------|:-----------------|:-----------------|:-----------------|:-----------------|:-----------------|:-----------------|:-----------------|:-----------------|
|Period     |Jan1993 - Dec2014 |Jan1993 - Dec2014 |Jan1993 - Dec2014 |Jan1993 - Dec2014 |Jan1993 - Dec2014 |Jan1993 - Dec2014 |Jan1993 - Dec2014 |Jan1993 - Dec2014 |
|Cagr       |9.39              |7.1               |7.36              |4.98              |6.68              |7.29              |7.39              |8.1               |
|Sharpe     |0.57              |1.09              |1.11              |1.57              |1.23              |1.1               |1.15              |1.42              |
|DVR        |0.42              |1.01              |1.04              |1.56              |1.17              |1.03              |1.07              |1.34              |
|Volatility |19.09             |6.57              |6.67              |3.17              |5.47              |6.69              |6.49              |5.69              |
|MaxDD      |-55.19            |-14.18            |-15.07            |-5.58             |-10.34            |-15.7             |-14.1             |-7.13             |
|AvgDD      |-2.09             |-1                |-0.95             |-0.41             |-0.82             |-0.96             |-0.93             |-0.78             |
|VaR        |-1.91             |-0.65             |-0.64             |-0.29             |-0.54             |-0.65             |-0.63             |-0.57             |
|CVaR       |-2.84             |-0.93             |-0.92             |-0.44             |-0.77             |-0.93             |-0.9              |-0.81             |
|Exposure   |99.98             |95.8              |99.98             |99.21             |99.98             |99.98             |99.98             |99.98             |
    




{% highlight r %}
print(last.trades(models$top1, make.plot=F, return.table=T))
{% endhighlight %}

    




{% highlight r %}
#*****************************************************************
# Same for 2014
#*****************************************************************
models1 = bt.trim(models, dates = '2014')


#*****************************************************************
# Create Report
#*****************************************************************
plotbt(models1, plotX = T, log = 'y', LeftMargin = 3, main = NULL)
mtext('Cumulative Performance', side = 2, line = 1)
{% endhighlight %}

![plot of chunk plot-3](/public/images/2014-12-08-Logical-Invest-Bug/plot-3-2.png) 

{% highlight r %}
print(plotbt.strategy.sidebyside(models1, make.plot=F, return.table=T))
{% endhighlight %}



|           |SPY               |permanent.y       |permanent.m       |volatility        |volatility2       |momentum          |mean.reversion    |timing            |
|:----------|:-----------------|:-----------------|:-----------------|:-----------------|:-----------------|:-----------------|:-----------------|:-----------------|
|Period     |Jan2014 - Dec2014 |Jan2014 - Dec2014 |Jan2014 - Dec2014 |Jan2014 - Dec2014 |Jan2014 - Dec2014 |Jan2014 - Dec2014 |Jan2014 - Dec2014 |Jan2014 - Dec2014 |
|Cagr       |15.54             |10.43             |10.33             |1.92              |4.94              |10.84             |10.07             |4.87              |
|Sharpe     |1.3               |2.14              |2.12              |1.46              |1.36              |2.13              |2.01              |1.25              |
|DVR        |1.07              |1.68              |1.64              |1.12              |0.44              |1.64              |1.49              |0.9               |
|Volatility |10.73             |4.78              |4.78              |1.33              |3.72              |5                 |4.97              |3.65              |
|MaxDD      |-7.27             |-2.65             |-2.57             |-0.76             |-2.79             |-2.69             |-2.7              |-2.57             |
|AvgDD      |-1.29             |-0.6              |-0.62             |-0.16             |-0.54             |-0.65             |-0.63             |-0.64             |
|VaR        |-1.14             |-0.46             |-0.47             |-0.13             |-0.37             |-0.49             |-0.5              |-0.37             |
|CVaR       |-1.69             |-0.65             |-0.64             |-0.19             |-0.54             |-0.67             |-0.65             |-0.59             |
|Exposure   |100               |100               |100               |100               |100               |100               |100               |100               |
    




{% highlight r %}
#*****************************************************************
# Same for last 5 years
#*****************************************************************
models1 = bt.trim(models, dates = '2010::')


#*****************************************************************
# Create Report
#*****************************************************************
plotbt(models1, plotX = T, log = 'y', LeftMargin = 3, main = NULL)
mtext('Cumulative Performance', side = 2, line = 1)
{% endhighlight %}

![plot of chunk plot-3](/public/images/2014-12-08-Logical-Invest-Bug/plot-3-3.png) 

{% highlight r %}
print(plotbt.strategy.sidebyside(models1, make.plot=F, return.table=T))
{% endhighlight %}



|           |SPY               |permanent.y       |permanent.m       |volatility        |volatility2       |momentum          |mean.reversion    |timing            |
|:----------|:-----------------|:-----------------|:-----------------|:-----------------|:-----------------|:-----------------|:-----------------|:-----------------|
|Period     |Jan2010 - Dec2014 |Jan2010 - Dec2014 |Jan2010 - Dec2014 |Jan2010 - Dec2014 |Jan2010 - Dec2014 |Jan2010 - Dec2014 |Jan2010 - Dec2014 |Jan2010 - Dec2014 |
|Cagr       |15.15             |7.66              |7.51              |2.55              |5.97              |7.72              |7.25              |6.96              |
|Sharpe     |0.99              |1.24              |1.21              |1.73              |1.16              |1.19              |1.21              |1.34              |
|DVR        |0.91              |1.05              |0.98              |1.44              |0.89              |0.96              |0.98              |1.26              |
|Volatility |15.86             |6.3               |6.31              |1.51              |5.28              |6.61              |6.1               |5.28              |
|MaxDD      |-18.61            |-8.05             |-8.85             |-1.1              |-6.74             |-9.27             |-8.53             |-3.7              |
|AvgDD      |-1.67             |-0.97             |-0.96             |-0.18             |-0.79             |-1.02             |-0.91             |-0.72             |
|VaR        |-1.61             |-0.62             |-0.64             |-0.15             |-0.51             |-0.67             |-0.61             |-0.54             |
|CVaR       |-2.42             |-0.92             |-0.92             |-0.21             |-0.81             |-0.97             |-0.89             |-0.8              |
|Exposure   |100               |100               |100               |100               |100               |100               |100               |100               |
    

Finally let's include some 'newer' asset classes that were not easily accessible during the 80's.
    Convertible bonds (CWB)
    Foreign bonds (PCY)
    Inflation protected Treasuries (TIP)



*(this report was produced on: 2014-12-10)*
