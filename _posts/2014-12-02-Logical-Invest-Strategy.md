---
layout: post
title: Logical Invest Universal Investment Strategy (UIS)
---


To install [Systematic Investor Toolbox (SIT)](https://github.com/systematicinvestor/SIT) please visit [About](/about) page.





Look at Frank Grossmann's blog post:
[The SPY-TLT Universal Investment Strategy (UIS)](http://www.logical-invest.com/universal-investment-strategy/)

The real world is just not a 100% "risk on" or "risk off" world. Most of the time, the best allocation is somewhere in between.



Load historical data for SPY and TLT, and align it, so that dates on both time series match. We also adjust data for stock splits and dividends.


{% highlight r %}
	#*****************************************************************
	# Load historical data
	#*****************************************************************
	library(SIT)
	load.packages('quantmod')
	tickers = spl('SPY,TLT')

	data <- new.env()
	getSymbols(tickers, src = 'yahoo', from = '1970-01-01', env = data, auto.assign = T)
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
	month.ends = endpoints(prices, 'months')
		
	models = list()
	
	#*****************************************************************
	# Code Strategies, SPY - Buy & Hold
	#*****************************************************************
	data$weight[] = NA
		data$weight$SPY = 1
	models$SPY = bt.run.share(data, clean.signal=T, silent=T)
	
	#*****************************************************************
	# Code Strategies, Equal Weight, re-balanced monthly
	#*****************************************************************
	data$weight[] = NA
		data$weight[month.ends,] = ntop(prices, n)[month.ends,]	
	models$equal.weight = bt.run.share(data, clean.signal=F, silent=T)
	
	#*****************************************************************
	# Code Strategies, Top 1 based on 3 month momentum, re-balanced monthly
	#*****************************************************************
	position.score = prices / mlag(prices, 3*21)	
	
	data$weight[] = NA
		data$weight[month.ends,] = ntop(position.score[month.ends,], 1)	
	models$top1 = bt.run.share(data, trade.summary=T, clean.signal=F, silent=T)

	#*****************************************************************
	# Code Strategies, Logical Invest Universal Investment Strategy (UIS)
	#*****************************************************************
adaptive.weight <- function(prices, lookback = 80, f=2, index = 1:nrow(prices)) {
	if( len(lookback) == 1 ) lookback = rep(lookback, len(index))
	if( len(f) == 1 ) f = rep(f, len(index))
	
	ret = prices / mlag(prices) - 1
		ret = coredata(ret)
		
	allocation = seq(0,100,10)
	allocation = cbind(allocation, 100-allocation)/100
		
	out = NA * prices
	index = index[index>0]
	for(i in 1:len(index)) {
		if(index[i] < lookback[i]) next

		allocation.ret = ret[(index[i] - lookback[i] + 1) : index[i],] %*% t(allocation)
		
		allocation.sd = apply(allocation.ret, 2, sd)
		allocation.mean = apply(allocation.ret, 2, mean)
		metric = allocation.mean/(allocation.sd ^ f)

		j = which.max(metric)
		
		out[index[i],] = allocation[j,]
	}
	out
}

tic(10)
	weight = adaptive.weight(prices, index = month.ends)
toc(10)	
{% endhighlight %}



Elapsed time is 0.18 seconds

    




{% highlight r %}
	data$weight[] = NA
		data$weight[month.ends,] = weight[month.ends,]
	models$UIS = bt.run.share(data, trade.summary=T, clean.signal=F, silent=T)

	
tic(10)
	for(lookback in seq(50,80,5))
		for(f in seq(0.5,3,0.1))
			weight = weight + adaptive.weight(prices, lookback = lookback, f=f, index = month.ends)
toc(10)	
{% endhighlight %}



Elapsed time is 26.63 seconds

    




{% highlight r %}
	weight[month.ends,] = weight[month.ends,] / rowSums(weight[month.ends,])


	data$weight[] = NA
		data$weight[month.ends,] = weight[month.ends,]
	models$UISA = bt.run.share(data, trade.summary=T, clean.signal=F, silent=T)



	#*****************************************************************
	# Create Report
	#*****************************************************************
	plotbt(models, plotX = T, log = 'y', LeftMargin = 3, main = NULL)
		mtext('Cumulative Performance', side = 2, line = 1)
{% endhighlight %}

![plot of chunk plot-3](/public/images/2014-12-02-Logical-Invest-Strategy/plot-3-1.png) 

{% highlight r %}
	print(plotbt.strategy.sidebyside(models, make.plot=F, return.table=T))
{% endhighlight %}



|           |SPY               |equal.weight      |top1              |UIS               |UISA              |
|:----------|:-----------------|:-----------------|:-----------------|:-----------------|:-----------------|
|Period     |Jul2002 - Dec2014 |Jul2002 - Dec2014 |Jul2002 - Dec2014 |Jul2002 - Dec2014 |Jul2002 - Dec2014 |
|Cagr       |9.03              |8.89              |13.83             |12.78             |12.43             |
|Sharpe     |0.53              |0.98              |0.97              |1.24              |1.22              |
|DVR        |0.33              |0.85              |0.81              |1.09              |1.08              |
|Volatility |19.87             |9.14              |14.46             |10.09             |10.03             |
|MaxDD      |-55.19            |-24.68            |-17.05            |-15.76            |-17.12            |
|AvgDD      |-1.92             |-1.15             |-2.08             |-1.35             |-1.38             |
|VaR        |-1.85             |-0.88             |-1.43             |-0.95             |-0.94             |
|CVaR       |-3.01             |-1.3              |-2.03             |-1.44             |-1.44             |
|Exposure   |99.97             |99.94             |97.85             |97.2              |97.2              |
    




{% highlight r %}
	print(last.trades(models$top1, make.plot=F, return.table=T))	
{% endhighlight %}



|symbol |weight |entry.date |exit.date  |entry.price |exit.price |return |
|:------|:------|:----------|:----------|:-----------|:----------|:------|
|SPY    |100    |2013-04-30 |2013-05-31 |155.05      |158.71     |  2.36 |
|SPY    |100    |2013-05-31 |2013-06-28 |158.71      |156.59     | -1.34 |
|SPY    |100    |2013-06-28 |2013-07-31 |156.59      |164.68     |  5.17 |
|SPY    |100    |2013-07-31 |2013-08-30 |164.68      |159.75     | -3.00 |
|SPY    |100    |2013-08-30 |2013-09-30 |159.75      |164.80     |  3.16 |
|SPY    |100    |2013-09-30 |2013-10-31 |164.80      |172.43     |  4.63 |
|SPY    |100    |2013-10-31 |2013-11-29 |172.43      |177.54     |  2.96 |
|SPY    |100    |2013-11-29 |2013-12-31 |177.54      |182.15     |  2.59 |
|SPY    |100    |2013-12-31 |2014-01-31 |182.15      |175.73     | -3.52 |
|TLT    |100    |2014-01-31 |2014-02-28 |105.37      |105.92     |  0.52 |
|TLT    |100    |2014-02-28 |2014-03-31 |105.92      |106.70     |  0.74 |
|TLT    |100    |2014-03-31 |2014-04-30 |106.70      |108.93     |  2.09 |
|SPY    |100    |2014-04-30 |2014-05-30 |186.54      |190.87     |  2.32 |
|TLT    |100    |2014-05-30 |2014-06-30 |112.15      |111.87     | -0.25 |
|SPY    |100    |2014-06-30 |2014-07-31 |194.81      |192.19     | -1.34 |
|SPY    |100    |2014-07-31 |2014-08-29 |192.19      |199.78     |  3.95 |
|TLT    |100    |2014-08-29 |2014-09-30 |117.93      |115.44     | -2.11 |
|TLT    |100    |2014-09-30 |2014-10-31 |115.44      |118.69     |  2.82 |
|TLT    |100    |2014-10-31 |2014-11-28 |118.69      |122.21     |  2.97 |
|SPY    |100    |2014-11-28 |2014-12-05 |207.20      |208.00     |  0.39 |
    




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

![plot of chunk plot-3](/public/images/2014-12-02-Logical-Invest-Strategy/plot-3-2.png) 

{% highlight r %}
	print(plotbt.strategy.sidebyside(models1, make.plot=F, return.table=T))
{% endhighlight %}



|           |SPY               |equal.weight      |top1              |UIS               |UISA              |
|:----------|:-----------------|:-----------------|:-----------------|:-----------------|:-----------------|
|Period     |Jan2014 - Dec2014 |Jan2014 - Dec2014 |Jan2014 - Dec2014 |Jan2014 - Dec2014 |Jan2014 - Dec2014 |
|Cagr       |16.67             |20.38             |10.49             |18.88             |16.03             |
|Sharpe     |1.38              |3.28              |0.92              |2.82              |2.3               |
|DVR        |1.13              |3.18              |0.71              |2.72              |2.19              |
|Volatility |10.75             |5.54              |10.12             |5.93              |6.12              |
|MaxDD      |-7.27             |-2.67             |-5.37             |-2.76             |-3.7              |
|AvgDD      |-1.31             |-0.52             |-1.72             |-0.61             |-0.66             |
|VaR        |-1.14             |-0.58             |-1                |-0.58             |-0.63             |
|CVaR       |-1.69             |-0.77             |-1.4              |-0.83             |-0.9              |
|Exposure   |100               |100               |100               |100               |100               |
    




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

![plot of chunk plot-3](/public/images/2014-12-02-Logical-Invest-Strategy/plot-3-3.png) 

{% highlight r %}
	print(plotbt.strategy.sidebyside(models1, make.plot=F, return.table=T))
{% endhighlight %}



|           |SPY               |equal.weight      |top1              |UIS               |UISA              |
|:----------|:-----------------|:-----------------|:-----------------|:-----------------|:-----------------|
|Period     |Jan2010 - Dec2014 |Jan2010 - Dec2014 |Jan2010 - Dec2014 |Jan2010 - Dec2014 |Jan2010 - Dec2014 |
|Cagr       |15.36             |13.33             |19.35             |17.39             |16.59             |
|Sharpe     |1                 |1.82              |1.3               |1.99              |1.84              |
|DVR        |0.92              |1.78              |1.26              |1.96              |1.81              |
|Volatility |15.87             |7.09              |14.74             |8.37              |8.69              |
|MaxDD      |-18.61            |-7.08             |-12.02            |-6.46             |-6.75             |
|AvgDD      |-1.68             |-0.83             |-2                |-0.97             |-1.05             |
|VaR        |-1.61             |-0.71             |-1.45             |-0.79             |-0.82             |
|CVaR       |-2.44             |-0.99             |-2.01             |-1.19             |-1.23             |
|Exposure   |100               |100               |100               |100               |100               |
    

The idea for this Universal Investment Strategy was to develop a strategy which 
has an adaptive allocation between 0% and 100% for each ETF depending of the market situation.

The way to calculate the optimum composition is done by calculating which composition 
had the maximum Sharpe ratio during an optimized look back period (normally 50-80 days). 
During normal market periods, the maximum Sharpe ratio is not at a 100% SPY or 
at a 100% TLT allocation, but somewhere in between. 

To calculate this maximum Sharpe ratio, I loop through all possible compositions 
from 0%SPY-100%TLT to 100%SPY-0%TLT and calculate the resulting Sharpe ratio for 
the look back period.




Sample test of process:


{% highlight r %}
ret = prices / mlag(prices) - 1
	ret = coredata(ret)

i = dates2index(prices, '2014:07:21')
index = (i - 80 + 1) : i

f = 1

allocation = seq(0,100,5)
	allocation = cbind(allocation, 100-allocation)/100

allocation.ret = ret[index,] %*% t(allocation)

allocation.sd = apply(allocation.ret, 2, sd)
allocation.mean = apply(allocation.ret, 2, mean)

out = rbind(t(allocation), allocation.mean/(allocation.sd ^ f), sqrt(252)*allocation.sd, 252*allocation.mean)
	rownames(out) = spl('SPY,TLT,Sharpe,Volatility,Return')
	colnames(out) = out['SPY',]
out[,which.max(out['Sharpe',])]	
{% endhighlight %}

       SPY        TLT     Sharpe Volatility     Return 
0.55000000 0.45000000 0.27959204 0.04749188 0.21078728 


{% highlight r %}
out = round(100*out,1)


plot(out['SPY',],out['Sharpe',], type='l', col='black', 
	las=1, ylim=c(0,30), xlab='SPY Allocation', ylab='')
	lines(out['SPY',],out['Volatility',], type='l', col='orange')
{% endhighlight %}

![plot of chunk plot-4](/public/images/2014-12-02-Logical-Invest-Strategy/plot-4-1.png) 

{% highlight r %}
print(out)
{% endhighlight %}



|           |     0| 0.05|  0.1| 0.15|  0.2| 0.25|  0.3| 0.35|  0.4| 0.45|  0.5| 0.55|  0.6| 0.65|  0.7| 0.75|  0.8| 0.85|  0.9| 0.95|     1|
|:----------|-----:|----:|----:|----:|----:|----:|----:|----:|----:|----:|----:|----:|----:|----:|----:|----:|----:|----:|----:|----:|-----:|
|SPY        |   0.0|  5.0| 10.0| 15.0| 20.0| 25.0| 30.0| 35.0| 40.0| 45.0| 50.0| 55.0| 60.0| 65.0| 70.0| 75.0| 80.0| 85.0| 90.0| 95.0| 100.0|
|TLT        | 100.0| 95.0| 90.0| 85.0| 80.0| 75.0| 70.0| 65.0| 60.0| 55.0| 50.0| 45.0| 40.0| 35.0| 30.0| 25.0| 20.0| 15.0| 10.0|  5.0|   0.0|
|Sharpe     |  12.6| 13.7| 14.9| 16.3| 17.8| 19.6| 21.5| 23.5| 25.3| 26.9| 27.8| 28.0| 27.3| 26.0| 24.4| 22.6| 20.9| 19.3| 17.8| 16.5|  15.4|
|Volatility |   9.8|  9.1|  8.4|  7.7|  7.1|  6.5|  6.0|  5.5|  5.1|  4.9|  4.7|  4.7|  4.9|  5.2|  5.6|  6.0|  6.6|  7.2|  7.8|  8.5|   9.2|
|Return     |  19.6| 19.7| 19.8| 20.0| 20.1| 20.2| 20.4| 20.5| 20.7| 20.8| 20.9| 21.1| 21.2| 21.4| 21.5| 21.6| 21.8| 21.9| 22.1| 22.2|  22.3|
    







*(this report was produced on: 2014-12-07)*
