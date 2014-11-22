---
layout: post
title: Thinking in SIT 
---


The best way to understand how SIT works, in my opinion, is to do a side by side strategy test
in Excel and one using SIT.

Let's do step by step in investigating following simple strategy:


* allocate 50% to Stock if it's prices is above 20 day moving average
* allocate 50% to Bond if it's prices is above 20 day moving average

To code this strategy in Excel, we for example can downloaded historical prices for SPY and TLT.
For simplicity, I generated in columns B and C Stock and Bond time series. To simulate each asset i used:

* Stock: 10% return and 20% volatility
* Bond: 4% return and 2% volatility

Next create columns D and E to hold 20 period moving averages for Stock and Bond. I.e. the formula
in column D is D21 =AVERAGE(B2:B21) and in column E is E21 = AVERAGE(C2:C21)

Next create columns F and G to hold signals. I.e. F21 = IF(B21>D21,0.5,0) and G21 = IF(C21>E21,0.5,0) 

To run back-test, we also need to compute asset returns, let column H and I hold Stock and Bond returns.
I.e. H3 =B3/B2-1 and I3 = C3/C2-1

Finally store strategy returns in column J. I.e. J21 = F21*H21+G21*I21 (RET = W.Stock * RET.Stock + W.Bond * RET.Bond)
and convert strategy returns into levels in column K. I.e. K3 = K2*(1+J3)


Let's for the moment think that Excel is one big matrix, in this case:

* columns B and C hold asset's prices
* columns D and E hold asset's 20 day moving averages
* columns F and G hold asset's signal. I.e. allocate 50% if prices > 20 day moving average
* columns H and I hold asset's returns
* column J holds strategy's return: RET = W.Stock * RET.Stock + W.Bond * RET.Bond
* column K holds strategy's price

[The sample spread sheet](/public/doc/2014-11-20-Thinking-SIT.xlsx )

To convert this matrix into SIT, we need:

1. download historical prices for SPY and TLT and align them so that dates on both series match
2. compute 20 day moving averages for both assets
3. compute signal: allocate 50%, if price > 20 day moving average
4. compute strategy's returns

First, let's load [Load Systematic Investor Toolbox (SIT)](http://systematicinvestor.wordpress.com/systematic-investor-toolbox/):


{% highlight r %}
###############################################################################
# Load Systematic Investor Toolbox (SIT)
# http://systematicinvestor.wordpress.com/systematic-investor-toolbox/
###############################################################################
if(!file.exists('../sit'))
shiny:::download('https://github.com/systematicinvestor/SIT/raw/master/sit.lite.gz', '../sit', mode = 'wb', quiet = TRUE)
con = gzcon(file('../sit', 'rb'))
source(con)
close(con)
{% endhighlight %}



Load historical data for SPY and TLT, and align it, so that dates on both time series match. We also adjust data for stock splits and dividends.


{% highlight r %}
	#*****************************************************************
	# Load historical data
	#*****************************************************************
	load.packages('quantmod')
	tickers = spl('SPY,TLT')

	data <- new.env()
	getSymbols(tickers, src = 'yahoo', from = '1970-01-01', env = data, auto.assign = T)
		for(i in ls(data)) data[[i]] = adjustOHLC(data[[i]], use.Adjusted=T)
	bt.prep(data, align='remove.na')
{% endhighlight %}

Next let's compute 20 day moving averages and signals. Please note that we are working with matrices below, so all computations are done
for both time series with just single statement.

{% highlight r %}
	prices = data$prices
	sma = bt.apply.matrix(prices, SMA, 20)
	signal = iif(prices > sma, 0.5, 0)
{% endhighlight %}

Now we ready to back-test our strategy:


{% highlight r %}
	#*****************************************************************
	# Code Strategies
	#*****************************************************************
	models = list()

	data$weight[] = NA
		data$weight[] = signal
	models$strategy = bt.run.share(data, clean.signal=F)
{% endhighlight %}



{% highlight text %}
## Latest weights :
## 
## Performance summary :
## 	CAGR	Best	Worst	
## 	2.9	3.6	-3.1	
{% endhighlight %}

and create reports

Create Report:

{% highlight r %}
	#*****************************************************************
	# Create Report
	#*****************************************************************
	plotbt(models, plotX = T, log = 'y', LeftMargin = 3, main = NULL)
		mtext('Cumulative Performance', side = 2, line = 1)
{% endhighlight %}

![plot of chunk plot-6](/public/images/2014-11-20-Thinking-SIT/plot-6-1.png) 

{% highlight r %}
	print(plotbt.strategy.sidebyside(models, make.plot=F, return.table=T))
{% endhighlight %}



|           |strategy          |
|:----------|:-----------------|
|Period     |Jul2002 - Nov2014 |
|Cagr       |2.88              |
|Sharpe     |0.45              |
|DVR        |0.37              |
|Volatility |6.88              |
|MaxDD      |-16.72            |
|AvgDD      |-1.47             |
|VaR        |-0.69             |
|CVaR       |-1                |
|Exposure   |88.65             |

Ok we done.

So what is easier Excel worksheet or R back-test, both take some time to get used to.
However, in the long run, R back-test will save you time because if you want to re-run it
another day, you don't need to worry about updating data and recallinh strategy rules.

The data is auto update and adjusted. The strategy rules are stored in code with your own comments.

