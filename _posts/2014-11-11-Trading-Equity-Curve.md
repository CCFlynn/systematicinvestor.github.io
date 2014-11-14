---
layout: post
title: Trading Equity Curve 
---

A quick test of the results presented at [Equity Curve Money Management](http://jonathankinlay.com/index.php/2014/11/equity-curve-money-management/). 

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

Load Historical Prices from Yahoo Finance:


{% highlight r %}
  #*****************************************************************
  # Load historical data
  #****************************************************************** 
  load.packages('quantmod')	
  tickers = spl('SPY')	    
                 
  data <- new.env()       
	getSymbols(tickers, src = 'yahoo', from = '1980-01-01', env = data, set.symbolnames = F, auto.assign = T)
    for(i in ls(data)) data[[i]] = adjustOHLC(data[[i]], use.Adjusted=T)		
  bt.prep(data, align='remove.na')
{% endhighlight %}

Look at sample strategy that goes long if [SPY](https://finance.yahoo.com/q?s=SPY) is above it's 20 day moving average; and short otherwise:


{% highlight r %}
  #*****************************************************************
  # Code Strategies
  #****************************************************************** 
  prices = data$prices  
  n = len(tickers)  

  sma.20 = bt.apply.matrix(prices, SMA, 20)  
	sma.fast = SMA(prices, 50)
	sma.slow = SMA(prices, 200)
  signal = sma.fast >= sma.slow

  models = list()

  #*****************************************************************
  # Code Strategies
  #******************************************************************
if(F) {
  data$weight[] = NA
    data$weight$SPY = 1
  models$SPY = bt.run.share(data, silent=T, clean.signal=T)
}
  
  data$weight[] = NA
    data$weight$SPY = iif(signal$SPY, 1, -1)
  models$SPY.CROSS = bt.run.share(data, silent=T, clean.signal=T)

  equity = models$SPY.CROSS$equity     
  stat = BBands(equity, 50, sd = 2)
  equity.ma = stat$mavg
  lower.equity = stat$dn 
             
  
  
  levergae = iif(cross.up(equity,lower.equity), 1.9, 
                iif(cross.up(equity,equity.ma), 1, 
            NA))
    # make sure to add [], otherwise dates are mixed up
    levergae[] = ifna.prev(levergae)
    
  data$weight[] = NA
    data$weight$SPY = iif(signal$SPY, 1, -1) * levergae 
  models$SPY.20.EQUITY = bt.run.share(data, silent=T, clean.signal=T)
  
  #*****************************************************************
  # Create Report
  #****************************************************************** 
  #strategy.performance.snapshoot(models, T)
  plotbt(models, plotX = T, log = 'y', LeftMargin = 3, main = NULL)	    	
    mtext('Cumulative Performance', side = 2, line = 1)
{% endhighlight %}

![plot of chunk plot-3](/public/images/2014-11-11-Trading-Equity-Curve/plot-3-1.png) 

{% highlight r %}
layout(1:2)    
  for(m in names(models))
	plotbt.transition.map(models[[m]]$weight, name=m)
		legend('topright', legend = m, bty = 'n')
{% endhighlight %}

![plot of chunk plot-3](/public/images/2014-11-11-Trading-Equity-Curve/plot-3-2.png) 

{% highlight r %}
  print(plotbt.strategy.sidebyside(models, make.plot=F, return.table=T))
{% endhighlight %}



|           |SPY.CROSS         |SPY.20.EQUITY     |
|:----------|:-----------------|:-----------------|
|Period     |Jan1993 - Nov2014 |Jan1993 - Nov2014 |
|Cagr       |10.15             |12.76             |
|Sharpe     |0.68              |0.7               |
|DVR        |0.62              |0.62              |
|Volatility |16.16             |20.1              |
|MaxDD      |-35.53            |-53.37            |
|AvgDD      |-2.43             |-2.38             |
|VaR        |-1.62             |-1.85             |
|CVaR       |-2.36             |-2.99             |
|Exposure   |99.98             |98.98             |

There is small improvement, but at what cost?
