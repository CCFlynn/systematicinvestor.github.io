---
layout: post
title: Loading Historical Stock Data
---

Historical Stock Data is critical for testing your investment strategies. There are many ways to load Historical Stock Data in to your R session. Below I show 4 different approaches to load historical stock data:

* Download Historical Stock quotes from Yahoo Fiance with getSymbols function from quantmod package
* Load Historical Stock data from the csv files you saved from Yahoo Fiance
* Load Historical Stock data from your custom files
* Load Historical Stock from one csv file, where each column represents one stock

In the code below I demonstrate these 4 methods and use loaded data to run a back-test: 

Load [Load Systematic Investor Toolbox (SIT)](http://systematicinvestor.wordpress.com/systematic-investor-toolbox/):


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

Loading Historical Prices:

{% highlight r %}
	#*****************************************************************
	# Load historical data
	#****************************************************************** 
	load.packages('quantmod')
  
	stock.folder = 'c:\\Stocks\\Data\\'
	tickers = spl('UUP,EMB,HYG')
    
	data <- new.env()
        
	# load historical data, select data load method
	data.load.method = 'basic'	
  
	if(data.load.method == 'basic') {       
        # quantmod - getSymbols
        getSymbols(tickers, src = 'yahoo', from = '1970-01-01', env = data, auto.assign = T)
	}else if(data.load.method == 'basic.local') {
        # if you saved yahoo historical price files localy
        getSymbols.sit(tickers, src = 'yahoo', from = '1980-01-01', env = data, auto.assign = T, stock.folder = stock.folder)
	}else if(data.load.method == 'custom.local') {
        # custom format historical price files
        for(n in tickers) {
            data[[n]] = read.xts(paste(stock.folder, n, '.csv', sep=''), format='%m/%d/%Y')
        }   
	}else if(data.load.method == 'custom.one.file') {
        # read from one csv file, column headers are tickers
        filename = 'hex.csv'
        all.data = read.xts(paste(stock.folder, filename, sep=''), format='%m/%d/%Y')
        for(n in names(all.data)) {
            data[[n]] = all.data[,n]
            colnames(data[[n]]) = 'Close'
            data[[n]]$Adjusted = data[[n]]$Open = data[[n]]$High = data[[n]]$Low = data[[n]]$Close
        }
	}       
            
	# prepare data for back test
		for(i in ls(data)) data[[i]] = adjustOHLC(data[[i]], use.Adjusted=T)                            
	bt.prep(data, align='remove.na')
 
   
	#*****************************************************************
	# Code Strategies
	#******************************************************************
	prices = data$prices  
	n = ncol(prices)
   
	models = list()
   
	# find period ends
	period.ends = endpoints(prices, 'months')
		period.ends = period.ends[period.ends > 0]
       
	obj = portfolio.allocation.helper(data$prices, period.ends=period.ends, lookback.len = 250, 
		min.risk.fns = list(EW=equal.weight.portfolio,
                        RP=risk.parity.portfolio(),
                        MV=min.var.portfolio,
                        MC=min.corr.portfolio)
	) 
    
	models = create.strategies(obj, data)$models
{% endhighlight %}

Finally let's make a report:
              

{% highlight r %}
	#*****************************************************************
	# Create Report
	#******************************************************************        
	plotbt(models, plotX = T, log = 'y', LeftMargin = 3, main = NULL)	    	
		mtext('Cumulative Performance', side = 2, line = 1)
{% endhighlight %}

![plot of chunk plot-3](/public/images/2014-11-13-Loading-Historical-Stock-Data/plot-3-1.png) 

{% highlight r %}
	print(plotbt.strategy.sidebyside(models, make.plot=F, return.table=T))
{% endhighlight %}



|           |EW                |RP                |MV                |MC                |
|:----------|:-----------------|:-----------------|:-----------------|:-----------------|
|Period     |Dec2007 - Nov2014 |Dec2007 - Nov2014 |Dec2007 - Nov2014 |Dec2007 - Nov2014 |
|Cagr       |5.44              |4.49              |2.99              |1.93              |
|Sharpe     |1.11              |0.98              |0.67              |0.41              |
|DVR        |1.06              |0.93              |0.61              |0.29              |
|Volatility |4.87              |4.59              |4.56              |4.95              |
|MaxDD      |-11.06            |-7.77             |-6.59             |-8.14             |
|AvgDD      |-0.79             |-0.81             |-0.9              |-1.4              |
|VaR        |-0.41             |-0.41             |-0.41             |-0.48             |
|CVaR       |-0.74             |-0.72             |-0.71             |-0.74             |
|Exposure   |84.97             |84.97             |84.97             |84.97             |
