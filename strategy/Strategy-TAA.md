---
layout: page
title: Quantitative Approach To Tactical Asset Allocation Strategy
---


To install [Systematic Investor Toolbox (SIT)](https://github.com/systematicinvestor/SIT) please visit [About](/about) page.





The [Quantitative Approach To Tactical Asset Allocation Strategy(QATAA) by Mebane T. Faber](http://mebfaber.com/timing-model/)
backtest and live signal. For more details please see [SSRN paper](http://papers.ssrn.com/sol3/papers.cfm?abstract_id=962461)

The [QATAA Strategy](http://mebfaber.com/timing-model/)
allocates 20% across 5 asset classes:

* US Stocks
* Foreign Stocks
* US 10YR Government Bonds
* Real Estate
* Commodities

If asset is above it's 10 month moving average it gets 20% allocation; otherwise, it's weight is
allocated to cash. The re-balancing process is done Monthly.

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
US.STOCKS = VTI + VTSMX
FOREIGN.STOCKS = VEU + FDIVX
US.10YR.GOV.BOND = IEF + VFITX
REAL.ESTATE = VNQ + VGSIX
COMMODITIES = DBC + CRB
CASH = BND + VBMFX
'

# load saved Proxies Raw Data, data.proxy.raw
load('data.proxy.raw.Rdata')

data <- new.env()

getSymbols.extra(tickers, src = 'yahoo', from = '1970-01-01', env = data, raw.data = data.proxy.raw, auto.assign = T, set.symbolnames = T, getSymbols.fn = getSymbols.fn, calendar=calendar)
  for(i in data$symbolnames) data[[i]] = adjustOHLC(data[[i]], use.Adjusted=T)
bt.prep(data, align='remove.na', dates='::')

print(last(data$prices))
{% endhighlight %}



|           | US.STOCKS| FOREIGN.STOCKS| US.10YR.GOV.BOND| REAL.ESTATE| COMMODITIES|  CASH|
|:----------|---------:|--------------:|----------------:|-----------:|-----------:|-----:|
|2016-01-08 |     97.97|          40.45|           107.15|       77.46|        12.8| 81.29|
    




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



{% highlight text %}
## Error in business.days.location.end(data$dates, input$calendar, fn.ends = date.ends.fn(input$period)): unused argument (fn.ends = date.ends.fn(input$period))
{% endhighlight %}



{% highlight text %}
## Error in date.ends.index(out, out$signal.timing): object 'out' not found
{% endhighlight %}



{% highlight text %}
## Error in eval(expr, envir, enclos): object 'period.ends' not found
{% endhighlight %}


Code Strategy Rules:



{% highlight r %}
#*****************************************************************
# Code Strategy
#******************************************************************
sma = bt.apply.matrix(prices, SMA, 200)

# If asset is above it's 10 month moving average it gets 20% allocation
weight = iif(prices > sma, 20/100, 0)

# otherwise, it's weight is allocated to cash
weight$CASH = 1 - rowSums(weight)

obj$weights$strategy = weight[period.ends,]
{% endhighlight %}



{% highlight text %}
## Error in `[.xts`(weight, period.ends, ): object 'period.ends' not found
{% endhighlight %}



{% highlight text %}
## Error in business.days.location.end(data$dates, input$calendar, fn.ends = date.ends.fn(input$period)): unused argument (fn.ends = date.ends.fn(input$period))
{% endhighlight %}



{% highlight text %}
## Error in out[[1]][[1]]: subscript out of bounds
{% endhighlight %}



{% highlight text %}
## Error in out[[1]][[1]]: subscript out of bounds
{% endhighlight %}




For your convenience, the 
[Strategy-TAA](/public/images/Strategy-TAA/Strategy-TAA.pdf)
report can also be downloaded and viewed the pdf format.





















*(this report was produced on: 2016-01-11)*
