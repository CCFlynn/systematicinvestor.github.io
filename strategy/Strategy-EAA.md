---
layout: page
title: Elastic Asset Allocation Strategy
---


To install [Systematic Investor Toolbox (SIT)](https://github.com/systematicinvestor/SIT) please visit [About](/about) page.





The [Elastic Asset Allocation Strategy(EAA)](http://papers.ssrn.com/sol3/papers.cfm?abstract_id=2543979)
backtest and live signal. For more details please see [SSRN paper](http://papers.ssrn.com/sol3/papers.cfm?abstract_id=2543979)

The [EAA Strategy](http://papers.ssrn.com/sol3/papers.cfm?abstract_id=2543979)
 uses a geometrical weighted average of the historical returns, volatilities
and correlations, using elasticities as weights:

> asset's attractivness = [return ^ wR] * [(1-correlation) ^ wC] / [volatility ^ wV] 

where:
---

* return is average asset's return over last year using different timeframes
* correlation is asset's correlation with equal weight, market, index
* volatility is asset's historical volatility
* wR, wC, wV are user elasticities





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
EMERGING.MARKETS=EEM + VEIEX
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



|           | US.STOCKS| FOREIGN.STOCKS| EMERGING.MARKETS| US.10YR.GOV.BOND| REAL.ESTATE| COMMODITIES| CASH|
|:----------|---------:|--------------:|----------------:|----------------:|-----------:|-----------:|----:|
|2015-05-14 |    109.81|          51.96|            42.82|           105.97|       80.27|       18.46|   82|
    




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
# Code Strategy
#******************************************************************
ret = diff(log(prices))

n.top = 3

mom.lookback = 80
vol.lookback = 12*22
cor.lookback = 12*22

weight=list(r=1, v=0, c=0.5, s=2)

hist.vol = sqrt(252) * bt.apply.matrix(ret, runSD, n = vol.lookback)

#mean of cumulative returns of 1, 3, 6, and 12 month periods
mom = (prices / mlag(prices, 22)-1 + prices / mlag(prices, 3*22)-1
+ prices / mlag(prices, 6*22)-1 + prices / mlag(prices, 12 * 22)-1)/22

mkt.ret = rowMeans(ret, na.rm=T)

#*****************************************************************
# Compute Correlation to Market
#******************************************************************
mkt.cor = data$weight * NA
for(i in period.ends[period.ends > cor.lookback]){
  index = (i - cor.lookback):i
  hist = ret[index,]
    include.index = !is.na(colSums(hist))

  mkt.cor[i,include.index] = cor(hist[,include.index], mkt.ret[index], use='complete.obs',method='pearson')
}
{% endhighlight %}



{% highlight text %}
## Error in cor(hist[, include.index], mkt.ret[index], use = "complete.obs", : no complete element pairs
{% endhighlight %}



{% highlight r %}
avg.rank = (mom^weight$r * (1 - mkt.cor)^weight$c / hist.vol^weight$v) ^ weight$s
meta.rank = br.rank(avg.rank[period.ends,])

#absolute momentum filter
weight = (meta.rank <= n.top)/rowSums(meta.rank <= n.top, na.rm=T) * (mom[period.ends,] > 0)

# otherwise, it's weight is allocated to cash
weight$CASH = 1 - rowSums(weight,na.rm=T)

obj$weights$strategy = weight
{% endhighlight %}


![plot of chunk plot-6](/public/images/Strategy-EAA/plot-6-1.png) 

#Strategy Performance:
    




|              |strategy          |
|:-------------|:-----------------|
|Period        |May1996 - May2015 |
|Cagr          |5.37              |
|Sharpe        |1.18              |
|DVR           |1.16              |
|R2            |0.99              |
|Volatility    |4.55              |
|MaxDD         |-9.32             |
|Exposure      |99.71             |
|Win.Percent   |69.3              |
|Avg.Trade     |0.45              |
|Profit.Factor |3.02              |
|Num.Trades    |228               |
    


![plot of chunk plot-6](/public/images/Strategy-EAA/plot-6-2.png) 

#Monthly Results for strategy :
    




|     |Jan  |Feb  |Mar  |Apr  |May  |Jun  |Jul  |Aug  |Sep  |Oct  |Nov  |Dec  |Year |MaxDD |
|:----|:----|:----|:----|:----|:----|:----|:----|:----|:----|:----|:----|:----|:----|:-----|
|1996 |     |     |     |     |     | 1.3 | 0.2 |-0.2 | 1.7 | 2.2 | 1.7 |-0.9 | 6.2 |-1.8  |
|1997 | 0.2 | 0.1 |-1.0 | 1.5 | 0.9 | 1.2 | 2.8 |-0.9 | 1.4 | 1.4 | 0.3 | 1.0 | 9.3 |-2.3  |
|1998 | 1.3 |-0.1 | 0.3 | 0.5 | 1.0 | 0.8 | 0.2 | 1.9 | 2.0 |-0.6 | 0.5 | 0.4 | 8.5 |-2.7  |
|1999 | 0.8 |-1.8 | 0.6 | 0.4 |-0.9 |-0.5 |-0.4 |-0.1 | 1.3 | 0.3 | 0.0 |-0.5 |-0.8 |-3.9  |
|2000 |-0.2 | 1.2 | 1.4 |-0.5 |-0.2 | 2.1 | 0.9 | 1.4 | 0.8 | 0.6 | 1.6 | 1.8 |11.3 |-3.0  |
|2001 | 1.8 | 0.9 | 0.5 |-0.5 | 0.7 | 0.5 | 2.2 | 1.1 | 0.9 | 1.9 |-1.3 |-0.7 | 8.3 |-3.5  |
|2002 | 0.8 | 0.8 |-1.5 | 1.7 | 0.8 | 0.3 | 0.5 | 1.7 | 1.5 |-0.6 |-0.2 | 2.3 | 8.3 |-2.5  |
|2003 | 0.1 | 1.4 |-0.1 | 0.9 | 1.8 |-0.1 |-3.3 | 0.6 | 2.7 |-1.0 | 0.2 | 0.8 | 3.8 |-4.8  |
|2004 | 0.9 | 1.0 | 0.8 |-2.6 |-0.4 | 0.5 | 1.0 | 1.9 | 0.2 | 0.8 |-0.8 | 0.4 | 3.6 |-4.6  |
|2005 | 1.2 |-0.6 |-0.5 | 1.3 | 1.0 | 0.5 |-1.0 | 1.4 |-1.1 |-0.8 | 0.5 | 1.0 | 2.8 |-2.7  |
|2006 |-0.1 | 0.4 |-1.0 |-0.2 |-0.1 | 0.1 | 1.3 | 1.6 | 0.8 | 0.7 | 1.1 |-0.5 | 4.2 |-2.3  |
|2007 |-0.1 | 1.5 | 0.0 | 0.0 |-0.8 |-0.3 | 0.9 | 1.5 | 0.6 | 0.9 | 1.9 | 0.8 | 7.2 |-2.4  |
|2008 | 1.2 | 0.1 | 0.4 |-0.4 |-1.0 | 0.0 | 0.0 | 0.8 |-0.5 |-3.0 | 3.9 | 5.1 | 6.7 |-9.3  |
|2009 |-2.1 |-0.6 | 1.1 | 0.5 | 0.7 | 0.6 | 1.3 | 1.0 | 1.0 | 0.3 | 1.3 |-1.5 | 3.5 |-3.2  |
|2010 | 1.3 | 0.3 |-0.2 | 1.1 | 1.1 | 1.4 | 0.9 | 1.5 | 0.0 | 0.3 |-0.7 |-1.0 | 6.1 |-3.6  |
|2011 | 0.1 | 0.3 |-0.2 | 1.5 | 1.2 |-0.4 | 1.6 | 1.6 | 0.7 | 0.1 |-0.1 | 1.2 | 7.8 |-1.5  |
|2012 | 0.6 | 0.0 |-0.5 | 1.1 | 0.9 | 0.1 | 1.2 | 0.2 | 0.2 |-0.1 | 0.0 |-0.9 | 2.8 |-1.4  |
|2013 |-0.7 | 0.5 | 0.1 | 1.0 |-1.9 |-1.7 | 0.4 |-0.9 | 1.1 | 0.8 |-0.3 |-0.6 |-2.2 |-5.2  |
|2014 | 1.5 | 0.5 |-0.2 | 0.8 | 1.0 | 0.1 |-0.3 | 1.1 |-0.6 | 0.7 | 0.8 | 0.0 | 5.7 |-1.2  |
|2015 | 2.4 |-1.3 | 0.5 |-0.3 |-1.1 |     |     |     |     |     |     |     | 0.1 |-2.4  |
|Avg  | 0.6 | 0.2 | 0.0 | 0.4 | 0.3 | 0.3 | 0.5 | 0.9 | 0.8 | 0.3 | 0.6 | 0.4 | 5.2 |-3.2  |
    


![plot of chunk plot-6](/public/images/Strategy-EAA/plot-6-3.png) ![plot of chunk plot-6](/public/images/Strategy-EAA/plot-6-4.png) 

#Trades for strategy :
    




|strategy |weight |entry.date |exit.date  |nhold |entry.price |exit.price |return |
|:--------|:------|:----------|:----------|:-----|:-----------|:----------|:------|
|CASH     |100    |2013-09-30 |2013-10-31 |31    |77.42       |78.08      | 0.86  |
|CASH     |100    |2013-10-31 |2013-11-29 |29    |78.08       |77.85      |-0.29  |
|CASH     |100    |2013-11-29 |2013-12-31 |32    |77.85       |77.36      |-0.64  |
|CASH     |100    |2013-12-31 |2014-01-31 |31    |77.36       |78.55      | 1.55  |
|CASH     |100    |2014-01-31 |2014-02-28 |28    |78.55       |78.92      | 0.47  |
|CASH     |100    |2014-02-28 |2014-03-31 |31    |78.92       |78.79      |-0.17  |
|CASH     |100    |2014-03-31 |2014-04-30 |30    |78.79       |79.42      | 0.80  |
|CASH     |100    |2014-04-30 |2014-05-30 |30    |79.42       |80.26      | 1.05  |
|CASH     |100    |2014-05-30 |2014-06-30 |31    |80.26       |80.32      | 0.08  |
|CASH     |100    |2014-06-30 |2014-07-31 |31    |80.32       |80.10      |-0.28  |
|CASH     |100    |2014-07-31 |2014-08-29 |29    |80.10       |81.02      | 1.14  |
|CASH     |100    |2014-08-29 |2014-09-30 |32    |81.02       |80.55      |-0.57  |
|CASH     |100    |2014-09-30 |2014-10-31 |31    |80.55       |81.13      | 0.72  |
|CASH     |100    |2014-10-31 |2014-11-28 |28    |81.13       |81.81      | 0.83  |
|CASH     |100    |2014-11-28 |2014-12-31 |33    |81.81       |81.86      | 0.06  |
|CASH     |100    |2014-12-31 |2015-01-30 |30    |81.86       |83.82      | 2.40  |
|CASH     |100    |2015-01-30 |2015-02-27 |28    |83.82       |82.73      |-1.31  |
|CASH     |100    |2015-02-27 |2015-03-31 |32    |82.73       |83.18      | 0.55  |
|CASH     |100    |2015-03-31 |2015-04-30 |30    |83.18       |82.91      |-0.32  |
|CASH     |100    |2015-04-30 |2015-05-14 |14    |82.91       |82.00      |-1.10  |
    




#Signals for strategy :
    




|           | US.STOCKS| FOREIGN.STOCKS| EMERGING.MARKETS| US.10YR.GOV.BOND| REAL.ESTATE| COMMODITIES| CASH|
|:----------|---------:|--------------:|----------------:|----------------:|-----------:|-----------:|----:|
|2013-09-27 |          |               |                 |                 |            |            |  100|
|2013-10-30 |          |               |                 |                 |            |            |  100|
|2013-11-27 |          |               |                 |                 |            |            |  100|
|2013-12-30 |          |               |                 |                 |            |            |  100|
|2014-01-30 |          |               |                 |                 |            |            |  100|
|2014-02-27 |          |               |                 |                 |            |            |  100|
|2014-03-28 |          |               |                 |                 |            |            |  100|
|2014-04-29 |          |               |                 |                 |            |            |  100|
|2014-05-29 |          |               |                 |                 |            |            |  100|
|2014-06-27 |          |               |                 |                 |            |            |  100|
|2014-07-30 |          |               |                 |                 |            |            |  100|
|2014-08-28 |          |               |                 |                 |            |            |  100|
|2014-09-29 |          |               |                 |                 |            |            |  100|
|2014-10-30 |          |               |                 |                 |            |            |  100|
|2014-11-26 |          |               |                 |                 |            |            |  100|
|2014-12-30 |          |               |                 |                 |            |            |  100|
|2015-01-29 |          |               |                 |                 |            |            |  100|
|2015-02-26 |          |               |                 |                 |            |            |  100|
|2015-03-30 |          |               |                 |                 |            |            |  100|
|2015-04-29 |          |               |                 |                 |            |            |  100|
    







For your convenience, the 
[Strategy-EAA](/public/images/Strategy-EAA/Strategy-EAA.pdf)
report can also be downloaded and viewed the pdf format.



*(this report was produced on: 2015-05-15)*
