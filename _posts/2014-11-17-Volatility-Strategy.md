---
layout: post
title: Volatility Strategy 
---


Volatility Risk Premium (VRP)

A quick test of the results presented at:

* [DDNs Volatility Risk Premium Strategy Revisited (2)](http://www.tradingtheodds.com/2014/11/ddns-volatility-risk-premium-strategy-revisited-2/)
* [Volatility Risk Premium Trading Volatility (Part I)](http://www.tradingtheodds.com/2014/10/volatility-risk-premium-trading-volatility-part-i/)
* [Volatility Risk Premium: Sharpe 2+, Return to Drawdown 3+](http://quantstrattrader.wordpress.com/2014/11/14/volatility-risk-premium-sharpe-2-return-to-drawdown-3/)
* [Chasing the Volatility Risk Premium](http://volatilitymadesimple.com/chasing-the-volatility-risk-premium/)
* [Easy Volatility Investing by Tony Cooper at Double-Digit Numerics](http://papers.ssrn.com/sol3/papers.cfm?abstract_id=2255327)
* [MarketSci VIX & Volatility](https://marketsci.wordpress.com/category/vix-volatility/)
  + [Free Historical VXX Data](https://marketsci.wordpress.com/2012/04/18/free-historical-vxx-data/)
    - [Bloomberg](http://www.marketsci.com/supporting.docs/VXX.20120416.xlsx)
    - [Reconstruct using CBOE futures](http://www.marketsci.com/supporting.docs/Estimate%20VXX.xlsx)
    - [CBOE VIX Futures Historical Market Data](http://cfe.cboe.com/Products/historicalVIX.aspx)
    - [QMAO getSymbols.cfe() method that will do the adjustment for you by default (rescale=TRUE)](https://r-forge.r-project.org/scm/viewvc.php/pkg/qmao/R/getSymbols.cfe.R?view=markup&root=twsinstrument)
* [Reconstructing VXX from CBOE futures data ](http://tradingwithpython.blogspot.com/2012/01/reconstructing-vxx-from-cboe-futures.html)
> Do you have equivalent code to recreate historical VXZ data?
> Just read the VXZ brochure and adjust the calculation accordingly

[How to calculate the VXX price and how does backwardation and contango influence it](http://investing.kuchita.com/2011/08/16/how-the-vxx-is-calculated-and-why-backwardation-amplfies-it/)

Paid Data:
----
[Products](http://sixfigureinvesting.com/downloads-2/)

Strtegies:
-----
[A Failed Attempt at Backtesting Structural Arbitrage](http://quantstrattrader.wordpress.com/2014/10/02/a-failed-attempt-at-backtesting-structural-arbitrage/)
[Trading VXX with nearest neighbors prediction ](http://tradingwithpython.blogspot.ca/2014/11/trading-vxx-with-nearest-neighbors.html)

> Helmuth Vollmeier
> You can download a "long" XIV and "long" VXX back to the start of VIX futures 2004 from my Dropbox which is updated daily with CSI data via a small R-script.
> VXX: https://dl.dropboxusercontent.com/s/950x55x7jtm9x2q/VXXlong.TXT
> XIV: https://dl.dropboxusercontent.com/s/jk6der1s5lxtcfy/XIVlong.TXT

> Ilya , Samuel update: ZIV & VXZ, reconstructed according to the method outlined in their prospectus
> VXZ: https://www.dropbox.com/s/y3cg6d3vwtkwtqx/VXZlong.TXT
> ZIV: https://www.dropbox.com/s/jk3ortdyru4sg4n/ZIVlong.TXT



> Frank says:The rationale behind is very easy: 
> Sell (volatiity) risk when there is fear in the market:
>  (implied volatility overestimates realized volatility), and vice versa.
> Even the most simple strategy based on that concept:
> Go long XIV at todays close if the VIX index will close below the front month VIX futures contract, 
> or go long VXX if it will close above. Hold until a change in position.
> came up with 50+% annualized returns over the course of the last 10 years 
> (including the financial crisis and the most recent bull market).
> See http://volatilitymadesimple.com/backtest-comparing-the-vix-index-to-front-month-vix-futures/


Instruments:

* VIX - CBOE Volatility Index
* XIV (inverse volatility)(inverse 1-month VIX) - VelocityShares Daily Inverse VIX Short-Term ETN
* VXX (long volatility)(long 1-month VIX) - iPath S&P 500 VIX Short-Term Futures
* VXZ - iPath S&P 500 VIX Mid-Term Futures



Strategy rules:

* At the close, calculate the following: 
  + the 5-day average of the [VIX index (10 day historical volatility of SPY * 100)].
  Note that historical volatility is based on the natural log of each days % change.
* Go long XIV at the close when the result of the above formula is greater than 0,
  otherwise go long VXX. Hold until a change in position
      
  

<!--
# Imports
library(RCurl)
options(RCurlOptions = list(timeout = 10, useragent = "Mozilla/6.0 (Windows NT 6.2; WOW64; rv:16.0.1) Gecko/20121011 Firefox/16.0.1"))
buffer <- getURL("http://www.cboe.com/data/volatilityindexes/volatilityindexes.aspx")
--> 



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

Load post functions to be moved to SIT

{% highlight r %}
source('post.fn.r')
source('../ds.r')
{% endhighlight %}

Based on [Reconstructing VXX from CBOE futures data ](http://tradingwithpython.blogspot.com/2012/01/reconstructing-vxx-from-cboe-futures.html),
I was able to reconstruct VXX from CBOE futures. I made a helper function `reconstruct.VXX.CBOE`
to show steps needed to reconstruct VXX. 
I also made another helper function, `cboe.volatility.term.structure.SPX`, to download an plot VIX Term Structure.

For comparison I used historical VXX data from [Free Historical VXX Data](https://marketsci.wordpress.com/2012/04/18/free-historical-vxx-data/)
>1) From the raw VX contract data, there are 33 Month-2 data values that have missing values, How did you fill those values? I filled them by using the same Month-2 to Month-1 ratio as the first subsequent non-missing value.

I just put NA's


{% highlight r %}
  #*****************************************************************
  # Load historical data
  #****************************************************************** 
  load.packages('quantmod')
  tickers = spl('^VIX,VXX')    
                 
  data <- new.env()
getSymbols(tickers, src = 'yahoo', from = '1980-01-01', env = data, set.symbolnames = F, auto.assign = T)

	load.packages('gdata,data.table')
{% endhighlight %}

Next let's load historical data

{% highlight r %}
  # load data from [Free Historical VXX Data](https://marketsci.wordpress.com/2012/04/18/free-historical-vxx-data/)  
  load.packages('gdata')
  perl='c:/Michael_Kapler/Soft/perl/bin/perl.exe'
  temp = read.xls('data/VXX.20120416.xlsx', sheet=1, header=T,perl=perl)
{% endhighlight %}



{% highlight text %}
## Error in findPerl(perl, verbose = verbose): perl executable not found. Use perl= argument to specify the correct path.
{% endhighlight %}



{% highlight text %}
## Error in file.exists(tfn): invalid 'file' argument
{% endhighlight %}



{% highlight r %}
  data$VXX.HIST = make.stock.xts(make.xts(temp$VXX, as.POSIXct(temp$DATE)))
{% endhighlight %}



{% highlight text %}
## Error in as.POSIXct(temp$DATE): object 'temp' not found
{% endhighlight %}



{% highlight r %}
  # reconstruct VXX
  temp = reconstruct.VXX.CBOE()  
  data$VXX.CBOE = make.stock.xts(temp$VXX)

print(bt.start.dates(data))
{% endhighlight %}



|         |Start      |
|:--------|:----------|
|VIX      |1990-01-02 |
|VXX      |2009-01-30 |
|VXX.CBOE |2004-03-26 |


{% highlight r %}
    proxy.test(data)    
{% endhighlight %}

![plot of chunk plot-4](/public/images/2014-11-17-Volatility-Strategy/plot-4-1.png) 

|      |VIX    |VXX    |VXX.CBOE |
|:-----|:------|:------|:--------|
|VIX   |       |89%    |90%      |
|VXX   |       |       |97%      |
|      |       |       |         |
|Mean  | 39.4% |-79.4% |-75.3%   |
|StDev |112.9% | 60.7% | 66.4%   |




{% highlight r %}
    proxy.test(data, spl('VXX,VXX.CBOE'))    
{% endhighlight %}

![plot of chunk plot-4](/public/images/2014-11-17-Volatility-Strategy/plot-4-2.png) 

|      |VXX    |VXX.CBOE |
|:-----|:------|:--------|
|VXX   |       |97%      |
|      |       |         |
|Mean  |-79.4% |-75.3%   |
|StDev | 60.7% | 66.4%   |




{% highlight r %}
    plota(temp$VXX,type='l',lwd=2, main='VXX reconstructed from CBOE futures')
{% endhighlight %}

![plot of chunk plot-4](/public/images/2014-11-17-Volatility-Strategy/plot-4-3.png) 


{% highlight r %}
  tickers = spl('VXZ')    
                 
  data <- new.env()
getSymbols(tickers, src = 'yahoo', from = '1980-01-01', env = data, set.symbolnames = F, auto.assign = T)
  
  # reconstruct VXZ
  temp = reconstruct.VXX.CBOE()  
  data$VXZ.CBOE = make.stock.xts(temp$VXZ)
  
print(bt.start.dates(data))
{% endhighlight %}



|         |Start      |
|:--------|:----------|
|VXZ      |2009-02-20 |
|VXZ.CBOE |2004-03-26 |


{% highlight r %}
    proxy.test(data)    
{% endhighlight %}

![plot of chunk plot-5](/public/images/2014-11-17-Volatility-Strategy/plot-5-1.png) 

|      |VXZ    |VXZ.CBOE |
|:-----|:------|:--------|
|VXZ   |       |94%      |
|      |       |         |
|Mean  |-33.7% |-33.2%   |
|StDev | 30.5% | 31.2%   |




{% highlight r %}
plota(temp$VXZ,type='l',lwd=2, main='VXZ reconstructed from CBOE futures')  
{% endhighlight %}

![plot of chunk plot-5](/public/images/2014-11-17-Volatility-Strategy/plot-5-2.png) 


Finally let's plot VIX Term Structure.


{% highlight r %}
    print(cboe.volatility.term.structure.SPX())
{% endhighlight %}



{% highlight text %}
## Warning in readLines(url): incomplete final line found on
## 'http://www.cboe.com/data/volatilityindexes/volatilityindexes.aspx'
{% endhighlight %}

![plot of chunk plot-6](/public/images/2014-11-17-Volatility-Strategy/plot-6-1.png) 

|trade.date          |expiration.date |   vix| contract.month|
|:-------------------|:---------------|-----:|--------------:|
|2014-11-20 15:14:59 |2014-12-20      | 13.60|              1|
|2014-11-20 15:14:59 |2015-01-17      | 14.62|              2|
|2014-11-20 15:14:59 |2015-02-20      | 15.97|              3|
|2014-11-20 15:14:59 |2015-03-20      | 16.71|              4|
|2014-11-20 15:14:59 |2015-06-19      | 18.18|              5|
|2014-11-20 15:14:59 |2015-09-18      | 18.80|              6|
|2014-11-20 15:14:59 |2015-12-19      | 19.70|              7|
|2014-11-20 15:14:59 |2016-01-15      | 19.50|              8|
|2014-11-20 15:14:59 |2016-06-17      | 20.51|              9|
|2014-11-20 15:14:59 |2016-12-16      | 21.67|             10|

