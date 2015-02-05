---
layout: post
title: Synthetic Volatility Index
comments: true
---


To install [Systematic Investor Toolbox (SIT)](https://github.com/systematicinvestor/SIT) please visit [About](',base.url,'about) page.





There is an interesting series of articles about Synthetic Volatility Index
at [QuantLab](https://quantlab.co.za/blog/blog.php)

* [Synthetic Volatility Index Quantified  Part 3](https://quantlab.co.za/blog/blog.php?controller=post&action=view&id_post=35)
* [Engineering a Synthetic Volatility Index  Part 2](https://quantlab.co.za/blog/blog.php?controller=post&action=view&id_post=34)
* [Engineering a Synthetic Volatility Index  Part 1](https://quantlab.co.za/blog/blog.php?controller=post&action=view&id_post=33)

The objective set by [QuantLab](https://quantlab.co.za/blog/blog.php) is to
create Synthetic Volatility Index that
1) when applied to the S&P 500 mirrors the VIX index as closely as possible and 
2) relies solely on price as an input so that it can be applied to any market index

The solution outlined is the Synthetic Volatility Index: 
> Mov(ATR(1)/C,20,S)

Below I will try to adapt a code from the posts:


{% highlight r %}
#*****************************************************************
# Load historical data
#*****************************************************************
library(SIT)
load.packages('quantmod')

tickers = 'SP=^GSPC,VIX=^VIX'

data <- new.env()
getSymbols.extra(tickers, src = 'yahoo', from = '1970-01-01', env = data, auto.assign = T)
for(i in ls(data)) data[[i]] = adjustOHLC(data[[i]], use.Adjusted=T)
bt.prep(data, align='remove.na', fill.gaps = T)

#*****************************************************************
# Plot daya
#*****************************************************************
layout(1:3)
plota(data$SP, type='l', plotX=F)
	plota.legend('SP','black',data$SP)
  
plota(data$VIX, type='l', plotX=F)
	plota.legend('VIX','black',data$VIX)

vix.proxy = SMA( ATR(HLC(data$SP),1)[,"atr"] / Cl(data$SP), 20 )
cor.vix = cor(vix.proxy, Cl(data$VIX), use='complete.obs',method='pearson')
plota(vix.proxy, type='l', main=paste('Correlation with VIX:', to.percent(cor.vix)))
	plota.legend('ATR 20','black',vix.proxy)
{% endhighlight %}

![plot of chunk plot-2](/public/images/2015-02-03-Synthetic-Volatility-Index/plot-2-1.png) 

{% highlight r %}
layout(1)
temp = na.omit(cbind(Cl(data$VIX),vix.proxy))
plota.matplot(scale.one(temp))
{% endhighlight %}

![plot of chunk plot-2](/public/images/2015-02-03-Synthetic-Volatility-Index/plot-2-2.png) 

{% highlight r %}
#*****************************************************************
# Test strategy
#*****************************************************************
vol.proxy = SMA( ATR(HLC(data$SP),1)[,"atr"] / Cl(data$SP), 20 )
high.vol.regime = vol.proxy > SMA(vol.proxy, 40)
low.vol.regime = vol.proxy < SMA(vol.proxy, 40)

layout(1:2)
index = '2013:10::'

highlight = high.vol.regime[index]

plota(data$SP[index], type='l', plotX=F, x.highlight = highlight)
  plota.legend('SP,HIGH VOL','black,orange')
plota(vol.proxy[index], type='l', x.highlight = highlight)
  plota.lines(SMA(vol.proxy, 40), col='blue')
  plota.legend('SP VOL PROXY, 40SMA','black,blue')
{% endhighlight %}

![plot of chunk plot-2](/public/images/2015-02-03-Synthetic-Volatility-Index/plot-2-3.png) 

{% highlight r %}
#*****************************************************************
# Test strategy
#*****************************************************************
models = list()

data$weight[] = NA
	data$weight$SP = 1
models$SP = bt.run.share(data, clean.signal=T)
{% endhighlight %}

Latest weights :


|           |  SP| VIX|
|:----------|---:|---:|
|2015-02-04 | 100|   0|
    



Performance summary :
	CAGR	Best	Worst	
	7.2	11.6	-9	



{% highlight r %}
data$weight[] = NA
	data$weight$SP = iif(low.vol.regime, 1, 0)
models$low.vol = bt.run.share(data, clean.signal=T)
{% endhighlight %}

Latest weights :


|           | SP| VIX|
|:----------|--:|---:|
|2015-02-04 |  0|   0|
    



Performance summary :
	CAGR	Best	Worst	
	1.2	6.5	-8.9	



{% highlight r %}
#*****************************************************************
# Report
#*****************************************************************
#strategy.performance.snapshoot(models, T)
layout(1)
plotbt(models, plotX = T, log = 'y', LeftMargin = 3, main = NULL)
	mtext('Cumulative Performance', side = 2, line = 1)
{% endhighlight %}

![plot of chunk plot-2](/public/images/2015-02-03-Synthetic-Volatility-Index/plot-2-4.png) 

{% highlight r %}
print(plotbt.strategy.sidebyside(models, make.plot=F, return.table=T))
{% endhighlight %}



|           |SP                |low.vol           |
|:----------|:-----------------|:-----------------|
|Period     |Jan1990 - Feb2015 |Jan1990 - Feb2015 |
|Cagr       |7.16              |1.21              |
|Sharpe     |0.47              |0.16              |
|DVR        |0.34              |0                 |
|Volatility |18.11             |11.51             |
|MaxDD      |-56.78            |-52.86            |
|AvgDD      |-2.5              |-3.79             |
|VaR        |-1.74             |-1.11             |
|CVaR       |-2.69             |-1.89             |
|Exposure   |99.98             |51.91             |
    

The estimate is similar to other volatility estimates available from TTR package:


{% highlight r %}
ohlc = OHLC(data$SP)
vClose = volatility(ohlc, n=20, calc="close")
vGK = volatility(ohlc, n=20, calc="garman")
vParkinson = volatility(ohlc, n=20, calc="parkinson")
vRS = volatility(ohlc, n=20, calc="rogers")

VIX = Cl(data$VIX)
temp = data.frame(vix.proxy, vClose, vGK, vParkinson, vRS, VIX)

print(to.percent(cor(temp, use='complete.obs',method='pearson')))
{% endhighlight %}



|           |atr.SMA.20 |vClose  |vGK     |vParkinson |vRS     |Close   |
|:----------|:----------|:-------|:-------|:----------|:-------|:-------|
|atr.SMA.20 |100.00%    | 97.75% | 99.11% | 99.58%    | 97.70% | 91.46% |
|vClose     | 97.75%    |100.00% | 95.71% | 98.01%    | 92.93% | 88.75% |
|vGK        | 99.11%    | 95.71% |100.00% | 99.42%    | 99.51% | 90.91% |
|vParkinson | 99.58%    | 98.01% | 99.42% |100.00%    | 98.04% | 91.07% |
|vRS        | 97.70%    | 92.93% | 99.51% | 98.04%    |100.00% | 90.03% |
|Close      | 91.46%    | 88.75% | 90.91% | 91.07%    | 90.03% |100.00% |
    




*(this report was produced on: 2015-02-05)*
