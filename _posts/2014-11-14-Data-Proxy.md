---
layout: post
title: Data Proxy - extending time series with proxies
---


To install [Systematic Investor Toolbox (SIT)](https://github.com/systematicinvestor/SIT) please visit [About](',base.url,'about) page.




This page will hold collection of Proxies I collected to extend historical time series.

Note: there are more examples at
[Composing Synthetic Prices For Extended Historical ETF Data](http://indexswingtrader.blogspot.ca/2014/05/composing-synthetic-prices-for-extended.html)



<!-- open collapse div, http://stackoverflow.com/questions/15917463/embedding-markdown-in-jekyll-html 
<a href="#self" class="acollapse">(hide)</a>

<button>-</button>hide
<div markdown="1">

<button>+</button>show
<div markdown="1" style="display:none;">   

<input type="button" value="-">Commodities:
-->




<input type="button" class="btn btn-sm" value="-">Commodities:
----

<div markdown="1">

{% highlight r %}
    #*****************************************************************
    # Load external data
    #******************************************************************   
    library(SIT)
    load.packages('quantmod')  

raw.data <- new.env()
    
    # TRJ_CRB file was downloaded from the http://www.corecommodityllc.com/CoreCommodity-Indexes/1cc/681
    # for TRJ/CRB Index-Total Return
    temp = extract.table.from.webpage( join(readLines("data/TR_CC-CRB")), 'EODValue' )
    temp = join( apply(temp, 1, join, ','), '\n' )
    raw.data$CRB = make.stock.xts( read.xts(temp, format='%m/%d/%y' ) )
     
    # prfmdata.csv file was downloaded from the http://www.crbequityindexes.com/indexdata-form.php
    # for "TR/J CRB Global Commodity Equity Index", "Total Return", "All Dates"
    raw.data$CRB_2 = make.stock.xts( read.xts('data/prfmdata.csv', format='%m/%d/%Y' ) )

    #*****************************************************************
    # Load historical data
    #******************************************************************   
    load.packages('quantmod')  
        
    tickers = spl('GSG,DBC')
    data = new.env()
    
    data$CRB = raw.data$CRB
    data$CRB_2 = raw.data$CRB_2
    
    getSymbols(tickers, src = 'yahoo', from = '1970-01-01', env = data, auto.assign = T)   
        for(i in ls(data)) data[[i]] = adjustOHLC(data[[i]], use.Adjusted=T)
{% endhighlight %}

Look at historical start date for each series:
 

{% highlight r %}
print(bt.start.dates(data))
{% endhighlight %}



|      |Start      |
|:-----|:----------|
|CRB   |1994-01-03 |
|CRB_2 |1999-12-31 |
|DBC   |2006-02-06 |
|GSG   |2006-07-21 |
    

There are 2 sources of historical commodity index data. Let's compare them with commodity ETFs.


{% highlight r %}
    proxy.test(data)
{% endhighlight %}

![plot of chunk plot-4](/public/images/2014-11-14-Data-Proxy/plot-4-1.png) 

|      |CRB   |CRB_2 |DBC   |GSG   |
|:-----|:-----|:-----|:-----|:-----|
|CRB   |      |66%   |89%   |88%   |
|CRB_2 |      |      |66%   |63%   |
|DBC   |      |      |      |90%   |
|      |      |      |      |      |
|Mean  |-0.1% | 9.5% | 0.9% |-4.4% |
|StDev |19.1% |26.6% |21.0% |25.0% |
    

The historical commodity index data from [crbequityindexes](http://www.crbequityindexes.com/indexdata-form.php)
, that I denoted CRB_2, looks very different over the common interval for all proxies


{% highlight r %}
    proxy.overlay.plot(data)
{% endhighlight %}

![plot of chunk plot-5](/public/images/2014-11-14-Data-Proxy/plot-5-1.png) 

On the all history chart CRB_2 is also different.



{% highlight r %}
    proxy.prices(data)  
{% endhighlight %}

![plot of chunk plot-6](/public/images/2014-11-14-Data-Proxy/plot-6-1.png) 

|      |CRB Price |CRB Total |CRB_2 Price |CRB_2 Total |DBC Price |DBC Total |GSG Price |GSG Total |
|:-----|:---------|:---------|:-----------|:-----------|:---------|:---------|:---------|:---------|
|Mean  | 8.2%     | 8.2%     |11.5%       |11.5%       |-0.8%     |-0.8%     |-7.6%     |-7.6%     |
|StDev |16.3%     |16.3%     |21.5%       |21.5%       |20.9%     |20.9%     |24.9%     |24.9%     |
    

Quick glance at historical time series does not show anything abnormal between Price and Adjusted Price series. 
    

{% highlight r %}
    tickers = spl('DBC, DBC.CRB=DBC+CRB')
    proxy.map(data, tickers)      
{% endhighlight %}

![plot of chunk plot-7](/public/images/2014-11-14-Data-Proxy/plot-7-1.png) 





</div>
            
Please use `CRB` to extend Commodities.


        
<input type="button" class="btn btn-sm" value="-">REIT ex-U.S.: 
---- 
(Dow Jones Global **ex-U.S.** Real Estate Securities Index)


<div markdown="1">
    

{% highlight r %}
create.proxy = function(tickers, proxy.map.tickers, raw.data = new.env()) {
    #*****************************************************************
    # Load historical data
    #******************************************************************   
    load.packages('quantmod')  
    
    tickers = spl(tickers)
    data = new.env()
    
	getSymbols.extra(tickers, src = 'yahoo', from = '1970-01-01', env = data, raw.data = raw.data, auto.assign = T)    
        for(i in ls(data)) data[[i]] = adjustOHLC(data[[i]], use.Adjusted=T)
                         
    #*****************************************************************
    # Compare
    #******************************************************************
    print(bt.start.dates(data))
    
    proxy.test(data)    
    
    proxy.overlay.plot(data)   
    
    proxy.prices(data)

    tickers = spl(proxy.map.tickers)
    proxy.map(data, tickers)
}
    

	create.proxy('RWX,VNQ,VGSIX', 'RWX, RWX.VNQ=RWX+VNQ, RWX.VNQ.VGSIX=RWX+VNQ+VGSIX')
{% endhighlight %}



|      |Start      |
|:-----|:----------|
|VGSIX |1996-06-28 |
|VNQ   |2004-10-01 |
|RWX   |2007-03-02 |
    


![plot of chunk plot-9](/public/images/2014-11-14-Data-Proxy/plot-9-1.png) 

|      |RWX   |VGSIX |VNQ   |
|:-----|:-----|:-----|:-----|
|RWX   |      |66%   |67%   |
|VGSIX |      |      |99%   |
|      |      |      |      |
|Mean  | 3.2% |13.0% |13.3% |
|StDev |25.5% |40.5% |39.3% |
    


![plot of chunk plot-9](/public/images/2014-11-14-Data-Proxy/plot-9-2.png) ![plot of chunk plot-9](/public/images/2014-11-14-Data-Proxy/plot-9-3.png) 

|      |RWX Price |RWX Total |VGSIX Price |VGSIX Total |VNQ Price |VNQ Total |
|:-----|:---------|:---------|:-----------|:-----------|:---------|:---------|
|Mean  | 3.2%     | 3.2%     |14.6%       |14.6%       |16.0%     |16.0%     |
|StDev |25.5%     |25.5%     |28.2%       |28.2%       |35.2%     |35.2%     |
    


![plot of chunk plot-9](/public/images/2014-11-14-Data-Proxy/plot-9-4.png) 
</div>

Please use `VNQ` and `VGSIX` to extend REIT ex-U.S.


> Aside comparison of RWO vs. RWX:
[RWO vs. RWX: Head-To-Head ETF Comparison](http://etfdb.com/tool/etf-comparison/RWO-RWX/)
    
    
<input type="button" class="btn btn-sm" value="+">Global REIT:
----
(Dow Jones Global Select Real Estate Securities Index)

<div markdown="1" style="display:none;">   

{% highlight r %}
	create.proxy('IYR,VGSIX,RWO', 'RWO, RWO.IYR=RWO+IYR, RWO.IYR.VGSIX=RWO+IYR+VGSIX')
{% endhighlight %}



|      |Start      |
|:-----|:----------|
|VGSIX |1996-06-28 |
|IYR   |2000-06-19 |
|RWO   |2008-05-22 |
    


![plot of chunk plot-10](/public/images/2014-11-14-Data-Proxy/plot-10-1.png) 

|      |IYR   |RWO   |VGSIX |
|:-----|:-----|:-----|:-----|
|IYR   |      |85%   |99%   |
|RWO   |      |      |85%   |
|      |      |      |      |
|Mean  |14.7% | 9.2% |16.8% |
|StDev |39.2% |30.0% |42.1% |
    


![plot of chunk plot-10](/public/images/2014-11-14-Data-Proxy/plot-10-2.png) ![plot of chunk plot-10](/public/images/2014-11-14-Data-Proxy/plot-10-3.png) 

|      |IYR Price |IYR Total |RWO Price |RWO Total |VGSIX Price |VGSIX Total |
|:-----|:---------|:---------|:---------|:---------|:-----------|:-----------|
|Mean  |14.7%     |14.7%     | 9.2%     | 9.2%     |14.6%       |14.6%       |
|StDev |29.6%     |29.6%     |30.0%     |30.0%     |28.2%       |28.2%       |
    


![plot of chunk plot-10](/public/images/2014-11-14-Data-Proxy/plot-10-4.png) 
</div>

Please use `IYR` and `VGSIX` to extend Global REIT.





<input type="button" class="btn btn-sm" value="+">CASH:
----

<div markdown="1" style="display:none;">   

{% highlight r %}
	#--------------------------------   
	# load 3-Month Treasury Bill from FRED (BIL)
	filename = 'data/TB3M.Rdata'
	if(!file.exists(filename)) {
		TB3M = quantmod::getSymbols('DTB3', src='FRED', auto.assign = FALSE)
		save(TB3M, file=filename)
	}
	load(file=filename)
	TB3M[] = ifna.prev(TB3M)
	#compute.raw.annual.factor(TB3M)
	raw.data$TB3M = make.stock.xts(processTBill(TB3M, timetomaturity = 1/4, 261))
	#--------------------------------   

	create.proxy('BIL,TB3M', 'BIL, BIL.TB3M=BIL+TB3M', raw.data)
{% endhighlight %}



|     |Start      |
|:----|:----------|
|TB3M |1954-01-04 |
|BIL  |2007-05-30 |
    


![plot of chunk plot-11](/public/images/2014-11-14-Data-Proxy/plot-11-1.png) 

|      |BIL  |TB3M |
|:-----|:----|:----|
|BIL   |     |26%  |
|      |     |     |
|Mean  |0.6% |0.7% |
|StDev |0.7% |0.3% |
    


![plot of chunk plot-11](/public/images/2014-11-14-Data-Proxy/plot-11-2.png) ![plot of chunk plot-11](/public/images/2014-11-14-Data-Proxy/plot-11-3.png) 

|      |BIL Price |BIL Total |TB3M Price |TB3M Total |
|:-----|:---------|:---------|:----------|:----------|
|Mean  |0.6%      |0.6%      |4.3%       |4.3%       |
|StDev |0.7%      |0.7%      |0.4%       |0.4%       |
    


![plot of chunk plot-11](/public/images/2014-11-14-Data-Proxy/plot-11-4.png) 

{% highlight r %}
	#--------------------------------   
	# load 3 years t-bill from FRED (BIL)
	filename = 'data/TB3Y.Rdata'
	if(!file.exists(filename)) {
		TB3Y = quantmod::getSymbols('DGS3', src='FRED', auto.assign = FALSE)
		save(TB3Y, file=filename)
	}
	load(file=filename)
	TB3Y[] = ifna.prev(TB3Y)
	#compute.raw.annual.factor(TB3Y)
	raw.data$TB3Y = make.stock.xts(processTBill(TB3Y, timetomaturity = 3, 261))
	#--------------------------------   
	
	create.proxy('SHY,TB3Y', 'SHY, SHY.TB3Y=SHY+TB3Y', raw.data)	
{% endhighlight %}



|     |Start      |
|:----|:----------|
|TB3Y |1962-01-02 |
|SHY  |2002-07-31 |
    


![plot of chunk plot-11](/public/images/2014-11-14-Data-Proxy/plot-11-5.png) 

|      |SHY  |TB3Y |
|:-----|:----|:----|
|SHY   |     |87%  |
|      |     |     |
|Mean  |2.2% |2.5% |
|StDev |1.5% |2.7% |
    


![plot of chunk plot-11](/public/images/2014-11-14-Data-Proxy/plot-11-6.png) ![plot of chunk plot-11](/public/images/2014-11-14-Data-Proxy/plot-11-7.png) 

|      |SHY Price |SHY Total |TB3Y Price |TB3Y Total |
|:-----|:---------|:---------|:----------|:----------|
|Mean  |2.2%      |2.2%      |5.7%       |5.7%       |
|StDev |1.5%      |1.5%      |3.3%       |3.3%       |
    


![plot of chunk plot-11](/public/images/2014-11-14-Data-Proxy/plot-11-8.png) 
</div>


<input type="button" class="btn btn-sm" value="+">GOLD:
----

<div markdown="1" style="display:none;">   

{% highlight r %}
	#--------------------------------
	filename = 'data/GOLD.Rdata'
	if(!file.exists(filename)) {
		GOLD = bundes.bank.data.gold()
		save(GOLD, file=filename)
	}
	load(file=filename)
	raw.data$GOLD = make.stock.xts(GOLD)
	#--------------------------------

	create.proxy('GLD,GOLD', 'GLD, GLD.GOLD=GLD+GOLD', raw.data)		
{% endhighlight %}



|     |Start      |
|:----|:----------|
|GOLD |1968-04-01 |
|GLD  |2004-11-18 |
    


![plot of chunk plot-12](/public/images/2014-11-14-Data-Proxy/plot-12-1.png) 

|      |GLD   |GOLD  |
|:-----|:-----|:-----|
|GLD   |      |61%   |
|      |      |      |
|Mean  |11.6% |12.2% |
|StDev |20.6% |20.4% |
    


![plot of chunk plot-12](/public/images/2014-11-14-Data-Proxy/plot-12-2.png) ![plot of chunk plot-12](/public/images/2014-11-14-Data-Proxy/plot-12-3.png) 

|      |GLD Price |GLD Total |GOLD Price |GOLD Total |
|:-----|:---------|:---------|:----------|:----------|
|Mean  |12.1%     |12.1%     | 9.4%      | 9.4%      |
|StDev |20.3%     |20.3%     |20.1%      |20.1%      |
    


![plot of chunk plot-12](/public/images/2014-11-14-Data-Proxy/plot-12-4.png) 
</div>






Let's save these proxies in **data.proxy.Rdata** for convience to use later on
----

Syntax to specify tickers in `getSymbols.extra` function:

* Basic : XLY
* Rename: BOND=TLT
* Extend: XLB+RYBIX
* Mix above: XLB=XLB+RYBIX+FSDPX+FSCHX+PRNEX+DREVX
* TLT;MTM + IEF => TLT = TLT + IEF and MTM = MTM + IEF
* BOND = TLT;MTM + IEF => BOND = TLT + IEF, TLT = TLT + IEF, and MTM = MTM + IEF
* BOND;US.BOND = TLT;MTM + IEF => BOND = TLT + IEF, US.BOND = TLT + IEF, TLT = TLT + IEF, and MTM = MTM + IEF
* BOND = [TLT] + IEF => BOND = TLT + IEF, and TLT = TLT + IEF

* use comma or new line to separate entries
* lines starting with `#` symbol or empty lines are skipped **Make sure not to use commas in comments**

,message=T, warning=T


{% highlight r %}
tickers = '
COM = DBC;GSG + CRB

RExUS = [RWX] + VNQ + VGSIX
RE = [RWX] + VNQ + VGSIX
RE.US = [ICF] + VGSIX

EMER.EQ = [EEM] + VEIEX
EMER.FI = [EMB] + PREMX

GOLD = [GLD] + GOLD,
US.CASH = [BIL] + TB3M,
SHY + TB3Y,

US.HY = [HYG] + VWEHX

# Bonds
US.BOND = [AGG] + VBMFX
INTL.BOND = [BWX] + BEGBX

JAPAN.EQ = [EWJ] + FJPNX
EUROPE.EQ = [IEV] + FIEUX
US.SMCAP = IWM;VB + NAESX
TECH.EQ = [QQQ] + ^NDX
US.EQ = [VTI] + VTSMX + VFINX
US.MID = [VO] + VIMSX
EAFE = [EFA] + VDMIX + VGTSX

MID.TR = [IEF] + VFITX
CORP.FI = [LQD] + VWESX
TIPS = [TIP] + VIPSX + LSGSX
LONG.TR = [TLT] + VUSTX
'


data.proxy <- new.env()
getSymbols.extra(tickers, src = 'yahoo', from = '1970-01-01', env = data.proxy, raw.data = raw.data, auto.assign = T)

data.proxy.raw = raw.data
save(data.proxy.raw, file='data/data.proxy.raw.Rdata',compress='gzip') 
save(data.proxy, file='data/data.proxy.Rdata',compress='gzip') 
{% endhighlight %}

To use saved proxy data to extend historical time series, use `extend.data.proxy` function:


{% highlight r %}
tickers = spl('BIL')

data <- new.env()
getSymbols(tickers, src = 'yahoo', from = '1970-01-01', env = data, auto.assign = T)

print(bt.start.dates(data))
{% endhighlight %}



|    |Start      |
|:---|:----------|
|BIL |2007-05-30 |
    




{% highlight r %}
extend.data.proxy(data, proxy.filename = 'data/data.proxy.Rdata')

print(bt.start.dates(data))
{% endhighlight %}



|    |Start      |
|:---|:----------|
|BIL |1954-01-04 |
    


*(this report was produced on: 2015-02-02)*
