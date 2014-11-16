---
layout: post
title: Data Proxy - extending time series with proxies
---

This page will hold collection of Proxies I collected to extend historical time series.

Note: there are more examples at
[Composing Synthetic Prices For Extended Historical ETF Data](http://indexswingtrader.blogspot.ca/2014/05/composing-synthetic-prices-for-extended.html)


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

<!-- open collapse div, http://stackoverflow.com/questions/15917463/embedding-markdown-in-jekyll-html -->

Commodities:
----
<a href="#self" class="acollapse">(hide)</a>

<div markdown="1">

{% highlight r %}
    #*****************************************************************
    # Load historical data
    #******************************************************************   
    load.packages('quantmod')  
    
    tickers = spl('GSG,DBC')
    data = new.env()
    getSymbols(tickers, src = 'yahoo', from = '1970-01-01', env = data, auto.assign = T)   
        for(i in ls(data)) data[[i]] = adjustOHLC(data[[i]], use.Adjusted=T)
      
    # "TRJ_CRB" file was downloaded from the http://www.corecommodityllc.com/CoreCommodity-Indexes/1cc/681
    # for "TRJ/CRB Index-Total Return"
    temp = extract.table.from.webpage( join(readLines("data/TR_CC-CRB")), 'EODValue' )
    temp = join( apply(temp, 1, join, ','), '\n' )
    data$CRB_1 = make.stock.xts( read.xts(temp, format='%m/%d/%y' ) )
     
    # "prfmdata.csv" file was downloaded from the http://www.crbequityindexes.com/indexdata-form.php
    # for "TR/J CRB Global Commodity Equity Index", "Total Return", "All Dates"
    data$CRB_2 = make.stock.xts( read.xts("data/prfmdata.csv", format='%m/%d/%Y' ) )
{% endhighlight %}

Look at historical start date for each series:
 

{% highlight r %}
print(bt.start.dates(data))
{% endhighlight %}



|      |Start      |
|:-----|:----------|
|CRB_1 |1994-01-03 |
|CRB_2 |1999-12-31 |
|DBC   |2006-02-06 |
|GSG   |2006-07-21 |

There are 2 sources of historical commodity index data. Let's compare them with commodity ETFs.
      

{% highlight r %}
    proxy.test(data)
{% endhighlight %}

![plot of chunk plot-5](/public/images/2014-11-14-Data-Proxy/plot-5-1.png) 

|      |CRB_1 |CRB_2 |DBC   |GSG   |
|:-----|:-----|:-----|:-----|:-----|
|CRB_1 |      |66%   |89%   |88%   |
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

![plot of chunk plot-6](/public/images/2014-11-14-Data-Proxy/plot-6-1.png) 

On the all history chart CRB_2 is also different.



{% highlight r %}
    proxy.prices(data)  
{% endhighlight %}

![plot of chunk plot-7](/public/images/2014-11-14-Data-Proxy/plot-7-1.png) 

|      |CRB_1 Price |CRB_1 Total |CRB_2 Price |CRB_2 Total |DBC Price |DBC Total |GSG Price |GSG Total |
|:-----|:-----------|:-----------|:-----------|:-----------|:---------|:---------|:---------|:---------|
|Mean  | 8.2%       | 8.2%       |11.5%       |11.5%       | 1.7%     | 1.7%     |-4.2%     |-4.2%     |
|StDev |16.3%       |16.3%       |21.5%       |21.5%       |20.9%     |20.9%     |24.9%     |24.9%     |

Quick glance at historical time series does not show anything abnormal between Price and Adjusted Price series. 
    

{% highlight r %}
    tickers = spl('DBC, DBC.CRB=DBC+CRB_1')
    proxy.map(data, tickers)
{% endhighlight %}

![plot of chunk plot-8](/public/images/2014-11-14-Data-Proxy/plot-8-1.png) 
</div>
                    
REITs: 
---- 
(Dow Jones Global **ex-U.S.** Real Estate Securities Index)
<a href="#self" class="acollapse">(hide)</a>

<div markdown="1">
    

{% highlight r %}
    #*****************************************************************
    # Load historical data
    #******************************************************************   
    load.packages('quantmod')  
    
    tickers = spl('RWX,VNQ,VGSIX')
    data = new.env()
    getSymbols(tickers, src = 'yahoo', from = '1970-01-01', env = data, auto.assign = T)   
        for(i in ls(data)) data[[i]] = adjustOHLC(data[[i]], use.Adjusted=T)
                         
    #*****************************************************************
    # Compare
    #******************************************************************
    print(bt.start.dates(data))
{% endhighlight %}



|      |Start      |
|:-----|:----------|
|VGSIX |1996-06-28 |
|VNQ   |2004-10-01 |
|RWX   |2007-03-02 |


{% highlight r %}
    proxy.test(data)    
{% endhighlight %}

![plot of chunk plot-9](/public/images/2014-11-14-Data-Proxy/plot-9-1.png) 

|      |RWX   |VGSIX |VNQ   |
|:-----|:-----|:-----|:-----|
|RWX   |      |66%   |67%   |
|VGSIX |      |      |99%   |
|      |      |      |      |
|Mean  | 3.1% |12.1% |12.2% |
|StDev |25.8% |40.9% |39.8% |




{% highlight r %}
    proxy.overlay.plot(data)   
{% endhighlight %}

![plot of chunk plot-9](/public/images/2014-11-14-Data-Proxy/plot-9-2.png) 

{% highlight r %}
    proxy.prices(data)
{% endhighlight %}

![plot of chunk plot-9](/public/images/2014-11-14-Data-Proxy/plot-9-3.png) 

|      |RWX Price |RWX Total |VGSIX Price |VGSIX Total |VNQ Price |VNQ Total |
|:-----|:---------|:---------|:-----------|:-----------|:---------|:---------|
|Mean  | 3.1%     | 3.1%     |14.3%       |14.3%       |15.2%     |15.2%     |
|StDev |25.8%     |25.8%     |28.3%       |28.3%       |35.5%     |35.5%     |




{% highlight r %}
    tickers = spl('RWX, RWX.VNQ=RWX+VNQ, RWX.VNQ.VGSIX=RWX+VNQ+VGSIX')
    proxy.map(data, tickers)
{% endhighlight %}

![plot of chunk plot-9](/public/images/2014-11-14-Data-Proxy/plot-9-4.png) 
</div>
[RWO vs. RWX: Head-To-Head ETF Comparison](http://etfdb.com/tool/etf-comparison/RWO-RWX/)
    
    
REITs:
----
(Dow Jones Global Select Real Estate Securities Index)
<a href="#self" class="acollapse">(show)</a>

<div markdown="1" style="display:none;">   

{% highlight r %}
    #*****************************************************************
    # Load historical data
    #******************************************************************   
    load.packages('quantmod')  
    
    tickers = spl('IYR,VGSIX,RWO')
    data = new.env()
    getSymbols(tickers, src = 'yahoo', from = '1970-01-01', env = data, auto.assign = T)   
        for(i in ls(data)) data[[i]] = adjustOHLC(data[[i]], use.Adjusted=T)
           
    #*****************************************************************
    # Compare
    #******************************************************************
    print(bt.start.dates(data))
{% endhighlight %}



|      |Start      |
|:-----|:----------|
|VGSIX |1996-06-28 |
|IYR   |2000-06-19 |
|RWO   |2008-05-22 |


{% highlight r %}
    proxy.test(data)    
{% endhighlight %}

![plot of chunk plot-10](/public/images/2014-11-14-Data-Proxy/plot-10-1.png) 

|      |IYR   |RWO   |VGSIX |
|:-----|:-----|:-----|:-----|
|IYR   |      |85%   |99%   |
|RWO   |      |      |85%   |
|      |      |      |      |
|Mean  |13.8% | 8.3% |15.8% |
|StDev |39.8% |30.4% |42.7% |




{% highlight r %}
    proxy.overlay.plot(data)   
{% endhighlight %}

![plot of chunk plot-10](/public/images/2014-11-14-Data-Proxy/plot-10-2.png) 

{% highlight r %}
    proxy.prices(data)
{% endhighlight %}

![plot of chunk plot-10](/public/images/2014-11-14-Data-Proxy/plot-10-3.png) 

|      |IYR Price |IYR Total |RWO Price |RWO Total |VGSIX Price |VGSIX Total |
|:-----|:---------|:---------|:---------|:---------|:-----------|:-----------|
|Mean  |14.0%     |14.0%     | 8.3%     | 8.3%     |14.3%       |14.3%       |
|StDev |29.8%     |29.8%     |30.4%     |30.4%     |28.3%       |28.3%       |




{% highlight r %}
    tickers = spl('RWO, RWO.IYR=RWO+IYR, RWO.IYR.VGSIX=RWO+IYR+VGSIX')
    proxy.map(data, tickers)
{% endhighlight %}

![plot of chunk plot-10](/public/images/2014-11-14-Data-Proxy/plot-10-4.png) 
</div>

