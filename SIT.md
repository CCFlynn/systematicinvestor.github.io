---
layout: page
title: SIT
permalink: sit/
---
     
[Systematic Investor Toolbox (SIT)](https://github.com/systematicinvestor/SIT)
Systematic Investor Toolbox is a collection of tools that I use in my investment research.

{% highlight r %}
###############################################################################
# Load Systematic Investor Toolbox (SIT)
# http://systematicinvestor.wordpress.com/systematic-investor-toolbox/
###############################################################################
con = gzcon(url('http://www.systematicportfolio.com/sit.gz', 'rb'))
    source(con)
close(con)
 
 
###############################################################################
# Load Systematic Investor Toolbox (SIT): Windows only
###############################################################################
# Load Systematic Investor Toolbox (SIT)
setInternet2(TRUE)
con = gzcon(url('https://github.com/systematicinvestor/SIT/raw/master/sit.gz', 'rb'))
    source(con)
close(con)
 
 
###############################################################################
# Load Systematic Investor Toolbox (SIT): from file, if for example you saved sit.gz to c:/temp/sit.gz
###############################################################################
con = gzcon(file('c:/temp/sit.gz', 'rb'))
    source(con)
close(con)
 
 
###############################################################################
# Load Systematic Investor Toolbox (SIT): Requires RCurl package
###############################################################################
require(RCurl)
sit = getURLContent('https://github.com/systematicinvestor/SIT/raw/master/sit.gz', binary=TRUE, followlocation = TRUE, ssl.verifypeer = FALSE)
    con = gzcon(rawConnection(sit, 'rb'))
    source(con)
close(con)
 
 
###############################################################################
# Example Usage:
###############################################################################
# Run plota test
plota.test()
{% endhighlight %}

