---
layout: post
title: Sortino vs Omega ratios
comments: true
---


To install [Systematic Investor Toolbox (SIT)](https://github.com/systematicinvestor/SIT) please visit [About](/about) page.




Following is quick visual comparison of 
[Omega ratio](http://en.wikipedia.org/wiki/Omega_ratio) vs
[Sortino ratio](http://en.wikipedia.org/wiki/Sortino_ratio)


{% highlight r %}
library(SIT)
load.packages('quantmod')

#*****************************************************************
# Helper functions
#*****************************************************************
sortino.ratio = function(R,MAR) {
	r = R[R < MAR]
	mean(R - MAR) / sqrt(sum((MAR - r)^2) / length(r) ) 
}

omega.ratio = function(R,MAR) {
	mean(pmax(R - MAR, 0)) / mean(pmax(MAR - R, 0))
}

#*****************************************************************
# check: Omega has a value of 1 at the mean of the distribution
#*****************************************************************
R = rnorm(100)
MAR = mean(R)

print(list(Sortino = sortino.ratio(R, MAR), Omega = omega.ratio(R,MAR)))
{% endhighlight %}



<pre>
$Sortino
[1] -1.827587e-17

$Omega
[1] 1

</pre>
    




{% highlight r %}
#*****************************************************************
# Plot Sortino vs Omega
# based on [R graph with two y-axes](http://robjhyndman.com/hyndsight/r-graph-with-two-y-axes/)
#*****************************************************************
plot.sortino.omega = function(R, main='') {
  x = quantile(R, c(0.05, 0.95))
  x = seq(x[1],x[2], length.out=100)

  par(mar=c(5,4,4,5))
  plot(x, sapply(x, function(y) sortino.ratio(R,y)), xlab='MAR', ylab='Sortino', 
    main=paste('Sortino vs Omega', main), type='l', col='red')

  par(new=TRUE)
  plot(x, sapply(x, function(y) omega.ratio(R,y)), xaxt="n",yaxt="n",xlab="", ylab='', type='l', col='blue')

  axis(4)
  mtext('Omega',side=4,line=3)
  legend("topright",col=c('red','blue'),lty=1,legend=c('Sortino','Omega'), bty='n')
}

plot.sortino.omega(R, ' based on random data')
{% endhighlight %}

![plot of chunk plot-2](/public/images/2015-04-02-Sortino-Omega/plot-2-1.png) 

{% highlight r %}
#*****************************************************************
# Load historical data
#*****************************************************************
data = env()
getSymbols('SPY', src = 'yahoo', from = '1970-01-01', env = data, auto.assign = T)

price = Ad(data$SPY)
ret = diff(log(price))

plot.sortino.omega(last(ret, 252), ' based on last year of SPY')
{% endhighlight %}

![plot of chunk plot-2](/public/images/2015-04-02-Sortino-Omega/plot-2-2.png) 



*(this report was produced on: 2015-04-03)*
