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
	(mean(R) - MAR) / sqrt(sum((MAR - r)^2) / length(r) ) 
}

omega.ratio = function(R,MAR) {
	#sum(pmax(R - MAR, 0)) / sum(pmax(MAR - R, 0))
	r = R - MAR
	- sum(r[r > 0]) / sum(r[r < 0])
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
[1] 0

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



Re-run [Maximizing Omega Ratio](https://systematicinvestor.wordpress.com/2011/11/03/maximizing-omega-ratio/)

Please set threshold used in Omega calculations (i.e. MAR) to a small number.
Otherwise, optimizer will be forced to find corner solutions and result in not stable weights.


{% highlight r %}
	#--------------------------------------------------------------------------
	# Create Efficient Frontier
	#--------------------------------------------------------------------------
	ia = aa.test.create.ia()
	n = ia$n		

	# 0 <= x.i <= 0.8 
	constraints = new.constraints(n, lb = 0, ub = 0.8)
	
	# SUM x.i = 1
	constraints = add.constraints(rep(1, n), 1, type = '=', constraints)		
	
	# Omega - http://en.wikipedia.org/wiki/Omega_ratio
		# do not set threshold too high
		ia$parameters.omega = 1/100 
		# convert annual to monthly
		ia$parameters.omega = ia$parameters.omega / 12


	# create efficient frontier(s)
	ef.risk = portopt(ia, constraints, 50, 'Risk')

	#plot.ef(ia, list(ef.risk), portfolio.risk, T, T)			
	
	
	# Plot Omega Efficient Frontiers and Transition Maps
	layout( matrix(1:4, nrow = 2, byrow=T) )
	
	# weights
	rownames(ef.risk$weight) = paste('Risk','weight',1:50,sep='_')
	plot.omega(ef.risk$weight[c(1,10,40,50), ], ia)
	
	# assets
	temp = diag(n)
	rownames(temp) = ia$symbols
	plot.omega(temp, ia)
		
	# portfolio
	plot.ef(ia, list(ef.risk), portfolio.omega, T, T)			
{% endhighlight %}

![plot of chunk plot-4](/public/images/2015-04-02-Sortino-Omega/plot-4-1.png) 

{% highlight r %}
	#--------------------------------------------------------------------------
	# Create Efficient Frontier in Omega Ratio framework
	#--------------------------------------------------------------------------
	# Create maximum Omega Efficient Frontier
	ef.omega = portopt.omega(ia, constraints, 50)
{% endhighlight %}

i = 1 
i = 2 
i = 3 
i = 4 
i = 5 
i = 6 
i = 7 
i = 8 
i = 9 
i = 10 
i = 11 
i = 12 
i = 13 
i = 14 
i = 15 
i = 16 
i = 17 
i = 18 
i = 19 
i = 20 
i = 21 
i = 22 
i = 23 
i = 24 
i = 25 
i = 26 
i = 27 
i = 28 
i = 29 
i = 30 
i = 31 
i = 32 
i = 33 
i = 34 
i = 35 
i = 36 
i = 37 
i = 38 
i = 39 
i = 40 
i = 41 
i = 42 
i = 43 
i = 44 
i = 45 
i = 46 
i = 47 
i = 48 
i = 49 
i = 50 


{% highlight r %}
	# Plot Omega Efficient Frontiers and Transition Maps
	layout( matrix(1:4, nrow = 2, byrow=T) )

	# weights
	plot.omega(ef.risk$weight[c(1,10,40,50), ], ia)	
	#plot.omega(ef.omega$weight[c(1,10,40,50), ], ia)
	

	# weights
	rownames(ef.omega$weight) = paste('Omega','weight',1:50,sep='_')	
	plot.omega(ef.omega$weight[c(1,10,40,50), ], ia)
		
	# portfolio
	plot.ef(ia, list(ef.omega, ef.risk), portfolio.omega, T, T)			
{% endhighlight %}

![plot of chunk plot-4](/public/images/2015-04-02-Sortino-Omega/plot-4-2.png) 

{% highlight r %}
	#--------------------------------------------------------------------------
	# Compare Risk and Omega frontiers
	#--------------------------------------------------------------------------
	# Plot multiple Efficient Frontiers and Transition Maps
	layout( matrix(1:4, nrow = 2) )
	plot.ef(ia, list(ef.risk,ef.omega), portfolio.risk, F)			
	plot.ef(ia, list(ef.risk,ef.omega), portfolio.omega, F)			

	plot.transition.map(ef.risk)
	plot.transition.map(ef.omega)
{% endhighlight %}

![plot of chunk plot-4](/public/images/2015-04-02-Sortino-Omega/plot-4-3.png) 
		
I will use methods presented in 
[Optimizing Omega by H. Mausser, D. Saunders, L. Seco (2006)](http://www.researchgate.net/publication/230786885_Optimizing_Omega)
 paper to construct optimal portfolios that maximize Omega Ratio.		



{% highlight r %}
#' @export 	
target.omega.portfolio <- function
(
	target.omega
)
{
	target.omega = target.omega

	function
	(
		ia,			# input assumptions
		constraints	# constraints
	)
	{
		ia$parameters.omega = target.omega
		max.omega.portfolio(ia, constraints)
	}	
}

 
 
	#*****************************************************************
	# Load historical data
	#****************************************************************** 
	load.packages('quantmod,quadprog,corpcor,lpSolve')
	tickers = spl('SPY,QQQ,EEM,IWM,EFA,TLT,IYR,GLD')

	data = env()
	getSymbols(tickers, src = 'yahoo', from = '1980-01-01', env = data, auto.assign = T)
		for(i in ls(data)) data[[i]] = adjustOHLC(data[[i]], use.Adjusted=T)							
	bt.prep(data, align='remove.na', dates='1990::') 
	
	#*****************************************************************
	# Code Strategies
	#****************************************************************** 					
	obj = portfolio.allocation.helper(data$prices, 
		periodicity = 'months', lookback.len = 60, 
		min.risk.fns = list(
			EW=equal.weight.portfolio,
			RP=risk.parity.portfolio(),
			MD=max.div.portfolio,						
			
			MV=min.var.portfolio,
			MVE=min.var.excel.portfolio,
			MO=target.omega.portfolio(0/100),
			
			MC=min.corr.portfolio,
			MCE=min.corr.excel.portfolio,
			MC2=min.corr2.portfolio,
			
			MS=max.sharpe.portfolio()
		),
		silent=T
	)
	
	models = create.strategies(obj, data, silent=T)$models
	
#*****************************************************************
# Create Report
#*****************************************************************
#strategy.performance.snapshoot(models, T)
plotbt(models, plotX = T, log = 'y', LeftMargin = 3, main = NULL)
	mtext('Cumulative Performance', side = 2, line = 1)
{% endhighlight %}

![plot of chunk plot-5](/public/images/2015-04-02-Sortino-Omega/plot-5-1.png) 

{% highlight r %}
print(plotbt.strategy.sidebyside(models, make.plot=F, return.table=T, perfromance.fn=engineering.returns.kpi))
{% endhighlight %}



|           |EW                |RP                |MD                |MV                |MVE               |MO                |MC                |MCE               |MC2               |MS                |
|:----------|:-----------------|:-----------------|:-----------------|:-----------------|:-----------------|:-----------------|:-----------------|:-----------------|:-----------------|:-----------------|
|Period     |Nov2004 - Apr2015 |Nov2004 - Apr2015 |Nov2004 - Apr2015 |Nov2004 - Apr2015 |Nov2004 - Apr2015 |Nov2004 - Apr2015 |Nov2004 - Apr2015 |Nov2004 - Apr2015 |Nov2004 - Apr2015 |Nov2004 - Apr2015 |
|Cagr       |8.95              |9.95              |10.7              |10.68             |11.59             |13.3              |9.97              |9.97              |9.83              |13.04             |
|Sharpe     |0.58              |0.75              |1.24              |1.31              |1.29              |0.98              |1.1               |1.1               |1.06              |1                 |
|DVR        |0.48              |0.67              |1.17              |1.22              |1.22              |0.89              |1.04              |1.04              |1.01              |0.92              |
|R2         |0.84              |0.9               |0.95              |0.94              |0.95              |0.92              |0.95              |0.95              |0.95              |0.92              |
|Volatility |17.6              |14.09             |8.52              |8.02              |8.85              |13.82             |9.05              |9.05              |9.24              |13.09             |
|MaxDD      |-45.51            |-34.53            |-13.44            |-13.24            |-14.3             |-22.5             |-17.23            |-17.17            |-17.65            |-22.03            |
|Exposure   |97.36             |97.36             |97.36             |97.36             |97.36             |97.36             |97.36             |97.36             |97.36             |97.36             |
    








*(this report was produced on: 2015-04-07)*
