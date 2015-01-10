---
layout: post
title: Regime Detection Update
comments: true
---


To install [Systematic Investor Toolbox (SIT)](https://github.com/systematicinvestor/SIT) please visit [About](/about) page.





I did series of posts about Regime Detection using [RHmm](http://cran.r-project.org/web/packages/RHmm/index.html)
sometime ago. Unfortunately, the [RHmm](http://cran.r-project.org/web/packages/RHmm/index.html) is no 
longer available from [CRAN](http://cran.r-project.org/web/packages/RHmm/index.html), so I want to
update the repository location for [RHmm](http://r-forge.r-project.org/R/?group_id=85) package, and
also replicate functionality with [depmixS4](http://cran.r-project.org/web/packages/depmixS4/index.html)
package. The [depmixS4](http://cran.r-project.org/web/packages/depmixS4/index.html) package
also allows linear constraints on parameters.

Summary:

* RHmm is available at [R-Forge](http://r-forge.r-project.org/R/?group_id=85)
* For more info about [depmixS4](http://cran.r-project.org/web/packages/depmixS4/index.html) package,
please have a look at [Getting Started with Hidden Markov Models in R](http://blog.revolutionanalytics.com/2014/03/r-and-hidden-markov-models.html)  

Please see below updated code for the [bt.regime.detection.test() function in bt.test.r at github](https://github.com/systematicinvestor/SIT/blob/master/R/bt.test.r):


{% highlight r %}
library(SIT)
load.packages('quantmod')

###############################################################################
# Regime Detection
# http://blogs.mathworks.com/pick/2011/02/25/markov-regime-switching-models-in-matlab/
###############################################################################
#bt.regime.detection.test <- function() 

	#*****************************************************************
	# Generate data as in the post
	#****************************************************************** 
	bull1 = rnorm( 100, 0.10, 0.15 )
	bear  = rnorm( 100, -0.01, 0.20 )
	bull2 = rnorm( 100, 0.10, 0.15 )
	true.states = c(rep(1,100),rep(2,100),rep(1,100))
	returns = c( bull1, bear,  bull2 )

	# find regimes
  load.packages('RHmm', repos ='http://R-Forge.R-project.org')

	y=returns
	ResFit = HMMFit(y, nStates=2)
	VitPath = viterbi(ResFit, y)
{% endhighlight %}

DimObs=1


{% highlight r %}
	# HMMGraphicDiag(VitPath, ResFit, y)
	# HMMPlotSerie(y, VitPath)

	#Forward-backward procedure, compute probabilities
	fb = forwardBackward(ResFit, y)

	# Plot probabilities and implied states
	layout(1:2)
	plot(VitPath$states, type='s', main='Implied States', xlab='', ylab='State')
	
	matplot(fb$Gamma, type='l', main='Smoothed Probabilities', ylab='Probability')
		legend(x='topright', c('State1','State2'),  fill=1:2, bty='n')
{% endhighlight %}

![plot of chunk plot-2](/public/images/2015-01-04-Regime-Detection-Update/plot-2-1.png) 

{% highlight r %}
	# http://lipas.uwasa.fi/~bepa/Markov.pdf
	# Expected duration of each regime (1/(1-pii))                
	#1/(1-diag(ResFit$HMM$transMat))
          

	#*****************************************************************
	# It also can be done using depmixS4 package
  #[Getting Started with Hidden Markov Models in R](http://blog.revolutionanalytics.com/2014/03/r-and-hidden-markov-models.html)
	#****************************************************************** 
  load.packages('depmixS4')

  # Construct and fit a regime switching model
  mod = depmix(y ~ 1, family = gaussian(), nstates = 2, data=data.frame(y=y))
  fm2 = fit(mod, verbose = FALSE)
{% endhighlight %}

converged at iteration 48 with logLik: 124.6015 


{% highlight r %}
  probs = posterior(fm2)

	layout(1:2)
	plot(probs$state, type='s', main='Implied States', xlab='', ylab='State')
	
	matplot(probs[,-1], type='l', main='Probabilities', ylab='Probability')
		legend(x='topright', c('State1','State2'),  fill=1:2, bty='n')
{% endhighlight %}

![plot of chunk plot-2](/public/images/2015-01-04-Regime-Detection-Update/plot-2-2.png) 

{% highlight r %}
	#*****************************************************************
	# Add some data and see if the model is able to identify the regimes
	#****************************************************************** 
	bear2  = rnorm( 100, -0.01, 0.20 )
	bull3 = rnorm( 100, 0.10, 0.10 )
	bear3  = rnorm( 100, -0.01, 0.25 )
  true.states = c(true.states, rep(2,100),rep(1,100),rep(2,100))
	y = c( bull1, bear,  bull2, bear2, bull3, bear3 )
	VitPath = RHmm:::viterbi(ResFit, y)$states
{% endhighlight %}

DimObs=1


{% highlight r %}
	# map states: sometimes HMMFit function does not assign states consistently
	# let's use following formula to rank states
	# i.e. high risk, low returns => state 2 and low risk, high returns => state 1
	map = rank(sqrt(ResFit$HMM$distribution$var) - ResFit$HMM$distribution$mean)
	VitPath = map[VitPath]

	#*****************************************************************
	# Plot regimes
	#****************************************************************** 
	data = xts(y, as.Date(1:len(y)))

	layout(1:3)
		plota.control$col.x.highlight = col.add.alpha(true.states+1, 150)
	plota(data, type='h', plotX=F, x.highlight=T)
		plota.legend('Returns + True Regimes')
	plota(cumprod(1+data/100), type='l', plotX=F, x.highlight=T)
		plota.legend('Equity + True Regimes')
	
		plota.control$col.x.highlight = col.add.alpha(VitPath+1, 150)
	plota(data, type='h', x.highlight=T)
		plota.legend('Returns + Detected Regimes')
{% endhighlight %}

![plot of chunk plot-2](/public/images/2015-01-04-Regime-Detection-Update/plot-2-3.png) 


*(this report was produced on: 2015-01-04)*
