---
layout: post
title: Extending Leveraged Time Series
comments: true
---

The ['Hell on Fire' - Part I: The 3x leveraged Universal Investment Strategy](http://www.logical-invest.com/hell-on-fire-part-i-the-3x-leveraged-universal-investment-strategy/)
post mentioned an interesting idea to extend Leveraged Time Series. The idea is to first
examine both **leveraged** and **not leveraged** time series performance over common period,
and run regression to infer optimal leverage ratio. The alternative is to take the leverage ratio
specified in product offering. This approach does not always work, if proxy is different from
the one specified in product offering.

Below I will try to show case this idea:



Next, let's test with [Direxion Daily 20+ Yr Trsy Bull 3X ETF (TMF)](http://www.direxioninvestments.com/products/direxion-daily-20-year-treasury-bull-3x-etf)

Below I will extend it with [iShares 20+ Year Treasury Bond (TLT)](http://www.ishares.com/us/products/239454/ishares-20-year-treasury-bond-etf)
which is not a perfect proxy, but will do for our testing purposes.

```{r}
#*****************************************************************
# Load historical data
#*****************************************************************
tickers = 'TLT,TMF'

data = new.env()
getSymbols.extra(tickers, src = 'yahoo', from = '1980-01-01', env = data, auto.assign = T)

data$TMF = extend.data(data$TMF, create.leveraged(data$TLT, leverage=3), scale=T)    


###############################################################################
# Leveraged series
###############################################################################  
# Create Leveraged series with data from the unlevereged.
# @example create.leveraged(tlt, 2)
# @example extend.data(data$UBT, create.leveraged(data$TLT, leverage=2), scale=T)
# @example extend.data(data$TMF, create.leveraged(data$TLT, leverage=3), scale=T)





```{r}
source('data.r')
source('strategy.r')


#*****************************************************************
# Load historical data
#*****************************************************************
library(SIT)
load.packages('quantmod')

tickers = 'TLT'

data = new.env()
getSymbols.extra(tickers, src = 'yahoo', from = '1980-01-01', env = data, auto.assign = T)

for(i in ls(data))
	data[[i]] = adjustOHLC(data[[i]], use.Adjusted=T)

bt.prep(data, align='remove.na')

#*****************************************************************
# Setup
#*****************************************************************
prices = data$prices
			
models = list()

commission = list(cps = 0.01, fixed = 10.0, percentage = 0.0)
		
universe = prices > 0
	
key.date.index = date.month.ends(data$dates, F)
key.date = NA * prices
	key.date[key.date.index,] = T	
	
#*****************************************************************
# Benchmark
#*****************************************************************
data$weight[] = NA
	data$weight$TLT = 1
models$TLT = bt.run.share(data, clean.signal=T, commission = commission, trade.summary=T, silent=T)

#*****************************************************************
# Last 5 trading days of the month
#*****************************************************************
signals = list(L5=-4:0)
	signals = calendar.signal(key.date, signals)
	signals$exL5 = ifna(!signals$L5,T)
models = c(models, calendar.strategy(data, signals, universe = universe, commission = commission))
	

#*****************************************************************
# Create Report
#*****************************************************************
#strategy.performance.snapshoot(models, T)
plotbt(models, plotX = T, log = 'y', LeftMargin = 3, main = NULL)
	mtext('Cumulative Performance', side = 2, line = 1)

print(plotbt.strategy.sidebyside(models, make.plot=F, return.table=T, perfromance.fn=engineering.returns.kpi))

print(last.trades(models$L5, make.plot=F, return.table=T))

print(plotbt.monthly.table(models$L5$equity, make.plot = F))
```


	ret1, ret2
lm(ret1~ret2+0)

load.packages('quantreg')
tau = c(.2,.5,.8)
	result[i,1] = rq(rets[,i] ~ rets[,1], tau = tau[1])$coef[2]



# Ask the slopes for 20% and 80%
tau = c(.2,.8)

result = matrix(1, nr=n, nc=2)
for(i in 2:n) {
	result[i,1] = rq(rets[,i] ~ rets[,1], tau = tau[1])$coef[2]
	result[i,2] = rq(rets[,i] ~ rets[,1], tau = tau[2])$coef[2]
	if( i %% 100 == 0) cat(i, '\n')
}

rlm in the MASS package

orlm(MASS)           Fit Robust Linear Regression Model 
rlm(MASS)            Robust Fitting of Linear Models 

Robust regression is done by iterated re-weighted least squares (IRLS). 
The command for running robust regression is rlm in the MASS package. 

http://cran.r-project.org/web/packages/robust/robust.pdf
ack.rob <- lmRob(Loss ~ ., data = stack.dat)

http://www.alastairsanderson.com/R/tutorials/robust-regression-in-R/


http://www.alastairsanderson.com/projects/Dow-Jones-Industrial-Average-stock-clustering-analysis/