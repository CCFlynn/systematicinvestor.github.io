---
layout: post
title: Back-test Reality Check
comments: true
---


To install [Systematic Investor Toolbox (SIT)](https://github.com/systematicinvestor/SIT) please visit [About](/about) page.




The purpose of a back-test is to show a realistic historical picture of strategy performance. 
One might use back-test results and corresponding statistics to judge whether a strategy is suitable one.
Hence, it is best to structure a back-test to be as realistic as possible in order to avoid unpleasant 
surprises and have solid foundation for selecting a suitable strategy.

First strategy outline: the strategy is the strategic equal weight allocation
across following 5 stocks: MMM, AA, CAT, KO, HPQ. I selected these stocks from Dow Jones Industrial Average.
The allocation is updated monthly and back-test starts on Jan 1st, 1970 with $100,000 initial capital.

Let's start with the most simple back-test setup and incrementally add features to make it more realistic.

The most simple setup is to multiply weights vector by daily returns, based on adjusted prices,
to compute daily returns for the strategy. Please see below the equity line for the strategy (r.ew)


#Dividend and Split Adjusted Asset Performance
    


![plot of chunk plot-2](/public/images/2015-10-19-Backtest-Reality-Check/plot-2-1.png) ![plot of chunk plot-2](/public/images/2015-10-19-Backtest-Reality-Check/plot-2-2.png) 

|              |r.ew              |
|:-------------|:-----------------|
|Period        |Jan1970 - Oct2015 |
|Cagr          |12                |
|Sharpe        |0.65              |
|DVR           |0.52              |
|R2            |0.8               |
|Volatility    |20.94             |
|MaxDD         |-60.67            |
|Exposure      |99.82             |
|Win.Percent   |100               |
|Avg.Trade     |1645.47           |
|Profit.Factor |NaN               |
|Num.Trades    |5                 |
    

There is a problem with above approach, it assumes that weights stay constant through out the month, or
alternatively that we re-balance strategy daily to the target allocation. However, in reality, we invest
at the end of the month and update allocations at the end of the next month. The proper solution is to
compute share allocation at the end of the month and update shares at the end of the next month (s.ew)

![plot of chunk plot-3](/public/images/2015-10-19-Backtest-Reality-Check/plot-3-1.png) 

|              |r.ew              |s.ew              |
|:-------------|:-----------------|:-----------------|
|Period        |Jan1970 - Oct2015 |Jan1970 - Oct2015 |
|Cagr          |12                |11.53             |
|Sharpe        |0.65              |0.63              |
|DVR           |0.52              |0.51              |
|R2            |0.8               |0.81              |
|Volatility    |20.94             |20.86             |
|MaxDD         |-60.67            |-60.88            |
|Exposure      |99.82             |99.82             |
|Win.Percent   |100               |55.45             |
|Avg.Trade     |1645.47           |0.21              |
|Profit.Factor |NaN               |1.4               |
|Num.Trades    |5                 |2750              |
    

One of the missing features of above approach is commissions. In reality, every time we make a transaction,
brokerage charge commissions. Let's add following commission structure: $10 fixed per transaction, plus
1c per share (s.ew.com)


![plot of chunk plot-4](/public/images/2015-10-19-Backtest-Reality-Check/plot-4-1.png) 

|              |r.ew              |s.ew              |s.ew.com          |
|:-------------|:-----------------|:-----------------|:-----------------|
|Period        |Jan1970 - Oct2015 |Jan1970 - Oct2015 |Jan1970 - Oct2015 |
|Cagr          |12                |11.53             |11.03             |
|Sharpe        |0.65              |0.63              |0.61              |
|DVR           |0.52              |0.51              |0.49              |
|R2            |0.8               |0.81              |0.81              |
|Volatility    |20.94             |20.86             |20.85             |
|MaxDD         |-60.67            |-60.88            |-60.9             |
|Exposure      |99.82             |99.82             |99.82             |
|Win.Percent   |100               |55.45             |55.45             |
|Avg.Trade     |1645.47           |0.21              |0.21              |
|Profit.Factor |NaN               |1.4               |1.4               |
|Num.Trades    |5                 |2750              |2750              |
    

Another missing feature of above approach is round lot share allocation. In reality, we don't acquire fractional
shares, most of the time we buy shares in round lots. Let's add 100 shares round lot requirement (s.ew.com.lot) 

![plot of chunk plot-5](/public/images/2015-10-19-Backtest-Reality-Check/plot-5-1.png) 

|              |r.ew              |s.ew              |s.ew.com          |s.ew.com.lot      |
|:-------------|:-----------------|:-----------------|:-----------------|:-----------------|
|Period        |Jan1970 - Oct2015 |Jan1970 - Oct2015 |Jan1970 - Oct2015 |Jan1970 - Oct2015 |
|Cagr          |12                |11.53             |11.03             |11.03             |
|Sharpe        |0.65              |0.63              |0.61              |0.61              |
|DVR           |0.52              |0.51              |0.49              |0.49              |
|R2            |0.8               |0.81              |0.81              |0.81              |
|Volatility    |20.94             |20.86             |20.85             |20.85             |
|MaxDD         |-60.67            |-60.88            |-60.9             |-60.9             |
|Exposure      |99.82             |99.82             |99.82             |99.82             |
|Win.Percent   |100               |55.45             |55.45             |55.17             |
|Avg.Trade     |1645.47           |0.21              |0.21              |0.22              |
|Profit.Factor |NaN               |1.4               |1.4               |1.41              |
|Num.Trades    |5                 |2750              |2750              |2668              |
    
	
Another missing feature of above approach is turnover control. In reality, we don't blindly
re-balance to new allocation, but instead evaluate the cost of re-balance and tracking error, and
only re-balance when needed.  Let's re-balance only if total absolute discrepancy between
current allocation and target allocation is greater than 5% (s.ew.com.lot.turnover) 


![plot of chunk plot-6](/public/images/2015-10-19-Backtest-Reality-Check/plot-6-1.png) 

|              |r.ew              |s.ew              |s.ew.com          |s.ew.com.lot      |s.ew.com.lot.turnover |
|:-------------|:-----------------|:-----------------|:-----------------|:-----------------|:---------------------|
|Period        |Jan1970 - Oct2015 |Jan1970 - Oct2015 |Jan1970 - Oct2015 |Jan1970 - Oct2015 |Jan1970 - Oct2015     |
|Cagr          |12                |11.53             |11.03             |11.03             |11.12                 |
|Sharpe        |0.65              |0.63              |0.61              |0.61              |0.61                  |
|DVR           |0.52              |0.51              |0.49              |0.49              |0.49                  |
|R2            |0.8               |0.81              |0.81              |0.81              |0.81                  |
|Volatility    |20.94             |20.86             |20.85             |20.85             |20.82                 |
|MaxDD         |-60.67            |-60.88            |-60.9             |-60.9             |-60.79                |
|Exposure      |99.82             |99.82             |99.82             |99.82             |99.82                 |
|Win.Percent   |100               |55.45             |55.45             |55.17             |56.8                  |
|Avg.Trade     |1645.47           |0.21              |0.21              |0.22              |0.55                  |
|Profit.Factor |NaN               |1.4               |1.4               |1.41              |1.68                  |
|Num.Trades    |5                 |2750              |2750              |2668              |1095                  |
    
	

Another erroneous feature of above approach is automatic reinvestment of dividends. The back-test so far was
based on split and dividend adjusted prices. In reality, the dividends are deposited into account as cash and
allocated during next re-balance. Let's switch to raw, un-adjusted, prices and properly incorporate historical
splits and dividends into back-test (s.ew.com.lot.turnover.unadjusted)


![plot of chunk plot-7](/public/images/2015-10-19-Backtest-Reality-Check/plot-7-1.png) 

|              |r.ew              |s.ew              |s.ew.com          |s.ew.com.lot      |s.ew.com.lot.turnover |s.ew.com.lot.turnover.unadjusted |
|:-------------|:-----------------|:-----------------|:-----------------|:-----------------|:---------------------|:--------------------------------|
|Period        |Jan1970 - Oct2015 |Jan1970 - Oct2015 |Jan1970 - Oct2015 |Jan1970 - Oct2015 |Jan1970 - Oct2015     |Jan1970 - Oct2015                |
|Cagr          |12                |11.53             |11.03             |11.03             |11.12                 |11.4                             |
|Sharpe        |0.65              |0.63              |0.61              |0.61              |0.61                  |0.62                             |
|DVR           |0.52              |0.51              |0.49              |0.49              |0.49                  |0.5                              |
|R2            |0.8               |0.81              |0.81              |0.81              |0.81                  |0.81                             |
|Volatility    |20.94             |20.86             |20.85             |20.85             |20.82                 |20.71                            |
|MaxDD         |-60.67            |-60.88            |-60.9             |-60.9             |-60.79                |-60.66                           |
|Exposure      |99.82             |99.82             |99.82             |99.82             |99.82                 |99.82                            |
|Win.Percent   |100               |55.45             |55.45             |55.17             |56.8                  |54.05                            |
|Avg.Trade     |1645.47           |0.21              |0.21              |0.22              |0.55                  |0.21                             |
|Profit.Factor |NaN               |1.4               |1.4               |1.41              |1.68                  |1.18                             |
|Num.Trades    |5                 |2750              |2750              |2668              |1095                  |1012                             |
    

Another missing feature of above approach is taxes. In reality, unless you invest in tax
sheltered account, the taxes are due at the end of the year. Let's add tax event to the back-test
on the last day in April each year (s.ew.com.lot.turnover.unadjusted.tax)


![plot of chunk plot-8](/public/images/2015-10-19-Backtest-Reality-Check/plot-8-1.png) 

|              |r.ew              |s.ew              |s.ew.com          |s.ew.com.lot      |s.ew.com.lot.turnover |s.ew.com.lot.turnover.unadjusted |s.ew.com.lot.turnover.unadjusted.tax |
|:-------------|:-----------------|:-----------------|:-----------------|:-----------------|:---------------------|:--------------------------------|:------------------------------------|
|Period        |Jan1970 - Oct2015 |Jan1970 - Oct2015 |Jan1970 - Oct2015 |Jan1970 - Oct2015 |Jan1970 - Oct2015     |Jan1970 - Oct2015                |Jan1970 - Oct2015                    |
|Cagr          |12                |11.53             |11.03             |11.03             |11.12                 |11.4                             |9.48                                 |
|Sharpe        |0.65              |0.63              |0.61              |0.61              |0.61                  |0.62                             |0.54                                 |
|DVR           |0.52              |0.51              |0.49              |0.49              |0.49                  |0.5                              |0.45                                 |
|R2            |0.8               |0.81              |0.81              |0.81              |0.81                  |0.81                             |0.84                                 |
|Volatility    |20.94             |20.86             |20.85             |20.85             |20.82                 |20.71                            |20.84                                |
|MaxDD         |-60.67            |-60.88            |-60.9             |-60.9             |-60.79                |-60.66                           |-61.24                               |
|Exposure      |99.82             |99.82             |99.82             |99.82             |99.82                 |99.82                            |99.82                                |
|Win.Percent   |100               |55.45             |55.45             |55.17             |56.8                  |54.05                            |54.25                                |
|Avg.Trade     |1645.47           |0.21              |0.21              |0.22              |0.55                  |0.21                             |0.22                                 |
|Profit.Factor |NaN               |1.4               |1.4               |1.41              |1.68                  |1.18                             |1.19                                 |
|Num.Trades    |5                 |2750              |2750              |2668              |1095                  |1012                             |929                                  |
    




#Average Annual Portfolio Turnover
    


![plot of chunk plot-8](/public/images/2015-10-19-Backtest-Reality-Check/plot-8-2.png) 


#Events for s.ew.com.lot.turnover.unadjusted.tax:
    




|           |Type     |MMM       |AA        |CAT       |KO        |HPQ       |Cash      |Com       |Div       |Value     |
|:----------|:--------|:---------|:---------|:---------|:---------|:---------|:---------|:---------|:---------|:---------|
|2015-03-12 |dividend |    9,100 |  103,700 |   18,500 |   35,400 |   44,000 |   20,208 |        0 |   11,930 |7,307,318 |
|2015-03-30 |trade    |    8,800 |  103,700 |   18,000 |   36,000 |   44,000 |    2,027 |       44 |        0 |7,119,319 |
|2015-03-31 |trade    |    8,800 |  103,700 |   18,000 |   36,000 |   44,000 |    2,027 |        0 |        0 |7,064,770 |
|2015-04-16 |dividend |    8,800 |  103,700 |   18,000 |   36,000 |   44,000 |   14,609 |        0 |   12,582 |7,292,565 |
|2015-04-30 |trade    |    8,800 |  103,700 |   18,000 |   36,000 |   44,000 |   14,609 |        0 |        0 |7,257,175 |
|2015-05-07 |dividend |    8,800 |  103,700 |   18,000 |   36,000 |   44,000 |   17,720 |        0 |    3,111 |7,300,235 |
|2015-05-20 |dividend |    8,800 |  103,700 |   18,000 |   36,000 |   44,000 |   26,731 |        0 |    9,011 |7,333,230 |
|2015-05-29 |trade    |    8,800 |  103,700 |   18,000 |   36,000 |   44,000 |   26,731 |        0 |        0 |7,202,805 |
|2015-06-08 |dividend |    8,800 |  103,700 |   18,000 |   36,000 |   44,000 |   34,475 |        0 |    7,744 |7,117,612 |
|2015-06-11 |dividend |    8,800 |  103,700 |   18,000 |   36,000 |   44,000 |   46,391 |        0 |   11,916 |7,168,432 |
|2015-06-30 |trade    |    8,800 |  122,400 |   16,100 |   34,800 |   45,500 |      832 |      273 |        0 |6,819,693 |
|2015-07-16 |dividend |    8,800 |  122,400 |   16,100 |   34,800 |   45,500 |   13,277 |        0 |   12,445 |6,855,762 |
|2015-07-31 |trade    |    8,800 |  134,500 |   16,800 |   32,300 |   43,500 |    2,336 |      213 |        0 |6,637,131 |
|2015-08-05 |dividend |    8,800 |  134,500 |   16,800 |   32,300 |   43,500 |    6,371 |        0 |    4,035 |6,618,089 |
|2015-08-19 |dividend |    8,800 |  134,500 |   16,800 |   32,300 |   43,500 |   15,303 |        0 |    8,932 |6,348,626 |
|2015-08-31 |trade    |    8,800 |  134,500 |   16,800 |   32,300 |   43,500 |   15,303 |        0 |        0 |6,311,998 |
|2015-09-04 |dividend |    8,800 |  134,500 |   16,800 |   32,300 |   43,500 |   22,698 |        0 |    7,395 |6,176,036 |
|2015-09-11 |dividend |    8,800 |  134,500 |   16,800 |   32,300 |   43,500 |   33,357 |        0 |   10,659 |6,207,178 |
|2015-09-30 |trade    |    8,600 |  126,100 |   18,600 |   30,300 |   47,600 |      231 |      215 |        0 |6,087,947 |
|2015-10-16 |trade    |    8,500 |  132,700 |   18,200 |   30,100 |   43,900 |    1,761 |      160 |        0 |6,332,928 |
    
	

I feel a lot more comfortable with latest version of back-test result and corresponding statistics
because it resembles reality.

There are still more issues that one might want to incorporate into their back-test settings. Here are few ideas:

* if allocation is based on the signal, unlike the sample strategy above, you might want to add execution lag.
i.e. signal is generated on the second to last day of the month and execution takes place on the last day of the month

* consider various cash flows over the life of the back-test. for example, 
	+ a young investor, in his 20's, contributes 5% of initial capital each year for the first 10 years, 
	+ next there are small withdrawals of 1% of initial capital each year for the next 30 years to cover family expenses, 
	+ finally, in retirement stage the withdrawals raise to 5% of portfolio equity each year

In conclusion, do not blindly trust the back-test numbers and corresponding statistics, consider if back-test is actually
a good simulation of real portfolio performance. 


Please note that the supporting code for this post is still in development. If you want to experiment at your own
risk, please sign up for a beta testing by filling up the contact form.








*(this report was produced on: 2015-10-19)*
