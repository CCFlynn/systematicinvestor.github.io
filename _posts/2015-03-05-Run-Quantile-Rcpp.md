---
layout: post
title: Run Quantile in Rcpp
comments: true
---


To install [Systematic Investor Toolbox (SIT)](https://github.com/systematicinvestor/SIT) please visit [About](',base.url,'about) page.





In the [A 'Simple' Tactical Asset Allocation Portfolio with Percentile Channels (for Dummies)](https://cssanalytics.wordpress.com/2015/02/08/a-simple-tactical-asset-allocation-portfolio-with-percentile-channels-for-dummies/)
post, David Varadi discussed a Channel Breakout system that required to estimate moving window quantile.

This is a time consuming operation that can be speed up with runquantile function in caTools package.

Alternatively one can write a simple Rcpp function to speed up commutations If you are just starting 
with Rcpp, I found following resources extremely useful:

* [High performance functions with Rcpp chaoter in the Advanced R by Hadley Wickham](http://adv-r.had.co.nz/Rcpp.html)
* [Rcpp Gallery](http://gallery.rcpp.org/)

To get started on Windows you would need to install [Rtools](http://cran.r-project.org/bin/windows/Rtools/)

Below are sample functions to compute given quantile. The run_quantile0 function is basic and not
very efficient because it sorts all elements in each window. The second function, run_quantile
function takes advantage of previous order and only insert new element in the sorted array.

{% highlight cpp %}
#include <Rcpp.h>
using namespace Rcpp;

// [[Rcpp::plugins(cpp11)]]

// quantile setup
struct quantile {
	int lo, hi, n, total;
	double hlo, hhi;
};
quantile make_quantile(int n, double prob) {
	quantile res;
	double index = (n - 1) * prob;
	res.lo = floor(index);
	res.hi = res.lo + 1;
	res.hhi = index - res.lo;
	res.hlo = 1 - res.hhi;
	return res;	
}

// index vector
std::vector<int> make_seq(int n) {
	std::vector<int> id(n);
	std::iota(id.begin(), id.end(), 0);
	return id;
}

// [[Rcpp::export]]
NumericVector run_quantile0(NumericVector x, int n, double prob) {
	auto sz = x.size();
	NumericVector res(sz);	
	
	// quantile setup
	auto q = make_quantile(n, prob);

    for(int i = 0; i < (sz-n+1); i++) {
    	// can be made a lot faster by not re-sorting each time
    	std::vector<double> z(&x[i], &x[i+n]);
    	std::sort(z.begin(), z.end());    	
        res[i+n-1] = q.hlo * z[q.lo] + q.hhi * z[q.hi];  
    }
    
    // pad the first n-1 elements with NA
    std::fill(res.begin(), res.end()-sz+n-1, NA_REAL);
    return res;	
}

// [[Rcpp::export]]
NumericVector run_quantile(NumericVector x, int n, double prob) {
	auto sz = x.size();
	NumericVector res(sz);	
	
	// quantile setup
	auto q = make_quantile(n, prob);

	// index vector
	auto id = make_seq(n);
	
	std::sort(id.begin(), id.end(), 
		[&](int a, int b) { return x[a] < x[b]; });
	res[n-1] = q.hlo * x[id[q.lo]] + q.hhi * x[id[q.hi]];  	
		
    for(int i = 1; i < (sz-n+1); i++) {
    	// remove index (i-1)
    	id.erase(std::find(id.begin(), id.end(), i-1));
    	// insert keeping sorted order
    	id.insert(std::lower_bound(id.begin(), id.end(), i+n-1, 
    		[&](int a, int b) { return x[a] < x[b]; }), i+n-1);
    
        res[i+n-1] = q.hlo * x[id[q.lo]] + q.hhi * x[id[q.hi]];  
    }
    
    // pad the first n-1 elements with NA
    std::fill(res.begin(), res.end()-sz+n-1, NA_REAL);
    return res;	
}
{% endhighlight %}

Please save above code in the `quantile.cpp` file or download [quantile.cpp](/public/doc/quantile.cpp).

Next let's check for correctness and speed. 


{% highlight r %}
#*****************************************************************
# Rcpp Run Quantile
#*****************************************************************
# make sure to install Rtools on windows
# http://cran.r-project.org/bin/windows/Rtools/
library(SIT)
load.packages('Rcpp')

# load run quantile functions
sourceCpp('quantile.cpp')

#*****************************************************************
# Test functionality and speed
#*****************************************************************
# inefficient R implementation
run_quantileR = function(x, n, probs) {
	out = c( rep(NA,(n-1)), sapply(n:len(x), function(i) quantile(x[(i- (n-1) ):i], probs) ))
	as.vector(out)
}	


# basic test
load.packages('microbenchmark')

n = 10
probs = 0.5
x = runif(100)

test1 = run_quantileR(x, n, probs)
test2 = run_quantile0(x, n, probs)
test3 = run_quantile(x, n, probs)

all.equal(test1, test2)
{% endhighlight %}

[1] TRUE


{% highlight r %}
all.equal(test1, test3)
{% endhighlight %}

[1] TRUE


{% highlight r %}
print(microbenchmark(
	run_quantileR(x, n, probs),
	run_quantile0(x, n, probs),
	run_quantile(x, n, probs),
	times = 10
))
{% endhighlight %}



<pre>
Unit: microseconds
                       expr       min        lq       mean     median
 run_quantileR(x, n, probs) 13430.039 13595.727 14176.8040 14047.3915
 run_quantile0(x, n, probs)    39.605    40.688    48.2192    48.1685
  run_quantile(x, n, probs)    12.329    12.595    16.6316    14.0025
        uq       max neval
 14834.662 15255.772    10
    56.068    56.918    10
    17.859    29.899    10
</pre>
    

Second implementation, `run_quantile`, really shines over larger lock back window


{% highlight r %}
n = 1000
probs = 0.9
x = runif(100000)

test1 = run_quantile0(x, n, probs)
test2 = run_quantile(x, n, probs)

all.equal(test1, test2)
{% endhighlight %}

[1] TRUE


{% highlight r %}
print(microbenchmark(
	run_quantile0(x, n, probs),
	run_quantile(x, n, probs),
	times = 1
))
{% endhighlight %}



<pre>
Unit: milliseconds
                       expr        min         lq       mean     median
 run_quantile0(x, n, probs) 5538.88588 5538.88588 5538.88588 5538.88588
  run_quantile(x, n, probs)   81.01661   81.01661   81.01661   81.01661
         uq        max neval
 5538.88588 5538.88588     1
   81.01661   81.01661     1
</pre>
    

To be continued.


*(this report was produced on: 2015-03-06)*
