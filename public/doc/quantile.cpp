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