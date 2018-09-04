#include <Rcpp.h>
using namespace Rcpp;

// [[Rcpp::export]]
NumericVector euc_dist_cpp(NumericMatrix x, NumericMatrix y) {
    int xrow = x.nrow();
    NumericVector out(xrow);
    
    for (int i = 0; i < xrow; i++) {
        out[i] =  sqrt(pow(x(i,0)-y(i,0), 2) + pow(x(i,1)-y(i,1), 2));
    }
    return out;
}
