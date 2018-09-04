#include <Rcpp.h>
#include <RcppMLPACK.h>
using namespace Rcpp;

using namespace mlpack::kmeans;
using namespace Rcpp;

// [[Rcpp::depends(RcppMLPACK)]]
// [[Rcpp::export]]
List cppKmeans(const arma::mat& data, const int& clusters) {
arma::Col<size_t> assignments;
KMeans<> k; // Initialize with the default arguments.
k.Cluster(data, clusters, assignments);
return List::create(Named("clusters") = clusters, 
                    Named("result") = assignments);
}
