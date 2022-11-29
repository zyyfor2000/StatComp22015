#include <Rcpp.h>
using namespace Rcpp;

//' @title A Gibbs sampler using Rcpp
//' @description A Gibbs sampler using Rcpp
//' @param N the number of samples
//' @param thin the number of between-sample random numbers
//' @return a random sample of size \code{n}
//' @examples
//' \dontrun{
//' rnC <- gibbsC(100,10)
//' par(mfrow=c(2,1));
//' plot(rnC[,1],type='l')
//' plot(rnC[,2],type='l')
//' }
//' @export
// [[Rcpp::export]]
NumericMatrix gibbsC(int N, int thin) {
  NumericMatrix mat(N, 2);
  double x = 0, y = 0;
  for(int i = 0; i < N; i++) {
    for(int j = 0; j < thin; j++) {
      x = rgamma(1, 3, 1 / (y * y + 4))[0];
      y = rnorm(1, 1 / (x + 1), 1 / sqrt(2 * (x + 1)))[0];
    }
    mat(i, 0) = x;
    mat(i, 1) = y;
  }
  return(mat);
}


//' @title Use three inputs to predict response using Rcpp.
//' @description The prediction model is described in http://www.babelgraph.org/wp/?p=358.
//' @param n the first predictor (numeric)
//' @return square of \code{n}
//' @examples
//' \dontrun{
//' data(data)引用数据的方式
//' attach(data)
//' res <- test(n)
//' }
//' @export
// [[Rcpp::export]]
int test(int n) {
  int out = n*n;
  return out;
}
