#include <Rcpp.h>
using namespace Rcpp;

//' @title A Gibbs sampler using Rcpp for bivariate normal distribution
//' @description A Gibbs sampler using Rcpp
//' @param N the number of samples
//' @param burn burn-in length
//' @param thin the number of between-sample random numbers
//' @param mu1 N(mu1,mu2,sigma1^2,sigma2^2,rho)
//' @param mu2 N(mu1,mu2,sigma1^2,sigma2^2,rho)
//' @param sigma1 N(mu1,mu2,sigma1^2,sigma2^2,rho)
//' @param sigma2 N(mu1,mu2,sigma1^2,sigma2^2,rho)
//' @param rho N(mu1,mu2,sigma1^2,sigma2^2,rho)
//' @return a random sample of size \code{N-burn}
//' @examples
//' \dontrun{
//' X = gibbsC(10000,1000,10,0,0,1,1,0.9)
//' plot(mat, main="", cex=.5, xlab=bquote(X[1]),ylab=bquote(X[2]), ylim=range(x[,2]))
//' }
//' @export
// [[Rcpp::export]]
NumericMatrix gibbsC(int N,int burn, int thin, double mu1, double mu2,double sigma1,double sigma2,double rho) {
  NumericMatrix mat(N, 2);
  double m1,m2;
  double x = mu1, y = mu2;
  mat(0,0) = x;
  mat(0,1) = y;
  double s1 = sqrt(1-pow(rho,2))*sigma1;
  double s2 = sqrt(1-pow(rho,2))*sigma2;
  for(int i = 1; i < N; i++) {
    for(int j = 0; j < thin; j++) {
      y = mat(j-1, 2);
      m1 = mu1 + rho * (y - 0) * sigma1/sigma2;
      x = rnorm(1, m1, s1)[0];
      m2 = mu2 + rho * (x - 0) * sigma2/sigma1;
      y = rnorm(1, m2, s2)[0];
    }
    mat(i, 0) = x;
    mat(i, 1) = y;
  }
  NumericMatrix mlast = mat( Range(burn,N-1) ,Range(0,1));
  return(mlast);
}


//' @title A random walk Metropolis sampler using R for t distribution using Rcpp.
//' @description A random walk Metropolis sampler using R.
//' @param n degree of freedom of t distribution
//' @param sigma standard variance of proposal distribution N(xt,sigma^2)
//' @param x0 initial value
//' @param burn burn-in length
//' @param N size of random numbers required
//' @return a random sample of size \code{N-burn}
//' @examples
//' \dontrun{
//' X = rwMetropolisC(2,1,1,100,2) #n = 2
//' a = c(0.05,seq(0.1,0.9,0.1),0.95)
//' Q = qt(a,2)
//' qqplot(a,quantile(X,a))
//' }
//' @export
// [[Rcpp::export]]
NumericVector rwMetropolisC(int n, double sigma, double x0, int N, int burn) {
  NumericVector x(N);
  x[1] = x0;
  for (int i = 1; i < N; i++) {
    double u = runif(1)[0];
    double y = rnorm(1, x[i-1], sigma)[0];
    double tmp1 = ::Rf_dt(y, n, 0) ;
    double tmp2 = ::Rf_dt(x[i-1], n, 0);
    double tmp = tmp1/tmp2;
    if (u <= tmp){
      x[i] = y  ;
    }
      else {
        x[i] = x[i-1];
      }
  }
  NumericVector xlast = x[Range(burn,N-1)];
  return(xlast);
}
