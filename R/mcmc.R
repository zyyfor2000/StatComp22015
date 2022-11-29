#' @title Benchmark R and Rcpp functions.
#' @name benchmarks
#' @description Use R package \code{microbenchmark} to compare the performance of C functions (\code{gibbsR} and \code{vaccR}) and Cpp functions (\code{gibbsC} and \code{vaccC}).
#' @examples
#' \dontrun{
#' data(data)
#' attach(data)
#' tm1 <- microbenchmark::microbenchmark(
#'   rnR = gibbsR(100,10),
#'   rnC = gibbsC(100,10)
#' )
#' print(summary(tm1)[,c(1,3,5,6)])
#' }
#' @import microbenchmark
#' @import lattice
#' @import xtable
#' @importFrom Rcpp evalCpp
#' @importFrom stats rnorm rgamma
#' @useDynLib StatComp22015
NULL

#' @title A dataset used for illustration.
#' @name data
#' @description This dataset is used to compare the performance of C function \code{vaccR}) and C++ function \code{vaccC}.
#' @examples
#' \dontrun{
#' data(data)
#' attach(data)
#' tm <- microbenchmark::microbenchmark(
#'   vR = vaccR(age,female,ily),
#'   vC = vaccC(age,female,ily)
#' )
#' print(summary(tm)[,c(1,3,5,6)])
#' }
NULL

#' @title Use three inputs to predict response using R.
#' @description The prediction model is described in http://www.babelgraph.org/wp/?p=358.
#' @param age the first predictor (numeric)
#' @param female the second predictor (logical)
#' @param ily the third predictor (logical)
#' @return a random sample of size \code{n}
#' @examples
#' \dontrun{
#' data(data)
#' attach(data)
#' res <- vaccR(age,female,ily)
#' }
#' @export
vaccR <- function(age, female, ily) {
  p <- 0.25 + 0.3 * 1 / (1 - exp(0.04 * age)) + 0.1 * ily
  p <- p * ifelse(female, 1.25, 0.75)
  p <- pmax(0, p)
  p <- pmin(1, p)
  p
}

#' @title A Gibbs sampler using R for bivariate normal distribution
#' @description A Gibbs sampler using R
#' @param N the number of samples
#' @param burn burn-in length
#' @param thin the number of between-sample random numbers
#' @param mu1 N(mu1,mu2,sigma1^2,sigma2^2,rho)
#' @param mu2 N(mu1,mu2,sigma1^2,sigma2^2,rho)
#' @param sigma1 N(mu1,mu2,sigma1^2,sigma2^2,rho)
#' @param sigma2 N(mu1,mu2,sigma1^2,sigma2^2,rho)
#' @param rho N(mu1,mu2,sigma1^2,sigma2^2,rho)
#' @return a random sample of size \code{N-burn}
#' @examples
#' \dontrun{
#' X = gibbsR(10000,1000,10,0,0,1,1,0.9)
#' plot(mat, main="", cex=.5, xlab=bquote(X[1]),ylab=bquote(X[2]), ylim=range(x[,2]))
#' }
#' @export
gibbsR = function(N,burn,thin,mu1,mu2,sigma1,sigma2,rho) {
  mat = matrix(0,nrow = N, ncol = 2)
  mat[1,] = c(mu1,mu2)
  s1 = sqrt(1-rho^2)*sigma1
  s2 = sqrt(1-rho^2)*sigma2
  for (i in 2:N) {
    for (j in 1:thin) {
      y = mat[i-1, 2]
      m1 = mu1 + rho * (y - mu2) * sigma1/sigma2
      x = rnorm(1, m1, s1)
      m2 = mu2 + rho * (x - mu1) * sigma2/sigma1
      y = rnorm(1, m2, s2)
    }
    mat[i, ] = c(x, y)
  }
  mat[(burn+1):N,]
}
