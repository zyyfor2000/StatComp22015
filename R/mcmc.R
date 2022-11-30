
#' @title A random walk Metropolis sampler using R for t distribution
#' @description A random walk Metropolis sampler using R
#' @param n degree of freedom of t distribution
#' @param sigma standard variance of proposal distribution N(xt,sigma^2)
#' @param x0 initial value
#' @param burn burn-in length
#' @param N size of random numbers required
#' @return a random sample of size \code{N-burn}
#' @examples
#' \dontrun{
#' X = rwMetropolisR(2,1,1,100,2) #n = 2
#' a = c(0.05,seq(0.1,0.9,0.1),0.95)
#' Q = qt(a,2)
#' qqplot(a,quantile(X,a))
#' }
#' @export
rwMetropolisR = function(n, sigma, x0, N, burn) {
    x = numeric(N)
    x[1] = x0
    u = runif(N)
    for (i in 2:N) {
    y = rnorm(1, x[i-1], sigma)
    if (u[i] <= (dt(y, n) / dt(x[i-1], n)))
      x[i] = y  
    else {
      x[i] = x[i-1]
    }
  }
  return(x[(burn+1):N])
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
