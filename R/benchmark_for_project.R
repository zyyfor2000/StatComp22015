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
#' @import boot
#' @import bootstrap
#' @import DAAG
#' @import MASS
#' @import mediation
#' @import knitr
#' @importFrom Rcpp evalCpp
#' @importFrom stats rnorm rgamma runif dt mahalanobis
#' @importFrom graphics axis legend lines par
#' @useDynLib StatComp22015
NULL


