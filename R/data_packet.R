#' @title A illustration dataset
#' @name data
#' @description A dataset used to illustrate the performance of \code{vaccR} and \code{vaccC}.
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

#' @title A illustration dataset called Bank_loan
#' @name Bank_loan
#' @description A dataset in homework0.
#' @examples
#' \dontrun{
#' data = read.table("../data/Bank_loan.txt",head=TRUE,na.strings = c("NA"))
#' past.customers = subset(data,default != "NA")
#' pie(c(sum(past.customers$default == 1),sum(past.customers$default == 0)),c(1,0))
#' }
NULL