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

#' @title my simulation data for sigma=1
#' @name text1
#' @description A dataset in introduction.
#' @examples
#' \dontrun{
#' data1 = read.table("../data/text1.txt")
#' }
NULL

#' @title my simulation data for sigma=2
#' @name text2
#' @description A dataset in introduction.
#' @examples
#' \dontrun{
#' data1 = read.table("../data/text2.txt")
#' }
NULL

#' @title my simulation data for sigma=3
#' @name text3
#' @description A dataset in introduction.
#' @examples
#' \dontrun{
#' data1 = read.table("../data/text3.txt")
#' }
NULL

#' @title my simulation data for sigma=4
#' @name text4
#' @description A dataset in introduction.
#' @examples
#' \dontrun{
#' data1 = read.table("../data/text4.txt")
#' }
NULL

#' @title my simulation data for sigma=5
#' @name text5
#' @description A dataset in introduction.
#' @examples
#' \dontrun{
#' data1 = read.table("../data/text5.txt")
#' }
NULL

#' @title my simulation data for sigma=6
#' @name text6
#' @description A dataset in introduction.
#' @examples
#' \dontrun{
#' data1 = read.table("../data/text6.txt")
#' }
NULL

#' @title my simulation data for sigma=7
#' @name text7
#' @description A dataset in introduction.
#' @examples
#' \dontrun{
#' data1 = read.table("../data/text7.txt")
#' }
NULL

#' @title my simulation data for sigma=8
#' @name text8
#' @description A dataset in introduction.
#' @examples
#' \dontrun{
#' data1 = read.table("../data/text8.txt")
#' }
NULL

#' @title my simulation data for sigma=9
#' @name text9
#' @description A dataset in introduction.
#' @examples
#' \dontrun{
#' data1 = read.table("../data/text9.txt")
#' }
NULL

#' @title my simulation data for sigma=10
#' @name text10
#' @description A dataset in introduction.
#' @examples
#' \dontrun{
#' data1 = read.table("../data/text10.txt")
#' }
NULL