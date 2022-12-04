

#' @title A graph show about my simulation
#' @description to instaintiate the table data by using the graph
#' @param counter indicator of which sigma it comes from
#' @param result the raw data about my simulation result
#' @return a line chart including the tendecy of different methods I used
#' @examples
#' \dontrun{
#'result = array(data = 0, dim = c(10,4,9))
#'for (i in 1:10){
#'  a = read.table(paste("../data/text",i,".txt",sep = ""))
#'  a = a[,c(2,1,3,4,5,6,7,8,9)]
#'  result[i,,] = as.matrix(a)
#'  }
#'for (i in 1:4){
#'  par(mfrow = c(1,1))
#'  plot_s(i)
#'}
#'}
#' @export
plot_s = function(counter,result){
  sigma = c(1:10)
  par(mai = c(0.8,0.8,0.8,1))
  n_total = c(5,10,20,100)
  plot(c(1:9),result[1,counter,],type = "l",col=1,ylim=c(min(result[,counter,]-5),max(result[,counter,])+5),ylab = "total cost" ,xlab = "method",main = bquote(n == .(n_total[counter])))
  axis(1,"m",at = seq(1,9,1),labels = TRUE)
  for (i in 2:10){
    lines(result[i,counter,],type = "l",col=i)
  }
  usr = par("usr")
  x = usr[2]*1.02
  y = usr[4]*0.9
  legend(x,y, legend = sigma,lty=1,col = 1:10,ncol = 1,title = expression(sigma^2),xpd = TRUE)
}