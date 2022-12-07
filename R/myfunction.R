
#' @title random numbers for truncated distribution
#' @description to generate random numbers for truncated normal distribution in the function sim_result
#' @param n the number of random numbers you want to generate
#' @param distr the distribution name
#' @param lower the left trunction
#' @param upper the right trunction
#' @param ... some manual setting
#' @return random numbers you want
#' @examples
#' \dontrun{
#'x = rtrunc(10, "norm", lower = 0, mean = 2, sd = 5)
#'}
#' @export
rtrunc = function(n, distr, lower = -Inf, upper = Inf, ...){
  makefun = function(prefix, FUN, ...){
    txt = paste(prefix, FUN, "(x, ...)", sep = "")
    function(x, ...) eval(parse(text = txt))
  }
  if(length(n) > 1) n = length(n)
  pfun = makefun("p", distr, ...)
  qfun = makefun("q", distr, ...)
  lo = pfun(lower, ...)
  up = pfun(upper, ...)
  u = runif(n, lo, up)
  return(qfun(u, ...))
}




#' @title A function for the simulation in the paper
#' @description to manipulate the 3 methods proposed in the paper and 6 nonparametric methods 
#' @param n_replication number of replication 
#' @param n number of data you already know
#' @param sigma2 ture varaince in the simulation
#' @return a line chart including the tendecy of different methods I used
#' @examples
#' \dontrun{
#' begin = Sys.time()
#' n_total = c(5,10,20,100)
#' result = array(data = 0, dim = c(4,9))
#' n_replication = 1000
#' sigma2 = 1
#' for (i in 1:4){
#'      a = sim_result(n_replication,n,sigma2)
#'      result[i,] = as.matrix(a)
#'  }
#' end = Sys.time()
#' print(end-begin)
#' print(result)
#'}
#' @export
sim_result = function(n_replication,n,sigma2){
  # parameter setting
  mu = 4
  b =50
  h = 1
  L = 5
  # population generating
  x_all = rnorm(n_replication,10,sqrt(sigma2))
  x_l_all = rep(0,n_replication)
  for (i in c(1:n_replication)){
    x_l_all[i] = sum(sample(x_all,5))
  }
  r_all = 100 # 100 data sets are simulated
  tc_all = array(data = 0, dim = c(r_all,3)) # 3 methods
  tc_np = array(data = 0, dim = c(r_all,6)) # 6 methods
  for (r in c(1:r_all)){
    data_known = sample(x_all,n)
    mu_hat = mean(data_known)
    sigma2_hat = var(data_known)
    # exact method
    z_exact = rep(0,n_replication)
    for (j in c(1:n_replication)){
      sigma2_hat_1 = (n-1)*sigma2_hat/rchisq(1,n-1)
      mu_hat_1 = mu_hat + sqrt(sigma2_hat_1)*rnorm(1)/sqrt(n)
      z_exact[j] = rnorm(1,L*mu_hat_1,sqrt(L*sigma2_hat_1))
    }
    S_exact = quantile(z_exact,b/(b+h))
    z_exact_new = S_exact - x_l_all
    TC_exact = (sum(h * z_exact_new[z_exact_new > 0]) -
                  sum(b * z_exact_new[z_exact_new < 0])) / n_replication
    tc_all[r,1] = TC_exact
    # classic method
    S_classic = L * mu_hat + sqrt(L) * (-qnorm(b / (b + h))) * sqrt(sigma2_hat)
    z_classic = rnorm(n_replication, L * mu_hat, sqrt(L * sigma2_hat))
    z_classic_new = S_classic - x_l_all
    TC_classic = (sum(h * z_classic_new[z_classic_new > 0]) -
                    sum(b * z_classic_new[z_classic_new < 0])) / n_replication
    tc_all[r, 2] = TC_classic
    # asymptotic method
    z_asymtotic = rep(0,n_replication)
    left_bound = -sqrt(n / 2)
    for (j in c(1:n_replication)){
      sigma2_hat_1 = sigma2_hat + sqrt(2 * sigma2_hat ** 2 / n) * rtrunc(1, "norm",left_bound, Inf,  mean = 2, sd = 1)
      mu_hat_1 = mu_hat + sqrt(sigma2_hat / n) * rnorm(1)
      z_asymtotic[j] = rnorm(1,L * mu_hat_1,sqrt(L * sigma2_hat_1))
    }
    S_asymtotic = quantile(z_asymtotic, b / (b + h))
    z_asymtotic_new = S_asymtotic - x_l_all
    TC_asymtotic = (sum(h * z_asymtotic_new[z_asymtotic_new > 0]) -
                      sum(b * z_asymtotic_new[z_asymtotic_new < 0])) / n_replication
    tc_all[r, 3] = TC_asymtotic
    #s1
    S_1 = quantile(data_known, b / (b + h))*L
    z_1 = S_1 - x_l_all
    TC_1 = (sum(h * z_1[z_1 > 0]) -
              sum(b * z_1[z_1 < 0])) / n_replication
    tc_np[r, 1] = TC_1
    #s2
    n_1 = as.integer(floor(n * 0.5))
    n_2 = as.integer(floor(n * 0.8))
    S = rep(0,n_replication)
    for (j in c(1:100)){
      S[j] = quantile(sample(data_known, n_1, replace=TRUE), b / (b + h))
    }
    S_21 = mean(S)*L
    z_21 = S_21 - x_l_all
    TC_21 = (sum(h * z_21[z_21 > 0]) -sum(b * z_21[z_21 < 0])) / n_replication
    tc_np[r, 2] = TC_21
    S = rep(0,n_replication)
    for (j in c(1:100)){
      S[j] = quantile(sample(data_known, n_2, replace=TRUE), b / (b + h))
    }
    S_22 = mean(S)*L
    z_22 = S_22 - x_l_all
    TC_22 = (sum(h * z_22[z_22 > 0]) - sum(b * z_22[z_22 < 0])) / n_replication
    tc_np[r, 3] = TC_22
    # 3. SVP1-3
    data_ordered = sort(data_known)
    s_1 = 0
    p = b / (b + h)
    for (i in 2:(n-1)){
      s_1 = s_1 + (dbinom(i, n, p)+dbinom(i-1, n, p)) / 2 * data_ordered[i]
    }
    S_31 = (2*dbinom(0,n,p)+dbinom(1,n,p))/2*data_ordered[1]+dbinom(0,n,p)/2*data_ordered[2]-dbinom(0,n,p)/2*data_ordered[3]+s_1-dbinom(n,n,p)/2*data_ordered[n-2]+dbinom(n,n,p)/2*data_ordered[n-1]+(2*dbinom(n,n,p)+dbinom(n-1,n,p))/2*data_ordered[n]
    z_31 = S_31*L - x_l_all
    TC_31 = (sum(h * z_31[z_31 > 0]) - sum(b * z_31[z_31 < 0])) / n_replication
    tc_np[r, 4] = TC_31
    s_2 = 0
    for (i in 0:(n-1)) {
      s_2 = s_2 + dbinom(i,n,p)*data_ordered[i+1]
    }
    S_32 = s_2 +(2*data_ordered[n]-data_ordered[n-1])*dbinom(n,n,p)
    z_32 = S_32*L - x_l_all
    TC_32 = (sum(h * z_32[z_32 > 0]) - sum(b * z_32[z_32 < 0])) / n_replication
    tc_np[r, 5] = TC_32
    s_3 = 0
    for (i in 1:n){
      s_3 = s_3 + dbinom(i,n,p)*data_ordered[i]
    }
    S_33 = s_3 +(2*data_ordered[1]-data_ordered[2])*dbinom(0,n,p)
    z_33 = S_33*L - x_l_all
    TC_33 = (sum(h * z_33[z_33 > 0]) - sum(b * z_33[z_33 < 0])) / n_replication
    tc_np[r,6] = TC_33
  }
  result1 = as.vector(apply(tc_all,2,mean))
  result2 = as.vector(apply(tc_np, 2,mean))
  return(c(result1,result2))
}

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