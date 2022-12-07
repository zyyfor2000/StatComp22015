## -----------------------------------------------------------------------------
seq(1, 5, 0.5)
seq(length=9, from=1, to=5)
rep(1,5)
sequence(4:5)
gl(2, 6, label=c("Male", "Female"))
expand.grid(h=c(60,80), w=c(100, 300), sex=c("Male", "Female"))
qnorm(0.975)
matrix(1:6, 2, 3, byrow=TRUE)
ts(1:47, frequency = 12, start = c(1959, 2))

## ----echo=FALSE---------------------------------------------------------------
x = rnorm(10)
y = rnorm(10)
plot(x, y, xlab="Ten random values", ylab="Ten other values",
     xlim=c(-2, 2), ylim=c(-2, 2), pch=22, col="red",
     bg="yellow", bty="l", tcl=0.4,
     main="How to customize a plot with R", las=1, cex=1.5)

## -----------------------------------------------------------------------------
library(lattice)
n <- seq(5, 45, 5)
x <- rnorm(sum(n))
y <- factor(rep(n, n), labels=paste("n =", n))
densityplot(~ x | y,
            panel = function(x, ...) {
                panel.densityplot(x, col="DarkOliveGreen", ...)
                panel.mathdensity(dmath=dnorm,
                                  args=list(mean=mean(x), sd=sd(x)),
                                  col="darkblue")
})

## -----------------------------------------------------------------------------
ricker <- function(nzero, r, K=1, time=100, from=0, to=time) {
    N <- numeric(time+1)
    N[1] <- nzero
    for (i in 1:time) N[i+1] <- N[i]*exp(r*(1 - N[i]/K))
    Time <- 0:time
    plot(Time, N, type="l", xlim=c(from, to))
}
ricker(0.1, 1); title("r = 1")

## -----------------------------------------------------------------------------
xtable::xtable(head(iris))

## -----------------------------------------------------------------------------
data = read.table("../data/Bank_loan.txt",head=TRUE,na.strings = c("NA"))
past.customers = subset(data,default != "NA")
pie(c(sum(past.customers$default == 1),sum(past.customers$default == 0)),c(1,0))
for (i in 1:4){
a = nrow(past.customers[past.customers$default ==0 & past.customers$edu == i,])/nrow(past.customers[past.customers$edu==i,])
b = nrow(past.customers[past.customers$default ==1 & past.customers$edu == i,])/nrow(past.customers[past.customers$edu==i,])
if (i == 1){
  b_0 = b
  a_0 = a
  next
}
a_0 = c(a_0,a)
b_0 = c(b_0,b)
}
probability_on_condition = data.frame(c(1:4),a_0,b_0)

## -----------------------------------------------------------------------------
rm(list = ls() )   

## ---- fig.align='center'------------------------------------------------------
a = 2; b = 2
n = 100000
u = runif(n)
x = b/(1-u)**{1/a}
hist(x, prob = TRUE, main = expression(f(x) == ab^ax^{-a-1}))
y = seq(1,1000,0.01)
lines(y,a*b^a*y^{-a-1})
# we can derive commands about pareto by pakage EnvStats
# library(EnvStats)
# lines(y,dpareto(y,a,b))  this is the same result as above


## -----------------------------------------------------------------------------
f_beta = function(n,c,a,b){   # n:sample size c,a,b:predefined number
  k = 0  #counter for accepted
  j = 0  #iterations
  y = numeric(n)
    while(k<n){
      u = runif(1)
      j = j + 1
      x = runif(1)
      if (x**{a-1}*(1-x)**{b-1}/beta(a,b)/c >u){
        # we accept x
        k = k + 1
        y[k] = x
      }
    }
  y
}

## ----fig.align='center'-------------------------------------------------------
n = 1000
a = 3
b = 2
c = 16/9 
x = f_beta(n,c,a,b)
hist(x,probability = TRUE, main = expression(f(x)==12*x^2*(1-x)),ylim =c(0,2))
y = seq(0,1,0.01)
lines(y,dbeta(y,3,2))

## ----echo=FALSE,fig.align='center'--------------------------------------------
qqplot(x,rbeta(n,3,2),xlab='Accpetance-rejection',ylab='rbeta')
abline(0,1,col='blue',lwd=2)

## ---- fig.align='center'------------------------------------------------------
n = 1000
r = 4
beta = 2
lamda = rgamma(n, r, beta)
x = rexp(n, rate = lamda)
hist(x)

## ---- fig.align='center',eval=FALSE-------------------------------------------
#  # library(EnvStats)
#  hist(x,probability = TRUE, main = expression(f(x)==beta^{gamma}*gamma*(beta+x)^{-r-1},y >= 0))
#  y = seq(0,6,0.01)
#  z = seq(b,6+b,0.01)
#  lines(y,b^{r}*r*(b+y)^{-r-1},col = "blue",lwd = 2)
#  lines(z-b,dpareto(z,location = b,shape = r),col = "red",lwd = 1, lty = 1)

## -----------------------------------------------------------------------------
rm(list = ls() )   

## -----------------------------------------------------------------------------
set.seed(100)
# the quick sort algorithm
quick_sort<-function(x){
  num<-length(x)
  if(num==0||num==1){return(x)
  }else{
    a<-x[1]
    y<-x[-1]
    lower<-y[y<a]
    upper<-y[y>=a]
    return(c(quick_sort(lower),a,quick_sort(upper)))}
}
# apply it to randomly permuted numbers of 1,...,n when n in diferent numbers
for (n in c(1e4,2e4,4e4,6e4,8e4)){
  test<-sample(1:n)
  quick_sort(test)
}
# caculate computation time averaged over 100 simulations
n_simulation = 100
a = rep(0,5)
t = rep(0,5)
i = 1 # counter
for (n in c(1e4,2e4,4e4,6e4,8e4)){
  s_time = 0  # the sum of time
  for (j in c(1:n_simulation)){
  test<-sample(1:n)
  s_time = s_time + system.time(quick_sort(test))[1]
  }
  a[i] = s_time/n_simulation
  t[i] = n*log(n)
  i = i + 1
}
print(a)
print(t)

## ---- fig.align='center'------------------------------------------------------
# scatter plot and regression line
plot(t,a,xlab='theoretical time:nlog(n)', ylab='actual time',main = "Relationship between Theoretical and Actual Time")
abline(lm(a~t),col='red')

## -----------------------------------------------------------------------------
# the caculation process
v1 = 0.5*(exp(2)-1)-(exp(1)-1)**2
v2 = 0.5*(exp(2)-1)-(exp(1)-1)**2 + exp(1) - (exp(1)-1)**2
(r = (v1-v2)/(v1))

## -----------------------------------------------------------------------------
set.seed(100)
MC.Phi <- function(x, R = 10000, antithetic = FALSE) {
  u <- runif(R/2)
  if (antithetic) v <- 1 - u else v <- runif(R/2)
  u <- c(u, v)
  g <- exp(u) # x*u ~ N(0,x)
  cdf <- mean(g) 
  cdf
}
m <- 10000
MC1 <- MC2 <- numeric(m)
x <- 1
for (i in 1:m) {
  MC1[i] <- MC.Phi(x, R = m, antithetic = FALSE)
  MC2[i] <- MC.Phi(x, R = m, antithetic = TRUE)
}
round(c(var(MC1),var(MC2),(var(MC1)-var(MC2))/var(MC1)),5)

## ----fig.align='center'-------------------------------------------------------
x <- seq(1, 20, 0.01)
y <- x^2 * exp(-x^2/2)/sqrt(2 * pi)
plot(x, y, type = "l", ylim = c(0, 1))
lines(x, dnorm(x,mean = 1), lty = 2)
lines(x, dgamma(x,shape = 3,scale = 1), lty = 3)
legend("topright", legend = c("g(x)", "f1", "f2"), lty = 1:3)


## ----fig.align='center'-------------------------------------------------------
plot(x, y/(dnorm(x,mean=1)), type = "l", lty = 3, ylab = "")
lines(x, y/(dgamma(x,shape = 3,scale = 1)), lty = 2)
legend("topright", inset = 0.02, legend = c("g/f1", "g/f2"),
lty = 2:3)

## -----------------------------------------------------------------------------
set.seed(22015)
m = 1e6
est = sd <- numeric(2)
g = function(x) {
x^2 * exp(-x^2/2)/sqrt(2 * pi) * (x > 0)
}
x = rnorm(m,mean=1) #using f1
fg = g(x)/dnorm(x,mean=1)
est[1] = mean(fg)
sd[1] = sd(fg)
x = rgamma(m,shape = 3,scale = 1) #using f2
fg = g(x)/dgamma(x,shape = 3,scale = 1)
est[2] = mean(fg)
sd[2] = sd(fg)
rbind(est,sd)

## -----------------------------------------------------------------------------
I.int <- integrate(function(x) x^2*dnorm(x), 1, Inf)
(I.int$value)

## -----------------------------------------------------------------------------
set.seed(22015)
M = 10000  # number of replicates
k = 5 # number of strata
r = M/k # replicates per strata
# the original function
g = function(x){
  exp(-x)/(1+x^2)*(x>0)*(x<1)
}
# changed f3 on subinterval
f = function(x){
  exp(-x)/(1-exp(-1))
}
# create vectors involving estimated theta and varaince
theta = numeric(k)
var = numeric(k)
sd = numeric(k) #try to see if it is equal to sqrt(var)
for (i in 1:k){
  u = runif(r, (i-1)/k,i/k)
  x = -log(1-(1-exp(-1))*u) #use inverse transform method,of course it is not，because var(sum(theta_i)) = sum(var(theta_i)),se无此性质.
  gf = g(x)/f(x)
  theta[i] = mean(gf)
  var[i] = var(gf)
  sd[i] = sd(gf)
}
# 原
(sum(theta))
(sqrt(sum(var)))
(sum(sd))
# 改
(mean(theta))
(mean(var))

## -----------------------------------------------------------------------------
set.seed(0)
# data generation
generate = function(mu = 10,sigma.2 = 4,m = 1e5,n = 20){
  mu.hat = numeric(m)
  se.hat = numeric(m)
  for (i in 1:m){
  x = rlnorm(n)
  y = log(x)
  mu.hat[i] = mean(y)
  se.hat[i] = sd(y)/sqrt(n)
  }
  result = data.frame(mu.hat,se.hat,n)
  result
}
# data analysis
analysis = function(mu.hat,se.hat,n){
  p.val = 2*(1-pt(abs(mu.hat/se.hat),n-1))
  p.val
}
# data report
report = function(p.val){
  (1-mean(p.val<0.05))
}

# main procedure
result = generate(mu = 10,sigma.2 = 4,m = 1e5,n = 20)
gc()# memory is cleared up before calling each other
p.val = analysis(result$mu.hat,result$se.hat,result$n)
gc()# memory is cleared up before calling each other
report(p.val)

## -----------------------------------------------------------------------------
count5test = function(x,y){
  X = x - mean(x)
  Y = y - mean(y)
  outx = sum(X > max(Y)) + sum(X < min(Y))
  outy = sum(Y > max(X)) + sum(Y < min(X))
  return(as.integer(max(c(outx,outy))>5)) #return 1 (reject) or 0 (do not reject H0)
}
set.seed(22015)
# data generation
generation = function(m,n,mu,sigma1,sigma2){
  data = array(data=0,dim=c(n,2,m))
    for (i in 1:m){
    x = rnorm(n, mu, sigma1)
    y = rnorm(n, mu, sigma2)
   data[,,i] = cbind(x,y)
    }
  data
}
# data anlysis
analysis = function(data,m){
  power1 = numeric(m)
  power2 = numeric(m)
 for (i in 1:m){
   power1[i] = count5test(data[,1,i],data[,2,i])
   power2[i] = as.integer(var.test(data[,1,i],data[,2,i])$p.value <=0.055)
 }
  power = cbind(power1,power2)
  power
}
# data report
report = function(power){
  print(apply(power,2,mean))
}

# main procedure
for (n in c(5,10,20,50,100,200,500,1000)){
  data = generation(1e4,n,0,1,1.5)
  gc()
  power = analysis(data,1e4)
  gc()
  print(n)
  report(power)
}

## -----------------------------------------------------------------------------
p1_hat = 0.651
p2_hat = 0.676
n = 10000
sigma_2_hat = (p1_hat+p2_hat)/2*(1-(p1_hat+p2_hat)/2)
z = (p1_hat-p2_hat)/sqrt(2*sigma_2_hat/n)
2*(1-pnorm(abs(z)))

## -----------------------------------------------------------------------------
set.seed(22015)
library(boot)
data = aircondit$hours
# method 1
lambda.mle = 1/mean(data)
B = 1e3
lambda_estimate = numeric(B)
for (b in 1:B){
  data_sub = sample(data,replace = TRUE)
  lambda_estimate[b] = 1/mean(data_sub)
}
round(c(lambda.mle=lambda.mle,bias=mean(lambda_estimate)-lambda.mle,se.boot = sd(lambda_estimate)),3)
# method 2
lambda <- function(x, i) return(1/mean(x[i]) )
obj = boot(data, statistic = lambda, R = B)
round(c(lambda.mle = obj$t0, bias = mean(obj$t)- obj$t0, se.boot = sd(obj$t)),3)

## -----------------------------------------------------------------------------
rm(list = ls())

## -----------------------------------------------------------------------------
set.seed(22015)
data = aircondit$hours
lambda.inverse = function(x,i) mean(x[i])
de = boot(data = data,statistic = lambda.inverse,R=10000)
ci = boot.ci(de, type = c("norm", "perc", "basic", "bca"))
knitr::kable(data.frame("type"=c("norm", "perc", "basic", "bca"),
                      "ci_left"=c(ci$norm[2],ci$percent[4],ci$basic[4],ci$bca[4]),
                      "ci_right"=c(ci$norm[3],ci$percent[5],ci$basic[5],ci$bca[5])))

## ----fig.align='center'-------------------------------------------------------
hist(de$t, prob = TRUE, main = "") 
points(de$t0, 0, cex = 3, pch = 20)

## -----------------------------------------------------------------------------
rm(list = ls())

## -----------------------------------------------------------------------------
set.seed(22015)
mu =0; sigma = 1
n = 20; m = 10000
boot.mean = function(x,i) mean(x[i])
ci.norm = ci.basic = ci.perc = matrix(NA,m,2)
for(i in 1:m){
  data = rnorm(n,mean = mu,sd = sigma)
  de = boot(data = data,statistic = boot.mean,R=999)
  ci = boot.ci(de,type = c("norm","basic","perc"))
  ci.norm[i,] = ci$norm[2:3]
  ci.basic[i,] = ci$basic[4:5]
  ci.perc[i,] = ci$percent[4:5]
}

knitr::kable(data.frame("type"=c("norm","basic","perc"),
                      "empirical coverage rates"=
                        c(mean(ci.norm[,1]<=mu & ci.norm[,2]>=mu),
                          mean(ci.basic[,1]<=mu & ci.basic[,2]>=mu),
                          mean(ci.perc[,1]<=mu & ci.basic[,2]>=mu)),
                      "p_miss_on_left"=c(mean(ci.norm[,1]>mu),
                                         mean(ci.basic[,1]>mu),
                                         mean(ci.perc[,1]>mu)),
                      "p_miss_on_right"=c(mean(ci.norm[,2]<mu),
                                        mean(ci.basic[,2]<mu),
                                        mean(ci.perc[,2]<mu))))

## -----------------------------------------------------------------------------
rm(list = ls())

## -----------------------------------------------------------------------------
# data generation
library(bootstrap)
attach(scor)
class(scor)
data = as.matrix(scor)
print(summary(data))

## -----------------------------------------------------------------------------
set.seed(22015)
jack_hat = function(data){
  n = nrow(data)
  theta.jack = numeric(n)
  lambda.hat = eigen(cov(data))$values
  theta.hat = lambda.hat[1]/sum(lambda.hat)
  for (i in 1:n){
    lambda.hat = eigen(cov(data[-i,]))$values
    theta.jack[i] = lambda.hat[1]/sum(lambda.hat)
  }
  result = data.frame(theta.hat, theta.jack,n)
}
result = jack_hat(data)
n = result$n[1]
theta.hat = result$theta.hat[1]
theta.jack = result$theta.jack
bias.jack = (n - 1) * (mean(theta.jack) - theta.hat)
se.jack = sqrt((n - 1)/n * mean((theta.jack - mean(theta.jack))^2))
knitr::kable(data.frame("est" = theta.hat, "bias" = bias.jack, "se" = se.jack))

## -----------------------------------------------------------------------------
detach(scor)
detach(package:bootstrap)
rm(list = ls())

## -----------------------------------------------------------------------------
set.seed(22015)
library(DAAG)
attach(ironslag)
#Cross validation is applied to select a model in Example 7.17.
n = length(magnetic) #in DAAG ironslag 
N = choose(n,2) # the times need to calculate leave-two-out
e1 = e2 = e3 = e4 = numeric(N)
counter = 1 
# fit models on leave-two-out samples 
for (i in 1:n-1) 
  for ( j in (i+1):n){
      k = c(i,j)
      y = magnetic[-k]
      x = chemical[-k]
      # model1
      J1 = lm(y ~ x)
      yhat1 = J1$coef[1] + J1$coef[2] * chemical[k] 
      e1[counter] = sum((magnetic[k] - yhat1)**2)
      # model2
      J2 = lm(y ~ x + I(x^2))
      yhat2 = J2$coef[1] + J2$coef[2] * chemical[k] + J2$coef[3] * chemical[k]^2
      e2[counter] = sum((magnetic[k] - yhat2)**2)
      # model3
      J3 = lm(log(y) ~ x) 
      logyhat3 = J3$coef[1] + J3$coef[2] *chemical[k]
      yhat3 = exp(logyhat3) 
      e3[counter] = sum((magnetic[k] - yhat3)**2)
      # model4
      J4 = lm(log(y) ~ log(x))
      logyhat4 = J4$coef[1] + J4$coef[2] * log(chemical[k]) 
      yhat4 = exp(logyhat4)
      e4[counter] = sum((magnetic[k] - yhat4)**2)
      counter = counter + 1
}

## -----------------------------------------------------------------------------
knitr::kable(data.frame("model1" = sum(e1)/N,"model2" = sum(e2)/N, "model3" = sum(e3)/N, "model4" = sum(e4)/N))

## ----fig.align='center'-------------------------------------------------------
a = seq(10, 40, .1) #sequence for plotting fits
L2 = lm(magnetic ~ chemical + I(chemical^2)) 
plot(chemical, magnetic, main="Quadratic", pch=16) 
yhat2 = L2$coef[1] + L2$coef[2] * a + L2$coef[3] * a^2 
lines(a, yhat2, lwd=2)

## -----------------------------------------------------------------------------
detach(ironslag)
detach(package:DAAG)
rm(list = ls())

## -----------------------------------------------------------------------------
# data generation 1
library(MASS)
data_generate_1 = function(mu=c(1,2),sigma=matrix(c(1,0.6,0.6,1),2,2),n=50){
  data = mvrnorm(n,mu,sigma)
  data
}

## -----------------------------------------------------------------------------
# data generation 2
data_generate_2 = function(n){
  Y1 = rexp(n)
  Y2 = 0.6*Y1+1
  data = data.frame(Y1,Y2)
  data
}

## -----------------------------------------------------------------------------
# permutation test function
set.seed(22015)
permutation_test = function(R = 1000,X1,X2){
  n = length(X1)
  z= c(X1,X2)
  reps = numeric(R)
  s0 = cor.test(X1,X2,method = "spearman")$estimate
  for (i in 1:R){
  k = sample(1:(2*n),size = n,replace = FALSE)
  x1 = z[k]
  x2 = z[-k]
  reps[i] = cor.test(x1,x2,method = "spearman")$estimate
}
  p = mean(abs(c(s0,reps)) >= abs(s0))
  p
}

## -----------------------------------------------------------------------------
#result report
result_report = function(data,type){
  X1 = data[,1]
  X2 = data[,2]
  p1_permuation = permutation_test(R=1000,X1,X2)
  p1_spearman = cor.test(X1,X2,method = "spearman")$p.value
  list("type" = type, "p1_permuation" = p1_permuation,"p1_spearman"=p1_spearman)
}

## -----------------------------------------------------------------------------
data1 = data_generate_1(mu=c(1,2),sigma=matrix(c(1,0.6,0.6,1),2,2),n=50)
data2 = data_generate_2(n=50)
result_report(data1,"normal")
result_report(data2,"non-normal")

## -----------------------------------------------------------------------------
rm(list = ls())
detach(package:MASS)

## -----------------------------------------------------------------------------
set.seed(22015)
rw.Metropolis = function(sigma, x0, N) {
# sigma:  standard variance of proposal distribution N(xt,sigma)
# x0: initial value
# N: size of random numbers required.
    x = numeric(N)
    x[1] = x0
    u = runif(N)
    k = 0
    for (i in 2:N) {
        y = rnorm(1, x[i-1], sigma)
        if (u[i] <= exp(abs(x[i-1]) - abs(y)) )
            x[i] = y  
        else {
              x[i] = x[i-1]
              k = k + 1
            }
        }
      return(list(x=x, k=k))
}

n = 6000 # N: size of random numbers required.
sigma = c(.5, 1, 2,4,6,8)

x0 = 0
rw1 = rw.Metropolis(sigma[1], x0, n)
rw2 = rw.Metropolis(sigma[2], x0, n)
rw3 = rw.Metropolis(sigma[3], x0, n)
rw4 = rw.Metropolis(sigma[4], x0, n)
rw5 = rw.Metropolis(sigma[5], x0, n)
rw6 = rw.Metropolis(sigma[6], x0, n)
#number of candidate points rejected
print(c(rw1$k, rw2$k, rw3$k, rw4$k,rw5$k,rw6$k)/n)

## ----fig.align='center'-------------------------------------------------------
# trace plot
b = 1000
plot(rw1$x[(b+1):n],type = "l",ylab = "chain",col=1,ylim = c(-10,10))
lines(rw2$x[(b+1):n],type = "l",  ylab = "chain",col=2)
lines(rw3$x[(b+1):n],type = "l",  ylab = "chain",col=3)
lines(rw4$x[(b+1):n],type = "l",  ylab = "chain",col=4)
lines(rw5$x[(b+1):n],type = "l",  ylab = "chain",col=5)
lines(rw6$x[(b+1):n],type = "l",  ylab = "chain",col=6)
legend("bottomright", legend = sigma,lty=1,col = 1:6)

## -----------------------------------------------------------------------------
Gelman.Rubin = function(psi) {
      # psi[i,j] is the statistic psi(X[i,1:j])
      # for chain in i-th row of X
      psi = as.matrix(psi)
      n = ncol(psi)
      k = nrow(psi)

      psi.means = rowMeans(psi)     #row means
      B = n * var(psi.means)        #between variance est.
      psi.w = apply(psi, 1, "var")  #within variances
      W = mean(psi.w)               #within est.
      v.hat = W*(n-1)/n +B/n     #upper variance est.
      r.hat = v.hat / W             #G-R statistic
      return(r.hat)
}

## -----------------------------------------------------------------------------
x0 = c(-10,-5,5,10)
rw41 = rw.Metropolis(sigma[2], x0[1], n)
rw42 = rw.Metropolis(sigma[2], x0[2], n)
rw43 = rw.Metropolis(sigma[2], x0[3], n)
rw44 = rw.Metropolis(sigma[2], x0[4], n)
#compute diagnostic statistics
X = rbind(rw41$x,rw42$x,rw43$x,rw44$x)
psi = t(apply(X, 1, cumsum))
for (i in 1:nrow(psi))
    psi[i,] = psi[i,] / (1:ncol(psi))

#plot the sequence of R-hat statistics
rhat = rep(0, n)
for (j in (b+1):n)
    rhat[j] = Gelman.Rubin(psi[,1:j])
plot(rhat[(b+1):n], type="l", xlab="", ylab="R",ylim=c(1,2.5))
abline(h=1.2, lty=2)

## -----------------------------------------------------------------------------
rm(list = ls()[c(ls()!=("Gelman.Rubin"))] )   

## -----------------------------------------------------------------------------
set.seed(22015)

###### generate the chain #####
bivariate_normal_chain = function(mu1,mu2,sigma1,sigma2,rho,N){
  Z = matrix(0, N, 2) #the chain, a bivariate sample
  Z[1, ] = c(mu1, mu2) #initialize
  s1 = sqrt(1-rho^2)*sigma1
  s2 = sqrt(1-rho^2)*sigma2
  for (i in 2:N) {
    y = Z[i-1, 2]
    m1 = 0 + rho * (y - 0) * sigma1/sigma2
    Z[i, 1] = rnorm(1, m1, s1)
    x = Z[i, 1]
    m2 = 0 + rho * (x - 0) * sigma2/sigma1
    Z[i, 2] = rnorm(1, m2, s2)
  }
  return(Z)
}
#initialize constants and parameters
N = 8000 #length of chain
burn = 1000 #burn-in length
rho = 0.9 #correlation
k = 5 # number of initial values
mu1 = c(-15,-10,0,5,10)
mu2 = c(-15,-10,0,5,10)
sigma1 = 1
sigma2 = 1
Z = array(0,dim = c(k,N,2))
for (i in 1:k){
  Z[i,,] = bivariate_normal_chain(mu1[i],mu2[i],sigma1,sigma2,rho,N)
}

## -----------------------------------------------------------------------------
#compute diagnostic statistics
psi = apply(Z, c(1,3), cumsum)
for (i in 1:dim(psi)[2]){
  for (j in 1:dim(psi)[3]){
    psi[,i,j] = psi[,i,j] / (1:dim(psi)[1])
  }
}

## -----------------------------------------------------------------------------
Gelman.Rubin_muti_lemma2 = function(psi){
      n = dim(psi)[1] #n
      k = dim(psi)[2] #chain numbers
      p = dim(psi)[3] #parameter numbers
      psi.w = array(0,c(p,p,k))
      for (i in 1:k){
        psi.w[,,i] = cov(psi[,i,])
      }
      
      W = apply(psi.w, c(1,2), "mean")
      B.1 = cov(apply(psi,c(2,3),"mean" ))
      #v.hat = W*(n-1)/n +B/n     
      
      # solve max singular value
       lamda = eigen(solve(W)%*%B.1)$val
       r.hat = (n-1)/n + (k+1)/k*max(lamda)
      
      return(r.hat)
}

Gelman.Rubin_muti_lemma3 = function(psi){
      n = dim(psi)[1] #n
      k = dim(psi)[2] #chain numbers
      P = dim(psi)[3] #parameter numbers
      R = rep(0,P)
      for (p in 1:P){
      data = t(psi[,,p])
      R[p] = Gelman.Rubin(data)
      }
      return(max(R)**{1/P})
}      


## -----------------------------------------------------------------------------
#plot the sequence of R-hat statistics
rhat = rep(0, N)
for (j in (burn+1):N)
    rhat[j] = Gelman.Rubin_muti_lemma3(psi[1:j,,])
plot(rhat[(burn+1):N], type="l", xlab="", ylab="R")
abline(h=1.2, lty=2)

## ----eval=FALSE---------------------------------------------------------------
#  par(mfrow=c(3,2))
#  plot(Z[1,(burn+1):N,], main="", cex=.5, xlab=bquote(Z[1,(burn+1):N,1]),ylab=bquote(Z[1,(burn+1):N,2]))
#  for (i in 2:k){
#    plot(Z[i,(burn+1):N,], main="", cex=.5, xlab=bquote(Z[i,(burn+1):N,1]),ylab=bquote(Z[i,(burn+1):N,2]))
#  }

## -----------------------------------------------------------------------------
x = c()
y = c()
for (i in 1:k){
  x = c(x,Z[i,(burn+1):N,1])
  y = c(y,Z[i,(burn+1):N,2])
}
linear_function = lm(y~x)

## -----------------------------------------------------------------------------
summary(linear_function)


## -----------------------------------------------------------------------------
plot(linear_function)

## -----------------------------------------------------------------------------
rm(list = ls())

## -----------------------------------------------------------------------------
set.seed(22015)
#generate the data
data_generation = function(alpha,beta,n){
  gamma = 1
  x = seq(n)
  m = alpha*x +rnorm(n)
  y = beta*m +gamma*x+rnorm(n)
  return(list(m=m,y=y,x=x))
}

## -----------------------------------------------------------------------------
library("mediation")
permutation_test = function(situation,R = 200,m,x,y){
  model.m = lm(m~x)
  model.y = lm(y~m+x)
  model =  mediate(model.m, model.y,treat="x", mediator="m",sims=200)
  t0 = model$d0/sd(model$d0.sims)
  if (situation == 1){
    #$\alpha = 0，X change$
      n = length(x)
      reps = numeric(R)
      for (i in 1:R){
         x1 = sample(x,n,replace = FALSE)
        model.m = lm(m~x1)
        model.y = lm(y~m+x1)
        model =  mediate(model.m, model.y,treat="x1", mediator="m",sims=50)
        reps[i] = model$d0/sd(model$d0.sims)
      }
    p = mean(abs(c(t0,reps)) >= abs(t0))
    p
  }
if (situation == 2){
    #$\beta = 0，Y change$
      n = length(y)
      reps = numeric(R)
      for (i in 1:R){
         y1 = sample(y,n,replace = FALSE)
        model.m = lm(m~x)
        model.y = lm(y1~m+x)
        model =  mediate(model.m, model.y,treat="x", mediator="m",sims=50)
        reps[i] = model$d0/sd(model$d0.sims)
      }
    p = mean(abs(c(t0,reps)) >= abs(t0))
    p
  }
if (situation == 3){
    #$\alpha = 0,\beta = 0，M change$
      n = length(m)
      reps = numeric(R)
      for (i in 1:R){
        m1 = sample(m,n,replace = FALSE)
        model.m = lm(m1~x)
        model.y = lm(y~m1+x)
        model =  mediate(model.m, model.y,treat="x", mediator="m1",sims=50)
        reps[i] = model$d0/sd(model$d0.sims)
      }
    p = mean(abs(c(t0,reps)) >= abs(t0))
    p
  }  
return(p)
}

## -----------------------------------------------------------------------------
result_report = function(alpha = 0,beta = 1,n=100){
  data = data_generation(alpha,beta ,n)
  p = rep(0,3)
  for (i in 1:3){
    p[i]=permutation_test(i,R=50,data$m,data$x,data$y)
  } 
  return(p)
}

## -----------------------------------------------------------------------------
n = 100# the number of the data
a0b1 = result_report(0,1,n)
a1b0 = result_report(1,0,n)
a0b0 = result_report(0,0,n)
knitr::kable(data.frame(a0b1,a1b0,a0b0))

## -----------------------------------------------------------------------------
rm(list = ls())

## -----------------------------------------------------------------------------
calculate_alpha = function(N,beta1,beta2,beta3,f0) {
    x1 = rpois(N,lambda = 1)
    x2 = rexp(N,rate = 1)
    x3 = sample(0:1,N,replace = TRUE)
    g = function(alpha){
      tmp = exp(-alpha-beta1*x1-beta2*x2-beta3*x3)
      p = 1/(1+tmp)
      mean(p) - f0
    }
    solution = uniroot(g,c(-100,0))
    print(round(unlist(solution),5)[1:3])
    print(round(unlist(solution),5)[-(1:3)])
    alpha = solution$root
    # add function to check the results
    tmp = exp(-alpha-beta1*x1-beta2*x2-beta3*x3)
    p = 1/(1+tmp)
    d = rbinom(N,1,p)
    cat("check the precalenca:",mean(d)-f0,"\n")
    return(alpha)
}


## -----------------------------------------------------------------------------
set.seed(22015)
N = 1e6
beta1 = 0
beta2 = 1
beta3 = -1
f0_list = c(0.1,0.01,0.001,0.0001)
alpha_list = rep(0,length(f0_list))
counter = 1
for (f0 in f0_list){
  alpha_list[counter] = calculate_alpha(N,beta1,beta2,beta3,f0)
  counter = counter+1
}
alpha_list

## ----fig.align='center'-------------------------------------------------------
plot(-log(f0_list),alpha_list)

## -----------------------------------------------------------------------------
rm(list = ls() )   

## ---- fig.align='center', echo=FALSE, out.width="50%"-------------------------
library(knitr)
knitr::include_graphics("../inst/Q1.png")

## ---- fig.align='center', echo=FALSE, out.width="50%"-------------------------
library(knitr)
knitr::include_graphics("../inst/A1.jpg")
knitr::include_graphics("../inst/A2.jpg")

## -----------------------------------------------------------------------------
# import data
n = 10
data = matrix(data=c(11,12,8,9,27,28,13,14,16,17,0,1,23,24,10,11,24,25,2,3),ncol = 2,nrow = 10,byrow = TRUE)

## ---- warning=FALSE-----------------------------------------------------------
# method1 use stats4
library(stats4)
mlogL = function(lambda=4){
  s = 0
  for (i in 1:n){
    s = s + log(exp(-lambda*data[i,1])-exp(-lambda*data[i,2]))
  }
  return(-s)
}
fit = mle(mlogL)

## -----------------------------------------------------------------------------
print(fit@coef)

## ----eval=FALSE---------------------------------------------------------------
#  set.seed(22015)
#  N = 1e6
#  mlogL_partial = function(lambda){
#    s = 0
#    for (i in 1:n){
#      u = data[i,1]
#      v = data[i,2]
#      s = s + (v*exp(-lambda*v)-u*exp(-lambda*u))/(exp(-lambda*u)-exp(-lambda*v))
#    }
#    return(s)
#  }
#  solution2 = uniroot(mlogL_partial,c(0,10))
#  print(unlist(solution2),5)[-(1:3)]

## -----------------------------------------------------------------------------
# em
get_denominator = function(lambda){
  s = 0
  for (i in 1:n){
    u = data[i,1]
    v = data[i,2]
    d = exp(-lambda*u)-exp(-lambda*v)
    f = exp(-lambda*u)*(u+1/lambda)-exp(-lambda*v)*(v+1/lambda)
    s = s + f/d
  }
  return(s)
}

EM = function(max.it=1000,eps=1e-5){
  lambda = 1
  i = 1
  lambda1 = lambda
  lambda2 = n/get_denominator(lambda1)
  while( abs(lambda1 - lambda2) >= eps){
    lambda1 = lambda2
    lambda2 = n/get_denominator(lambda1)
    print(round(c(lambda2),5))
    if(i == max.it) {
      print("cannot converge") 
      break
    }
    i = i + 1    
    
  }
  return(lambda2)
}
EM(max.it=10000,eps=1e-5)

## -----------------------------------------------------------------------------
rm(list = ls())

## -----------------------------------------------------------------------------
# R  program to illustrate1
# converting list to vector
# Creating a list.
my_list_1 =list(l1 = c(1, 3),                
                l2 = c(1, 2, 3))   
# Apply unlist R function
print(my_list_1)
print(unlist(my_list_1))  
print(as.vector(my_list_1))

## -----------------------------------------------------------------------------
# R program to illustrate2
# Unlisting list with data frame
# Creating a list.
my_list_2 = list(l1 = c(1, 3),                
                l2 = c(1, 2, 3))   
  
# Add a data frame to the list                              
my_list_2[[3]] <- data.frame(x1 = c(1, 2, 3),       
                             x2 = c(4, 5, 6))
  
# Unlist list with data.frame
print(my_list_2)
print(unlist(my_list_2, use.names = FALSE))
print(as.vector(my_list_2))

## -----------------------------------------------------------------------------
c(1 == "1",-1 < FALSE,"one" < 2)

## -----------------------------------------------------------------------------
rm(list = ls() )   

## -----------------------------------------------------------------------------
x = c(1,2,5 )
dim(x)
nrow(x)
ncol(x)
NROW(x)
NCOL(x)

## -----------------------------------------------------------------------------
x = matrix(c(2,0,2,2,1,1),2,3)
is.matrix(x)
is.array(x)

## -----------------------------------------------------------------------------
rm(list = ls() )   

## -----------------------------------------------------------------------------
x = data.frame(a = c(1,2,3),b = c("1","2","3"),c = c(TRUE,FALSE,TRUE))
str(x)
y = as.matrix(x)
str(y)

## -----------------------------------------------------------------------------
# data frame with 0 columns and 0 rows
R1 = data.frame()
R1
# data frame with  0 rows
R2 = data.frame(a=integer(), b=logical(0),c=numeric(0),d=character(0)) 
R2
# data frame with  0 columns
R3 = data.frame(row.names = c("a","b","c"))
R3

## -----------------------------------------------------------------------------
scale01 = function(x) {
         rng = range(x, na.rm = TRUE) #logical, indicating if NA's should be omitted.
         (x - rng[1]) / (rng[2] - rng[1])
}

## -----------------------------------------------------------------------------
# At first, we create a dataframe with numeric columns only
data_numeric = data.frame(matrix(data = 1:10,nrow=5,ncol=2))
# Correct values arrive if we apply the scale01 function directly
data.frame(lapply(data_numeric, function(x) scale01(x)))

## -----------------------------------------------------------------------------
# Then, we create a dataframe with different types of values 
data_types = data.frame(a = c(1,2,3),b = c(TRUE,FALSE,FALSE), c = c("1","2","3"))
# Error will appear if we apply the function above 
# lapply(data, function(x) scale01(x))
# So we can utilize the function if
data.frame(lapply(data_types, function(x) if (is.numeric(x)) scale01(x) else x))

## -----------------------------------------------------------------------------
rm(list = ls())

## -----------------------------------------------------------------------------
deviation = function(data){ 
  # data should be the form of numeric data frame
  # vapply is similar to sapply, but has a pre-specified type of return value, so it can be safer (and sometimes faster) to use.
  a = vapply(data,FUN = function(x) sd(x),numeric(1))
  return(data.frame(a))
}
data_numeric = data.frame(matrix(data = 1:10,nrow=5,ncol=2))
deviation(data_numeric)

## -----------------------------------------------------------------------------
deviation_mix = function(data){ 
  # data is  a mixed data frame
  data = data[vapply(data,FUN = is.numeric,logical(1))]
  a = vapply(data,FUN = function(x) sd(x),numeric(1))
  return(data.frame(a))
}
# Then, we create a dataframe with different types of values 
data_types = data.frame(a = c(1,2,3),b = c(TRUE,FALSE,FALSE), c = c("1","2","3"))
deviation_mix(data_types)

## -----------------------------------------------------------------------------
rm(list = ls() )   

## -----------------------------------------------------------------------------
# library("Rcpp")
# source('gibbsR.R')
# sourceCpp('gibbsC.cpp')
# add
library(StatComp22015)
set.seed(22015)
gibbC = gibbsC(10000,10,1,0,0,1,1,0.9)
gibbR = gibbsR(10000,10,1,0,0,1,1,0.9)

## ----fig.align='center'-------------------------------------------------------
chi_R = mahalanobis(gibbR,colMeans(gibbR),cov(gibbR))
chi_C = mahalanobis(gibbC,colMeans(gibbC),cov(gibbC))
qqplot(chi_R,chi_C,main = expression("Q-Q plot of Mahalanobis" * ~chi_R *
                         " vs" * ~ chi_C))
abline(0, 1, col = 'gray')

## -----------------------------------------------------------------------------
library(microbenchmark)
# Compare the computation time of the two functions with the function “microbenchmark”.
ts = microbenchmark(gibbR = gibbsR(10000,10,1,0,0,1,1,0.9), gibbC = gibbsC(10000,10,1,0,0,1,1,0.9))
summary(ts)[,c(1,3,5,6)]

## -----------------------------------------------------------------------------
rm(list = ls() )   

