library(mvtnorm)
library(MASS)
library(shape)
library(ggplot2)
MH = function(N, sigma) {
  mu = c(0,0)
  covar = matrix(c(1,0.6,0.6,2), nrow=2)
  x = matrix(0, nrow = N, ncol = 2)
  #inv_sigma = solve(sigma)
  #x[1,] = rnorm(2, mean=0, sd=1)
  x[1,] = c(4,8)
  for (i in 2:N){
    x_t = x[i-1, ]
    x_new = mvrnorm(mu=x[i-1, ], Sigma=sigma)
    A = dmvnorm(x_new,sigma=covar) / dmvnorm(x_t, sigma=covar)
    u=rnorm(1)
    if (min(A,1) >= u) {
      x[i, ] = x_new
    } else {
      x[i, ] = x[i-1, ]  
    }
  }
  return (x)
}
mh_sample = MH(N=40, diag(2))
iters=40
plot(mh_sample, cex=0.7, xlab = "x1",ylab="x2")
for (k in 1:(iters-1)) {
  Arrows(mh_sample[k,1],mh_sample[k,2], mh_sample[k+1,1],mh_sample[k+1,2],
         lty=1,lwd=0.6, col='blue', arr.adj=1,code=2,arr.type='simple',arr.width=0.1)
}
for(i in seq(0.02,2,0.1)){
  mixtools::ellipse(mu=c(0,0),
                    sigma = sigma, alpha = i,npoints = 500,
                    draw = TRUE,newplot = FALSE, col="red",lwd=1) 
}
# simu = as.data.frame(mh_sample)
# sp = ggplot(data=simu, aes(x=V1,y=V2)) + geom_point(color = "gray") + geom_density_2d()
# sp
