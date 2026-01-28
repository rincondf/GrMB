likfun.nbin <- function(p, dd = datasetF$DD2, CR = datasetF$MBperWeek) { 
  
  sh1 <- p[1]
  sc1 <- p[2]
  sh2 <- p[3]
  sc2 <- p[4]

  dens1 <- p[5]
  dens2 <- p[6]
  
  size1 <- p[7]
  
  
  z1 <- dens1 * dgamma(dd, shape = sh1, scale = sc1, log = F)
  z2 <- dens2 * dgamma(dd, shape = sh2, scale = sc2, log = F)
  
  y <- z1 + z2
  
  NLL <- -sum(dnbinom(CR, mu = y, size = size1, log = TRUE))
  return(NLL)
} 


MLE <- optim(par=c(43, 12, 35, 55, 100, 100, 10), fn=likfun.nbin)
MLE

lines(seq(0, 3500), dgamma(seq(0, 3500), shape = MLE$par[1], scale = MLE$par[2]) + 
        dgamma(seq(0, 3500), shape = MLE$par[3], scale = MLE$par[4]), lwd = 2, col = "red")

likfun.nbin2 <- function(p, dd = datasetF$DD2, CR = datasetF$MBperWeek) { 
  
  sh1 <- p[1]
  sc1 <- p[2]
  sh2 <- p[3]
  sc2 <- p[4]
  
  size1 <- p[5]
  
  z1 <- dgamma(dd, shape = sh1, scale = sc1, log = F)
  z2 <- dgamma(dd, shape = sh2, scale = sc2, log = F)
  
  y <- z1 + z2
  
  NLL <- -sum(dnbinom(CR, mu = y, size = size1, log = TRUE))
  return(NLL)
} 


MLE2 <- optim(par=c(43, 12, 35, 55, 10), fn=likfun.nbin2)
MLE2

lines(seq(0, 3500), dgamma(seq(0, 3500), shape = MLE2$par[1], scale = MLE2$par[2]) + 
        dgamma(seq(0, 3500), shape = MLE2$par[3], scale = MLE2$par[4]), lwd = 2, col = "brown")