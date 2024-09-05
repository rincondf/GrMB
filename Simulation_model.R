library(MASS)

euler.m <- function(f, h = 1e-7, x0, y0, xfinal) {
  N = (xfinal - x0) / h
  x = y = numeric(N + 1)
  x[1] = x0; y[1] = y0
  i = 1
  while (i <= N) {
    x[i + 1] = x[i] + h
    y[i + 1] = y[i] + h * f(x[i], y[i])
    i = i + 1
  }
  return(data.frame(X = x, Y = y))
}



del_e <- (25 - 10) * 7.6
del_cr <- (25 - 10) * 14.29
del2ndA <- (24.4 - 10) * 8.3
del3rdA <- (24.4 - 10) * 12.2
del_mA <-(24.4 - 10) * 1.4
del_fA <- (24.4 - 10) * 34.6


preOv <- (25 - 10) * 22.14
repT <- (25 - 10) * 31.77

eggR <- 83.9 / repT

ke <- round(((25 - 10) * 7.6)^2 / (((25 - 10) * 0.68) * sqrt(31))^2)
kcr <- round(((25 - 10) * 14.29)^2 / (((25 - 10) * 0.26) * sqrt(392))^2)
k2ndA <- round(((24.4 - 10) * 8.3)^2 / (((24.4 - 10) * 1.3) * sqrt(4))^2)
k3rdA <- round(((24.4 - 10) * 12.2)^2 / (((24.4 - 10) * 2.6) * sqrt(11))^2)
k_mA <- ceiling(((24.4 - 10) * 1.4)^2 / (((24.4 - 10) * 0.8) * sqrt(7))^2)
k_fA <- round(((24.4 - 10) * 34.6)^2 / (((24.4 - 10) * 8.4) * sqrt(10))^2)

k_f_rep <- round(((25 - 10) * (54.84 - 22.14))^2 / (((25 - 10) * 1.227) * sqrt(64))^2)


stepy <- 0.0625
long <- 2500
t1 <- seq(0, long, stepy)


# FIRST GENERATION

eggs = 1000 
craw = 0
n2 = 0
n3 = 0
males = 0
females = 0
eggsII = 0

sta_A <- matrix(0, length(t1), ke); sta_A[1, 1] <- eggs
check <- rep(c(0, 1), long)

for(h in 1: length(t1)) {
  for(t in 1: (length(t1) - 1)) {
    for(i in 1: (ke - 1)) {
      sta_A[t + 1, 1] <- euler.m(function(x, y){ -((y) * (ke / del_e))}, h = stepy,
                                 x0 = t1[t], y0 = sta_A[t, 1], xfinal = t1[t + 1])$Y[2]
      sta_A[t + 1, i + 1] <- euler.m(function(x, y){(sta_A[t, i] * (ke / del_e)) - ((y) * (ke / del_e))},
                                     h = stepy, x0 = t1[t], y0 = sta_A[t, i + 1], xfinal = t1[t + 1])$Y[2]
    }
  }
  
  ######
  sta_A1 <- matrix(0, length(t1), kcr)
  sta_A1[1, 1] <- craw
  for(t in 1: (length(t1) - 1)) {
    for(i in 1: (kcr - 1)) {
      sta_A1[t + 1, 1] <- euler.m(function(x, y){(sta_A[t, ke] * (ke / del_e)) - ((y) * (kcr / del_cr))}, 
                                  h = stepy, x0 = t1[t], y0 = sta_A1[t, 1], xfinal = t1[t + 1])$Y[2]
      sta_A1[t + 1, i + 1] <- euler.m(function(x, y){(sta_A1[t, i] * (kcr / del_cr)) - ((y) * (kcr / del_cr))}, 
                                      h = stepy, x0 = t1[t],  y0 = sta_A1[t, i + 1], xfinal = t1[t + 1])$Y[2]
    }
  }
  #####
  
  sta_A2 <- matrix(0, length(t1), k2ndA)
  sta_A2[1, 1] <- n2
  for(t in 1: (length(t1) - 1)) {
    for(i in 1: (k2ndA - 1)) {
      sta_A2[t + 1, 1] <- euler.m(function(x, y){(sta_A1[t, kcr] * (kcr / del_cr)) - ((y) * (k2ndA / del2ndA))}, 
                                  h = stepy, x0 = t1[t], y0 = sta_A2[t, 1], xfinal = t1[t + 1])$Y[2]
      sta_A2[t + 1, i + 1] <- euler.m(function(x, y){(sta_A2[t, i] * (k2ndA / del2ndA)) - ((y) * (k2ndA / del2ndA))}, 
                                      h = stepy, x0 = t1[t],  y0 = sta_A2[t, i + 1], xfinal = t1[t + 1])$Y[2]
    }
  }
  #####
  
  sta_A3 <- matrix(0, length(t1), k3rdA)
  sta_A3[1, 1] <- n3
  for(t in 1: (length(t1) - 1)) {
    for(i in 1: (k3rdA - 1)) {
      sta_A3[t + 1, 1] <- euler.m(function(x, y){(sta_A2[t, k2ndA] * (k2ndA / del2ndA)) - ((y) * (k3rdA / del3rdA))}, 
                                  h = stepy, x0 = t1[t], y0 = sta_A3[t, 1], xfinal = t1[t + 1])$Y[2]
      sta_A3[t + 1, i + 1] <- euler.m(function(x, y){(sta_A3[t, i] * (k3rdA / del3rdA)) - ((y) * (k3rdA / del3rdA))}, 
                                      h = stepy, x0 = t1[t],  y0 = sta_A3[t, i + 1], xfinal = t1[t + 1])$Y[2]
    }
  }
  #####
  
  sta_A4 <- matrix(0, length(t1), k_mA)
  sta_A4[1, 1] <- males
  for(t in 1: (length(t1) - 1)) {
    sta_A4[t + 1, 1] <- euler.m(function(x, y){(sta_A3[t, k3rdA] * (0.5) * (k3rdA / del3rdA)) - ((y) * (k_mA / del_mA))}, 
                                h = stepy, x0 = t1[t], y0 = sta_A4[t, 1], xfinal = t1[t + 1])$Y[2]
  }
  #####
  
  sta_A4a <- matrix(0, length(t1), k_fA)
  sta_A4a[1, 1] <- females
  for(t in 1: (length(t1) - 1)) {
    for(i in 1: (k_fA - 1)) {
      sta_A4a[t + 1, 1] <- euler.m(function(x, y){(sta_A3[t, k3rdA] * (0.5) * (k3rdA / del3rdA)) - ((y) * (k_fA / del_fA))}, 
                                   h = stepy, x0 = t1[t], y0 = sta_A4a[t, 1], xfinal = t1[t + 1])$Y[2]
      sta_A4a[t + 1, i + 1] <- euler.m(function(x, y){(sta_A4a[t, i] * (k_fA / del_fA)) - ((y) * (k_fA / del_fA))}, 
                                       h = stepy, x0 = t1[t],  y0 = sta_A4a[t, i + 1], xfinal = t1[t + 1])$Y[2]
    }
  }
  #####
  
  sta_A5 <- matrix(0, length(t1), ke)
  sta_A5[1, 1] <- eggsII
  for(t in 1: (length(t1) - 1)) {
    for(i in 1: (ke - 1)) {
      sta_A5[t + 1, 1] <- euler.m(function(x, y){(sum(sta_A4a[t, ]) * eggR) - ((y) * (ke / del_e))}, 
                                  h = stepy, x0 = t1[t], y0 = sta_A5[t, 1], xfinal = t1[t + 1])$Y[2]
      sta_A5[t + 1, i + 1] <- euler.m(function(x, y){(sta_A5[t, i] * (ke / del_e)) - ((y) * (ke / del_e))}, 
                                      h = stepy, x0 = t1[t],  y0 = sta_A5[t, i + 1], xfinal = t1[t + 1])$Y[2]
    }
  }
  
  ####
  
  sta_A1A <- matrix(0, length(t1), kcr)
  for(t in 1: (length(t1) - 1)) {
    for(i in 1: (kcr - 1)) {
      sta_A1A[t + 1, 1] <- euler.m(function(x, y){(sta_A5[t, ke] * (ke / del_e)) - ((y) * (kcr / del_cr))}, 
                                   h = stepy, x0 = t1[t], y0 = sta_A1A[t, 1], xfinal = t1[t + 1])$Y[2]
      sta_A1A[t + 1, i + 1] <- euler.m(function(x, y){(sta_A1A[t, i] * (kcr / del_cr)) - ((y) * (kcr / del_cr))}, 
                                       h = stepy, x0 = t1[t],  y0 = sta_A1A[t, i + 1], xfinal = t1[t + 1])$Y[2]
    }
  }
  
  #####
  
  sta_A2A <- matrix(0, length(t1), k2ndA)
  for(t in 1: (length(t1) - 1)) {
    for(i in 1: (k2ndA - 1)) {
      sta_A2A[t + 1, 1] <- euler.m(function(x, y){(sta_A1A[t, kcr] * (kcr / del_cr)) - ((y) * (k2ndA / del2ndA))}, 
                                   h = stepy, x0 = t1[t], y0 = sta_A2A[t, 1], xfinal = t1[t + 1])$Y[2]
      sta_A2A[t + 1, i + 1] <- euler.m(function(x, y){(sta_A2A[t, i] * (k2ndA / del2ndA)) - ((y) * (k2ndA / del2ndA))}, 
                                       h = stepy, x0 = t1[t],  y0 = sta_A2A[t, i + 1], xfinal = t1[t + 1])$Y[2]
    }
  }
  #####
  
  sta_A3A <- matrix(0, length(t1), k3rdA)
  for(t in 1: (length(t1) - 1)) {
    for(i in 1: (k3rdA - 1)) {
      sta_A3A[t + 1, 1] <- euler.m(function(x, y){(sta_A2A[t, k2ndA] * (k2ndA / del2ndA)) - ((y) * (k3rdA / del3rdA))}, 
                                   h = stepy, x0 = t1[t], y0 = sta_A3A[t, 1], xfinal = t1[t + 1])$Y[2]
      sta_A3A[t + 1, i + 1] <- euler.m(function(x, y){(sta_A3A[t, i] * (k3rdA / del3rdA)) - ((y) * (k3rdA / del3rdA))}, 
                                       h = stepy, x0 = t1[t],  y0 = sta_A3A[t, i + 1], xfinal = t1[t + 1])$Y[2]
    }
  }
  #####
  
  sta_A4A <- matrix(0, length(t1), k_mA)
  for(t in 1: (length(t1) - 1)) {
    sta_A4A[t + 1, 1] <- euler.m(function(x, y){(sta_A3A[t, k3rdA] * (0.5) * (k3rdA / del3rdA)) - ((y) * (k_mA / del_mA))}, 
                                 h = stepy, x0 = t1[t], y0 = sta_A4A[t, 1], xfinal = t1[t + 1])$Y[2]
  }
  #####
  
  sta_A4Aa <- matrix(0, length(t1), k_fA)
  for(t in 1: (length(t1) - 1)) {
    for(i in 1: (k_fA - 1)) {
      sta_A4Aa[t + 1, 1] <- euler.m(function(x, y){(sta_A3A[t, k3rdA] * (0.5) * (k3rdA / del3rdA)) - ((y) * (k_fA / del_fA))}, 
                                    h = stepy, x0 = t1[t], y0 = sta_A4Aa[t, 1], xfinal = t1[t + 1])$Y[2]
      sta_A4Aa[t + 1, i + 1] <- euler.m(function(x, y){(sta_A4Aa[t, i] * (k_fA / del_fA)) - ((y) * (k_fA / del_fA))}, 
                                        h = stepy, x0 = t1[t],  y0 = sta_A4Aa[t, i + 1], xfinal = t1[t + 1])$Y[2]
    }
  }
  #####
  
  sta_A5A <- matrix(0, length(t1), ke)
  for(t in 1: (length(t1) - 1)) {
    for(i in 1: (ke - 1)) {
      sta_A5A[t + 1, 1] <- euler.m(function(x, y){(sum(sta_A4Aa[t, ]) * eggR) - ((y) * (ke / del_e))}, 
                                   h = stepy, x0 = t1[t], y0 = sta_A5A[t, 1], xfinal = t1[t + 1])$Y[2]
      sta_A5A[t + 1, i + 1] <- euler.m(function(x, y){(sta_A5A[t, i] * (ke / del_e)) - ((y) * (ke / del_e))}, 
                                       h = stepy, x0 = t1[t],  y0 = sta_A5A[t, i + 1], xfinal = t1[t + 1])$Y[2]
    }
  }
  
  ####
  
  check[h + 1] <- sta_A[length(t1),1]
  if(check[h + 1] == check[h]) break
}

save.image(file = "simulation_output.RData")