
c_to_f <- function(x) {
  (x * 9/5) + 32
}


CDD_FDD <- function(x) {
  (9/5) * x
}



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



del_eF <- (c_to_f(25) - c_to_f(10)) * 7.6
del_crF <- (c_to_f(25) - c_to_f(10)) * 14.29
del2ndAF <- (c_to_f(24.4) - c_to_f(10)) * 8.3
del3rdAF <- (c_to_f(24.4) - c_to_f(10)) * 12.2
del_mAF <-(c_to_f(24.4) - c_to_f(10)) * 1.4
del_fAF <- (c_to_f(24.4) - c_to_f(10)) * 34.6


preOvF <- (c_to_f(25) - c_to_f(10)) * 22.14
repTF <- (c_to_f(25) - c_to_f(10)) * 31.77

eggR <- 83.9 / repTF

keF <- round(((c_to_f(25) - c_to_f(10)) * 7.6)^2 / (((c_to_f(25) - c_to_f(10)) * 0.68) * sqrt(31))^2)
kcrF <- round(((c_to_f(25) - c_to_f(10)) * 14.29)^2 / (((c_to_f(25) - c_to_f(10)) * 0.26) * sqrt(392))^2)
k2ndAF <- round(((c_to_f(24.4) - c_to_f(10)) * 8.3)^2 / (((c_to_f(24.4) - c_to_f(10)) * 1.3) * sqrt(4))^2)
k3rdAF <- round(((c_to_f(24.4) - c_to_f(10)) * 12.2)^2 / (((c_to_f(24.4) - c_to_f(10)) * 2.6) * sqrt(11))^2)
k_mAF <- ceiling(((c_to_f(24.4) - c_to_f(10)) * 1.4)^2 / (((c_to_f(24.4) - c_to_f(10)) * 0.8) * sqrt(7))^2)
k_fAF <- round(((c_to_f(24.4) - c_to_f(10)) * 34.6)^2 / (((c_to_f(24.4) - c_to_f(10)) * 8.4) * sqrt(10))^2)

k_f_repF <- round(((c_to_f(25) - c_to_f(10)) * (54.84 - 22.14))^2 / (((c_to_f(25) - c_to_f(10)) * 1.227) * sqrt(64))^2)


stepy <- 0.0625
longF <- CDD_FDD(3500)
t1 <- seq(0, longF, stepy)


# FIRST GENERATION

eggs = 1000 
craw = 0
n2 = 0
n3 = 0
males = 0
females = 0
eggsII = 0

sta_AF <- matrix(0, length(t1), keF); sta_AF[1, 1] <- eggs
check <- rep(c(0, 1), longF)

for(h in 1: length(t1)) {
  for(t in 1: (length(t1) - 1)) {
    for(i in 1: (keF - 1)) {
      sta_AF[t + 1, 1] <- euler.m(function(x, y){ -((y) * (keF / del_eF))}, h = stepy,
                                 x0 = t1[t], y0 = sta_AF[t, 1], xfinal = t1[t + 1])$Y[2]
      sta_AF[t + 1, i + 1] <- euler.m(function(x, y){(sta_AF[t, i] * (keF / del_eF)) - ((y) * (keF / del_eF))},
                                     h = stepy, x0 = t1[t], y0 = sta_AF[t, i + 1], xfinal = t1[t + 1])$Y[2]
    }
  }
  
  ######
  sta_A1F <- matrix(0, length(t1), kcrF)
  sta_A1F[1, 1] <- craw
  for(t in 1: (length(t1) - 1)) {
    for(i in 1: (kcrF - 1)) {
      sta_A1F[t + 1, 1] <- euler.m(function(x, y){(sta_AF[t, keF] * (keF / del_eF)) - ((y) * (kcrF / del_crF))}, 
                                  h = stepy, x0 = t1[t], y0 = sta_A1F[t, 1], xfinal = t1[t + 1])$Y[2]
      sta_A1F[t + 1, i + 1] <- euler.m(function(x, y){(sta_A1F[t, i] * (kcrF / del_crF)) - ((y) * (kcrF / del_crF))}, 
                                      h = stepy, x0 = t1[t],  y0 = sta_A1F[t, i + 1], xfinal = t1[t + 1])$Y[2]
    }
  }
  #####
  
  sta_A2F <- matrix(0, length(t1), k2ndAF)
  sta_A2F[1, 1] <- n2
  for(t in 1: (length(t1) - 1)) {
    for(i in 1: (k2ndAF - 1)) {
      sta_A2F[t + 1, 1] <- euler.m(function(x, y){(sta_A1F[t, kcrF] * (kcrF / del_crF)) - ((y) * (k2ndAF / del2ndAF))}, 
                                  h = stepy, x0 = t1[t], y0 = sta_A2F[t, 1], xfinal = t1[t + 1])$Y[2]
      sta_A2F[t + 1, i + 1] <- euler.m(function(x, y){(sta_A2F[t, i] * (k2ndAF / del2ndAF)) - ((y) * (k2ndAF / del2ndAF))}, 
                                      h = stepy, x0 = t1[t],  y0 = sta_A2F[t, i + 1], xfinal = t1[t + 1])$Y[2]
    }
  }
  #####
  
  sta_A3F <- matrix(0, length(t1), k3rdAF)
  sta_A3F[1, 1] <- n3
  for(t in 1: (length(t1) - 1)) {
    for(i in 1: (k3rdAF - 1)) {
      sta_A3F[t + 1, 1] <- euler.m(function(x, y){(sta_A2F[t, k2ndAF] * (k2ndAF / del2ndAF)) - ((y) * (k3rdAF / del3rdAF))}, 
                                  h = stepy, x0 = t1[t], y0 = sta_A3F[t, 1], xfinal = t1[t + 1])$Y[2]
      sta_A3F[t + 1, i + 1] <- euler.m(function(x, y){(sta_A3F[t, i] * (k3rdAF / del3rdAF)) - ((y) * (k3rdAF / del3rdAF))}, 
                                      h = stepy, x0 = t1[t],  y0 = sta_A3F[t, i + 1], xfinal = t1[t + 1])$Y[2]
    }
  }
  #####
  
  sta_A4F <- matrix(0, length(t1), k_mAF)
  sta_A4F[1, 1] <- males
  for(t in 1: (length(t1) - 1)) {
    sta_A4F[t + 1, 1] <- euler.m(function(x, y){(sta_A3F[t, k3rdAF] * (0.5) * (k3rdAF / del3rdAF)) - ((y) * (k_mAF / del_mAF))}, 
                                h = stepy, x0 = t1[t], y0 = sta_A4F[t, 1], xfinal = t1[t + 1])$Y[2]
  }
  #####
  
  sta_A4aF <- matrix(0, length(t1), k_fAF)
  sta_A4aF[1, 1] <- females
  for(t in 1: (length(t1) - 1)) {
    for(i in 1: (k_fAF - 1)) {
      sta_A4aF[t + 1, 1] <- euler.m(function(x, y){(sta_A3F[t, k3rdAF] * (0.5) * (k3rdAF / del3rdAF)) - ((y) * (k_fAF / del_fAF))}, 
                                   h = stepy, x0 = t1[t], y0 = sta_A4aF[t, 1], xfinal = t1[t + 1])$Y[2]
      sta_A4aF[t + 1, i + 1] <- euler.m(function(x, y){(sta_A4aF[t, i] * (k_fAF / del_fAF)) - ((y) * (k_fAF / del_fAF))}, 
                                       h = stepy, x0 = t1[t],  y0 = sta_A4aF[t, i + 1], xfinal = t1[t + 1])$Y[2]
    }
  }
  #####
  
  sta_A5F <- matrix(0, length(t1), keF)
  sta_A5F[1, 1] <- eggsII
  for(t in 1: (length(t1) - 1)) {
    for(i in 1: (keF - 1)) {
      sta_A5F[t + 1, 1] <- euler.m(function(x, y){(sum(sta_A4aF[t, ]) * eggR) - ((y) * (keF / del_eF))}, 
                                  h = stepy, x0 = t1[t], y0 = sta_A5F[t, 1], xfinal = t1[t + 1])$Y[2]
      sta_A5F[t + 1, i + 1] <- euler.m(function(x, y){(sta_A5F[t, i] * (keF / del_eF)) - ((y) * (keF / del_eF))}, 
                                      h = stepy, x0 = t1[t],  y0 = sta_A5F[t, i + 1], xfinal = t1[t + 1])$Y[2]
    }
  }
  
  ####
  
  sta_A1AF <- matrix(0, length(t1), kcrF)
  for(t in 1: (length(t1) - 1)) {
    for(i in 1: (kcrF - 1)) {
      sta_A1AF[t + 1, 1] <- euler.m(function(x, y){(sta_A5F[t, keF] * (keF / del_eF)) - ((y) * (kcrF / del_crF))}, 
                                   h = stepy, x0 = t1[t], y0 = sta_A1AF[t, 1], xfinal = t1[t + 1])$Y[2]
      sta_A1AF[t + 1, i + 1] <- euler.m(function(x, y){(sta_A1AF[t, i] * (kcrF / del_crF)) - ((y) * (kcrF / del_crF))}, 
                                       h = stepy, x0 = t1[t],  y0 = sta_A1AF[t, i + 1], xfinal = t1[t + 1])$Y[2]
    }
  }
  
  #####
  
  sta_A2AF <- matrix(0, length(t1), k2ndAF)
  for(t in 1: (length(t1) - 1)) {
    for(i in 1: (k2ndAF - 1)) {
      sta_A2AF[t + 1, 1] <- euler.m(function(x, y){(sta_A1AF[t, kcrF] * (kcrF / del_crF)) - ((y) * (k2ndAF / del2ndAF))}, 
                                   h = stepy, x0 = t1[t], y0 = sta_A2AF[t, 1], xfinal = t1[t + 1])$Y[2]
      sta_A2AF[t + 1, i + 1] <- euler.m(function(x, y){(sta_A2AF[t, i] * (k2ndAF / del2ndAF)) - ((y) * (k2ndAF / del2ndAF))}, 
                                       h = stepy, x0 = t1[t],  y0 = sta_A2AF[t, i + 1], xfinal = t1[t + 1])$Y[2]
    }
  }
  #####
  
  sta_A3AF <- matrix(0, length(t1), k3rdAF)
  for(t in 1: (length(t1) - 1)) {
    for(i in 1: (k3rdAF - 1)) {
      sta_A3AF[t + 1, 1] <- euler.m(function(x, y){(sta_A2AF[t, k2ndAF] * (k2ndAF / del2ndAF)) - ((y) * (k3rdAF / del3rdAF))}, 
                                   h = stepy, x0 = t1[t], y0 = sta_A3AF[t, 1], xfinal = t1[t + 1])$Y[2]
      sta_A3AF[t + 1, i + 1] <- euler.m(function(x, y){(sta_A3AF[t, i] * (k3rdAF / del3rdAF)) - ((y) * (k3rdAF / del3rdAF))}, 
                                       h = stepy, x0 = t1[t],  y0 = sta_A3AF[t, i + 1], xfinal = t1[t + 1])$Y[2]
    }
  }
  #####
  
  sta_A4AF <- matrix(0, length(t1), k_mAF)
  for(t in 1: (length(t1) - 1)) {
    sta_A4AF[t + 1, 1] <- euler.m(function(x, y){(sta_A3AF[t, k3rdAF] * (0.5) * (k3rdAF / del3rdAF)) - ((y) * (k_mAF / del_mAF))}, 
                                 h = stepy, x0 = t1[t], y0 = sta_A4AF[t, 1], xfinal = t1[t + 1])$Y[2]
  }
  #####
  
  sta_A4AaF <- matrix(0, length(t1), k_fAF)
  for(t in 1: (length(t1) - 1)) {
    for(i in 1: (k_fAF - 1)) {
      sta_A4AaF[t + 1, 1] <- euler.m(function(x, y){(sta_A3AF[t, k3rdAF] * (0.5) * (k3rdAF / del3rdAF)) - ((y) * (k_fAF / del_fAF))}, 
                                    h = stepy, x0 = t1[t], y0 = sta_A4AaF[t, 1], xfinal = t1[t + 1])$Y[2]
      sta_A4AaF[t + 1, i + 1] <- euler.m(function(x, y){(sta_A4AaF[t, i] * (k_fAF / del_fAF)) - ((y) * (k_fAF / del_fAF))}, 
                                        h = stepy, x0 = t1[t],  y0 = sta_A4AaF[t, i + 1], xfinal = t1[t + 1])$Y[2]
    }
  }
  #####
  
  sta_A5AF <- matrix(0, length(t1), keF)
  for(t in 1: (length(t1) - 1)) {
    for(i in 1: (keF - 1)) {
      sta_A5AF[t + 1, 1] <- euler.m(function(x, y){(sum(sta_A4AaF[t, ]) * eggR) - ((y) * (keF / del_eF))}, 
                                   h = stepy, x0 = t1[t], y0 = sta_A5AF[t, 1], xfinal = t1[t + 1])$Y[2]
      sta_A5AF[t + 1, i + 1] <- euler.m(function(x, y){(sta_A5AF[t, i] * (keF / del_eF)) - ((y) * (keF / del_eF))}, 
                                       h = stepy, x0 = t1[t],  y0 = sta_A5AF[t, i + 1], xfinal = t1[t + 1])$Y[2]
    }
  }
  
  ####
  
  check[h + 1] <- sta_AF[length(t1),1]
  if(check[h + 1] == check[h]) break
}

save.image(file = "simulation_output_F.RData")
