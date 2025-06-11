

plot(seq(0, long, stepy) - (a + b), rowSums(sta_A1) / 1000, type = "l", ylab = "", xlab = "",
     xlim = c(-310, 1700), yaxt = "n", xaxt = "n", cex.lab = 2, cex.axis = 2, lwd = 2)

axis(2, at = seq(0, 1, 0.2), labels = FALSE)
axis(3, at = c(-(a + b), 0), labels = c("Eggs laid", "Jan 1st"), cex.axis = 1.8)
axis(1, at = c(seq(-500, -100, 200), 0, seq(100, 1700, 200)), cex.axis = 2.2)



craw1_raw <- rowSums(sta_A1)


library(SuppDists)
library(ExtDist)
library(bbmle)


eJohnsonSB(X = crwD$x, w = crwD$y*100)



estimat <- function(DDs, prs, method){
  LL1 <- function(gamma, delta, a, b) {
    -sum(pr * log(dJohnsonSB_ab(x = x, gamma = gamma, delta = delta, a = a, b = b)))
  }
  
  if(method == "L-BFGS-B"){
    MLL <- mle2(LL1, start = list(gamma = -0.5, delta = 2, a = min(DDs) - 1, b = max(DDs) + 1), 
                data = list(x = DDs, pr = prs),
                lower = list(gamma = -Inf, delta = 0, a = -Inf, b = max(DDs)), 
                upper = list(gamma = Inf, delta = Inf, a = min(DDs), b = Inf), method = "L-BFGS-B")
  }
  
  if(method == "Nelder-Mead"){
    MLL <- mle2(LL1, start = list(gamma = -0.5, delta = 2, a = min(DDs) - 1, b = max(DDs) + 1), 
                data = list(x = DDs, pr = prs),
                method = "Nelder-Mead")
  }
  
  MLL
}




MLL2 <- estimat(DDs = crwD$x, prs = crwD$y, method = "Nelder-Mead")

summary(MLL2)



funres2 <- function(DDs){
  xi = coef(MLL2)[3]
  lambda = coef(MLL2)[4] - coef(MLL2)[3]
  dJohnsonSB(DDs, gamma = coef(MLL2)[1],
             delta = coef(MLL2)[2],
             xi = xi,
             lambda = lambda)
}

funres2a <- function(DDs){
  xi = coef(MLL2)[3]
  lambda = coef(MLL2)[4] - coef(MLL2)[3]
  pJohnsonSB(DDs, gamma = coef(MLL2)[1],
             delta = coef(MLL2)[2],
             xi = xi,
             lambda = lambda)
}

plot(DDs, prop_cr1*15, type = "l", xlim = c(-330, 500))
lines(DDs, funres2(DDs))
lines(DDs[1:10000], dJohnsonSB(
  DDs[1:10000],
  gamma = mod1$par[1],
  delta = mod1$par[2],
  xi = mod1$par[3],
  lambda = mod1$par[4]), col = "red")

plot(DDs, cumsum(prop_cr1), type = "l", xlim = c(-330, 500))
lines(DDs, funres2a(DDs))




DDs <- seq(0, long, stepy) - (a + b)


plot(DDs, craw1_raw / sum(craw1_raw))

prop_cr1 <- craw1_raw / sum(craw1_raw)

dJohnsonSB(
  DDs,
  gamma = -0.5,
  delta = 2,
  xi = -0.5,
  lambda = 2)



crwD <- data.frame(x = DDs[1:10000], y = prop_cr1[1:10000])


minf <- function(data, pars1) {
  
  abs(sum((pJohnsonSB(data$x, 
                      gamma = pars1[1], 
                      delta = pars1[2], 
                      xi = pars1[3], 
                      lambda = pars1[4]) - (cumsum(data$y)))))
  
  
}



mod1 <- optim(par = c(gamma = 0.756, delta = 1.30, xi = -330.3, lambda = 628), data = crwD, minf, method = "Nelder-Mead")


plot(DDs, cumsum(prop_cr1), type = "l")
lines(DDs, pJohnsonSB(
  DDs,
  gamma = mod1$par[1],
  delta = mod1$par[2],
  xi = mod1$par[3],
  lambda = mod1$par[4]), col = "red")


plot(DDs[1:10000], prop_cr1[1:10000] * 15, type = "l")
lines(DDs[1:10000], dJohnsonSB(
  DDs[1:10000],
  gamma = mod1$par[1],
  delta = mod1$par[2],
  xi = mod1$par[3],
  lambda = mod1$par[4]), col = "red")



plot(DDs, prop_cr1*10, type = "l")
lines(DDs, dJohnsonSB(
  DDs,
  gamma = 1.07,
  delta = 1.23,
  xi = -310,
  lambda = 577), col = "red")



plot(DDs, dJohnsonSB(
  DDs,
  gamma = 1.07,
  delta = 1.23,
  xi = 69,
  lambda = 577), col = "red")



dJohnsonSB(
  578,
  gamma = 1.07,
  delta = 1.23,
  xi = 69,
  lambda = 577)






t <- seq(-4, 2, 0.01)
res <- rep(NA, length(t))


for(i in 1:length(t)) {
  res[i] <- abs(sum(dJohnsonSB(
    DDs,
    gamma = t[i],
    delta = 1.23,
    xi = -310,
    lambda = 600) - prop_cr1))
}


plot(t, res)





