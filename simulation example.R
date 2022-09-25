sim.network2 = function(seed, n, n0, m, K){
  p = m
  num_invalid = K
  set.seed(seed)
  MAF = 0.3 
  
  gamma1_ZX = runif(20,0.5,1)
  zero1 = sample(1:20, 1, replace = F)
  gamma1_ZX[zero1] = 0
  
  gamma2_ZX = runif(20,0.5,1)
  zero2 = sample(1:20, 3, replace = F)
  gamma2_ZX[zero2] = 0
  
  gamma3_ZX = runif(20,-1,-0.5)
  
  gamma4_ZX = runif(20,-1,-0.5)
  zero4 = sample(1:20, 7, replace = F)
  gamma4_ZX[zero4] = 0
  
  gamma5_ZX = runif(20,0.5,1)
  
  gamma6_ZX = runif(20,-1,-0.5)
  zero6 = sample(1:20, 2, replace = F)
  gamma6_ZX[zero6] = 0
  
  gamma7_ZX = runif(20,0.5,1)
  zero7 = sample(1:20, 1, replace = F)
  gamma7_ZX[zero7] = 0
  
  gamma8_ZX = runif(20,0.5,1)
  zero8 = sample(1:20, 10, replace = F)
  gamma8_ZX[zero8] = 0
  
  gamma9_ZX = runif(20,-1,-0.5)
  zero9 = sample(1:20, 6, replace = F)
  gamma9_ZX[zero9] = 0
  
  gamma10_ZX = runif(20,-1,-0.5)
  
  gamma11_ZX = runif(20,0.5,1)
  zero11 = sample(1:20, 6, replace = F)
  gamma11_ZX[zero11] = 0
  
  gamma12_ZX = runif(20,0.5,1)
  zero12 = sample(1:20, 3, replace = F)
  gamma12_ZX[zero12] = 0
  
  gamma13_ZX = runif(20,0.5,1)
  zero13 = sample(1:20, 10, replace = F)
  gamma13_ZX[zero13] = 0
  
  gamma14_ZX = runif(20,0.5,1)
  
  gamma15_ZX = runif(20,0.5,1)
  zero15 = sample(1:20, 5, replace = F)
  gamma15_ZX[zero15] = 0
  
  gamma16_ZX = runif(20,0.5,1)
  zero16 = sample(1:20, 2, replace = F)
  gamma16_ZX[zero16] = 0
  
  gamma17_ZX = runif(20,-1,-0.5)
  zero17 = sample(1:20, 2, replace = F)
  gamma17_ZX[zero17] = 0
  
  gamma18_ZX = runif(20,-1,-0.5)
  
  gamma19_ZX = runif(20,-1,-0.5)
  zero19 = sample(1:20, 8, replace = F)
  gamma19_ZX[zero19] = 0
  
  gamma20_ZX = runif(20,-1,-0.5)
  
  
  gamma_ZX = cbind(gamma1_ZX, gamma2_ZX, gamma3_ZX, gamma4_ZX, 
                   gamma5_ZX, gamma6_ZX, gamma7_ZX, gamma8_ZX,
                   gamma9_ZX, gamma10_ZX, gamma11_ZX, gamma12_ZX, 
                   gamma13_ZX, gamma14_ZX, 
                   gamma15_ZX, gamma16_ZX, gamma17_ZX, gamma18_ZX,
                   gamma19_ZX, gamma20_ZX)
  
  
  alpha1_ZY = NULL
  i = 0
  while (i<num_invalid) {
    g = rnorm(1,0,0.2)
    if(abs(g)>0 && abs(g)<1)
    {
      alpha1_ZY = c(alpha1_ZY,g)
      i = i + 1
    }
  }
  
  alpha2_ZY = NULL
  i = 0
  while (i<num_invalid) {
    g = rnorm(1,0,0.2)
    if(abs(g)>0 && abs(g)<1)
    {
      alpha2_ZY = c(alpha2_ZY,g)
      i = i + 1
    }
  }
  
  alpha3_ZY = NULL
  i = 0
  while (i<num_invalid) {
    gg = rnorm(1,0,0.2)
    if(abs(g)>0 && abs(g)<1)
    {
      alpha3_ZY = c(alpha3_ZY,g)
      i = i + 1
    }
  }
  
  alpha4_ZY = NULL
  i = 0
  while (i<num_invalid) {
    g = rnorm(1,0,0.2)
    if(abs(g)>0 && abs(g)<1)
    {
      alpha4_ZY = c(alpha4_ZY,g)
      i = i + 1
    }
  }
  
  alpha5_ZY = NULL
  i = 0
  while (i<num_invalid) {
    g = rnorm(1,0,0.2)
    if(abs(g)>0 && abs(g)<1)
    {
      alpha5_ZY = c(alpha5_ZY,g)
      i = i + 1
    }
  }
  
  alpha6_ZY = NULL
  i = 0
  while (i<num_invalid) {
    g = rnorm(1,0,0.2)
    if(abs(g)>0 && abs(g)<1)
    {
      alpha6_ZY = c(alpha6_ZY,g)
      i = i + 1
    }
  }
  
  alpha7_ZY = NULL
  i = 0
  while (i<num_invalid) {
    g = rnorm(1,0,0.2)
    if(abs(g)>0 && abs(g)<1)
    {
      alpha7_ZY = c(alpha7_ZY,g)
      i = i + 1
    }
  }
  
  alpha8_ZY = NULL
  i = 0
  while (i<num_invalid) {
    g = rnorm(1,0,0.2)
    if(abs(g)>0 && abs(g)<1)
    {
      alpha8_ZY = c(alpha8_ZY,g)
      i = i + 1
    }
  }
  
  alpha9_ZY = NULL
  i = 0
  while (i<num_invalid) {
    g = rnorm(1,0,0.2)
    if(abs(g)>0 && abs(g)<1)
    {
      alpha9_ZY = c(alpha9_ZY,g)
      i = i + 1
    }
  }
  
  alpha10_ZY = NULL
  i = 0
  while (i<num_invalid) {
    g = rnorm(1,0,0.2)
    if(abs(g)>0 && abs(g)<1)
    {
      alpha10_ZY = c(alpha10_ZY,g)
      i = i + 1
    }
  }
  
  alpha11_ZY = NULL
  i = 0
  while (i<num_invalid) {
    g = rnorm(1,0,0.2)
    if(abs(g)>0 && abs(g)<1)
    {
      alpha11_ZY = c(alpha11_ZY,g)
      i = i + 1
    }
  }
  
  alpha12_ZY = NULL
  i = 0
  while (i<num_invalid) {
    g = rnorm(1,0,0.2)
    if(abs(g)>0 && abs(g)<1)
    {
      alpha12_ZY = c(alpha12_ZY,g)
      i = i + 1
    }
  }
  
  alpha13_ZY = NULL
  i = 0
  while (i<num_invalid) {
    g = rnorm(1,0,0.2)
    if(abs(g)>0 && abs(g)<1)
    {
      alpha13_ZY = c(alpha13_ZY,g)
      i = i + 1
    }
  }
  
  alpha14_ZY = NULL
  i = 0
  while (i<num_invalid) {
    g = rnorm(1,0,0.2)
    if(abs(g)>0 && abs(g)<1)
    {
      alpha14_ZY = c(alpha14_ZY,g)
      i = i + 1
    }
  }
  
  alpha15_ZY = NULL
  i = 0
  while (i<num_invalid) {
    g = rnorm(1,0,0.2)
    if(abs(g)>0 && abs(g)<1)
    {
      alpha15_ZY = c(alpha15_ZY,g)
      i = i + 1
    }
  }
  
  alpha16_ZY = NULL
  i = 0
  while (i<num_invalid) {
    g = rnorm(1,0,0.2)
    if(abs(g)>0 && abs(g)<1)
    {
      alpha16_ZY = c(alpha16_ZY,g)
      i = i + 1
    }
  }
  
  alpha17_ZY = NULL
  i = 0
  while (i<num_invalid) {
    g = rnorm(1,0,0.2)
    if(abs(g)>0 && abs(g)<1)
    {
      alpha17_ZY = c(alpha17_ZY,g)
      i = i + 1
    }
  }
  
  alpha18_ZY = NULL
  i = 0
  while (i<num_invalid) {
    g = rnorm(1,0,0.2)
    if(abs(g)>0 && abs(g)<1)
    {
      alpha18_ZY = c(alpha18_ZY,g)
      i = i + 1
    }
  }
  
  alpha19_ZY = NULL
  i = 0
  while (i<num_invalid) {
    g = rnorm(1,0,0.2)
    if(abs(g)>0 && abs(g)<1)
    {
      alpha19_ZY = c(alpha19_ZY,g)
      i = i + 1
    }
  }
  
  alpha20_ZY = NULL
  i = 0
  while (i<num_invalid) {
    g = rnorm(1,0,0.2)
    if(abs(g)>0 && abs(g)<1)
    {
      alpha20_ZY = c(alpha20_ZY,g)
      i = i + 1
    }
  }
  
  alpha_ZY = cbind(alpha1_ZY, alpha2_ZY, alpha3_ZY, alpha4_ZY, 
                   alpha5_ZY, alpha6_ZY, alpha7_ZY, alpha8_ZY,
                   alpha9_ZY, alpha10_ZY, alpha11_ZY, 
                   alpha12_ZY, alpha13_ZY, alpha14_ZY, 
                   alpha15_ZY, alpha16_ZY, alpha17_ZY, alpha18_ZY,
                   alpha19_ZY, alpha20_ZY)
  
  e = mvrnorm(n = n, mu = c(0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0), Sigma = diag(20))
  
  Z0 = matrix(rbinom(n0*p,2,MAF),nrow = n0)
  
  # generate first sample
  Z1 = matrix(rbinom(n*p,2,MAF),nrow = n)
  Y1 = -Z1[,1:num_invalid]%*%alpha_ZY%*%ginv(gamma_ZX) +  e%*%ginv(gamma_ZX) 
  
  Z1 = scale(Z1,scale = F)
  Y1 = scale(Y1,scale = F)
  Z0 = scale(Z0,scale = F)
  
  
  
  out = list()
  
  out$Z0 = Z0
  out$Z = Z1
  out$Y = Y1
  return(out)
}


sim.network.test2 = function(seed, n, n0, m, K){
  sim = sim.network2(seed, n, n0, m, K)
  Z0 = sim$Z0
  D = sim$Y
  Z = sim$Z
  Y = sim$Y
  
  
  p = m
  
  my_data1 = list()
  my_data2 = list()
  my_data3 = list()
  my_data4 = list()
  
  for (q in 1:20){
    FirstStage = lm(Y[,q]~0+Z)
    gamma_hat = FirstStage$coef
    betaZY = NULL
    for (k in 1:m){
      betaZY[k] = as.numeric(lm(Y[,q]~0+Z[,k])$coef)
    }
    betaZX = matrix(NA, nrow = p, ncol = 19)
    D = Y[,-q]
    for (k in 1:m){
      betaZX[k,] = as.numeric(lm(D~0+Z[,k])$coef)
    }
    se_betaZY = NULL
    for (k in 1:m){
      se_betaZY[k] = summary(lm(Y[,q]~0+Z[,k]))$coefficients[, 2]
    }
    
    R1 = t(Z0)%*%Z0/n0 
    a1 = estimate_s_rss(z = betaZY/se_betaZY, R1, method = "null-mle")
    R1 = (1-a1)*R1 + (a1)*diag(p)
    
    result1 = onesample_mvstepIV1(n = n, R = R1, p = m,
                                  betaZX = betaZX,  betaZY = betaZY, 
                                  gamma_hat = gamma_hat,
                                  se_betaZY = se_betaZY)
    
    p1 = pnorm(-abs(result1$beta_est/result1$beta_se))*2
    p1[is.nan(p1)] = 1
    q.1 = p.adjust(p1,method="fdr")
    
    my_data1[[q]] = cbind(q.1, c(1:20)[-q])
    
    R2 = t(Z)%*%Z/n
    
    result2 = onesample_mvstepIV1(n = n, R = R2, p = m,
                                  betaZX = betaZX,  betaZY = betaZY, 
                                  gamma_hat = gamma_hat,
                                  se_betaZY = se_betaZY)
    
    p2 = pnorm(-abs(result2$beta_est/result2$beta_se))*2
    p2[is.nan(p2)] = 1
    q.2 = p.adjust(p2,method="fdr")
    
    my_data2[[q]] = cbind(q.2, c(1:20)[-q])
    
    test11 = diag(x = 0, nrow = p, ncol = p, names = T)
    diag(test11)[1:K] = 1
    W1 = cbind(test11, gamma_hat)
    solve.W1 = t(W1) %*% R2 %*% W1
    ZTY = matrix(diag(R2) * betaZY, ncol = 1)
    YTY = NULL
    for(SNP in 1:p){
      YTY[SNP] = (n-1)*R2[SNP, SNP]*(se_betaZY^2)[SNP] + ZTY[SNP]*betaZY[SNP]
    }
    YTY = mean(YTY)
    beta1 = matrix(ginv(solve.W1) %*% t(W1) %*% (ZTY), ncol = 1)
    beta_est = as.numeric(tail(beta1, n = 19))
    sigma_u2 = as.numeric(YTY - t(beta1)%*%t(W1)%*%ZTY)
    Varbeta = tail(diag(ginv(solve.W1*n)), n = 19)*sigma_u2
    beta_se = as.numeric(sqrt(Varbeta))
    p3 = pnorm(-abs(beta_est/beta_se))*2
    p3[is.nan(p3)] = 1
    q.3 = p.adjust(p3,method="fdr")
    my_data3[[q]] = cbind(q.3, c(1:20)[-q])
    
    test11 = diag(x = 1, nrow = p, ncol = p, names = T)
    W1 = cbind(test11, gamma_hat)
    solve.W1 = t(W1) %*% R2 %*% W1
    ZTY = matrix(diag(R2) * betaZY, ncol = 1)
    beta1 = matrix(ginv(solve.W1) %*% t(W1) %*% (ZTY), ncol = 1)
    beta_est = as.numeric(tail(beta1, n = 19))
    # out$beta4 = beta_est
    sigma_u2 = as.numeric(YTY - t(beta1)%*%t(W1)%*%ZTY)
    Varbeta = tail(diag(ginv(solve.W1*n)), n = 19)*sigma_u2
    beta_se = as.numeric(sqrt(Varbeta))
    p4 = pnorm(-abs(beta_est/beta_se))*2
    q.4 = p.adjust(p4,method="fdr")
    my_data4[[q]] = cbind(q.4, c(1:20)[-q])
  }
  
  N.true1 = 0
  N.true2 = 0
  N.true4 = 0
  N.t1 = 0
  N.t3 = 0
  N.t2 = 0
  N.t4 = 0
  for(J in 1:20){
    test3 = my_data3[[J]]
    N.t3 = N.t3 + length(which(test3[,1]<lvl))
    test1 = my_data1[[J]]
    N.t1 = N.t1 + length(which(test1[,1]<lvl))
    N.true1 = N.true1 + length(intersect(which(test3[,1]<lvl), which(test1[,1]<lvl)))
    N.false1 = N.t1 - N.true1
    test2 = my_data2[[J]]
    N.t2 = N.t2+ length(which(test2[,1]<lvl))
    N.true2 = N.true2 + length(intersect(which(test3[,1]<lvl), which(test2[,1]<lvl)))
    N.false2 = N.t2 - N.true2
    test4 = my_data4[[J]]
    N.t4 = N.t4 + length(which(test4[,1]<lvl))
    N.true4 = N.true4 + length(intersect(which(test3[,1]<lvl), which(test4[,1]<lvl)))
    N.false4 = N.t4 - N.true4
  }
  
  out = c(N.true1, N.false1, N.t1, N.true2, N.false2, N.t2, N.true4, N.false4, N.t4, N.t3)
  
  return(out)
}

n0 = 500
m = 5
K = 2


n0 = 500
m = 15
K = 3


n0 = 500
m = 20
K = 3

n0 = 500
m = 30
K = 5


lvl = 0.01
lvl = 0.05


edge1 = matrix(data = NA, nrow = 500, ncol = 10)
edge2 = matrix(data = NA, nrow = 500, ncol = 10)
edge3 = matrix(data = NA, nrow = 500, ncol = 10)
edge4 = matrix(data = NA, nrow = 500, ncol = 10)
edge5 = matrix(data = NA, nrow = 500, ncol = 10)
edge6 = matrix(data = NA, nrow = 500, ncol = 10)
edge7 = matrix(data = NA, nrow = 500, ncol = 10)

for (i in 1:500){
  sim1 = sim.network.test2(randomseed[i], n = 300, n0, m, K)
  sim2 = sim.network.test2(randomseed[i], n = 600, n0, m, K)
  sim3 = sim.network.test2(randomseed[i], n = 1000, n0, m, K)
  sim4 = sim.network.test2(randomseed[i], n = 1500, n0, m, K)
  sim5 = sim.network.test2(randomseed[i], n = 2000, n0, m, K)
  sim6 = sim.network.test2(randomseed[i], n = 4000, n0, m, K)
  sim7 = sim.network.test2(randomseed[i], n = 6000, n0, m, K)
  edge1[i,] = sim1
  edge2[i,] = sim2
  edge3[i,] = sim3
  edge4[i,] = sim4
  edge5[i,] = sim5
  edge6[i,] = sim6
  edge7[i,] = sim7
}


edge1
edge2
edge3
edge4
edge5
edge6
edge7

edge1 = na.omit(edge1)
edge2 = na.omit(edge2)
edge3 = na.omit(edge3)
edge4 = na.omit(edge4)
edge5 = na.omit(edge5)
edge6 = na.omit(edge6)
edge7 = na.omit(edge7)


pw1 = c(sum(edge1[,1])/sum(edge1[,3]), sum(edge2[,1])/sum(edge2[,3]),
        sum(edge3[,1])/sum(edge3[,3]), sum(edge4[,1])/sum(edge4[,3]),
        sum(edge5[,1])/sum(edge5[,3]), sum(edge6[,1])/sum(edge6[,3]),
        sum(edge7[,1])/sum(edge7[,3]))
fdr1 = c(sum(edge1[,2])/sum(edge1[,3]), sum(edge2[,2])/sum(edge2[,3]), 
         sum(edge3[,2])/sum(edge3[,3]), sum(edge4[,2])/sum(edge4[,3]),
         sum(edge5[,2])/sum(edge5[,3]), sum(edge6[,2])/sum(edge6[,3]),
         sum(edge7[,2])/sum(edge7[,3]))

pw2 = c(sum(edge1[,4])/sum(edge1[,6]), sum(edge2[,4])/sum(edge2[,6]),
        sum(edge3[,4])/sum(edge3[,6]), sum(edge4[,4])/sum(edge4[,6]),
        sum(edge5[,4])/sum(edge5[,6]), sum(edge6[,4])/sum(edge6[,6]),
        sum(edge7[,4])/sum(edge7[,6]))
fdr2 = c(sum(edge1[,5])/sum(edge1[,6]), sum(edge2[,5])/sum(edge2[,6]), 
         sum(edge3[,5])/sum(edge3[,6]), sum(edge4[,5])/sum(edge4[,6]),
         sum(edge5[,5])/sum(edge5[,6]), sum(edge6[,5])/sum(edge7[,6]),
         sum(edge7[,5])/sum(edge7[,6]))

pw0 = c(1,1,1,1,1)
pw3 = c(sum(edge1[,7])/sum(edge1[,9]), sum(edge2[,7])/sum(edge2[,9]),
        sum(edge3[,7])/sum(edge3[,9]), sum(edge4[,7])/sum(edge4[,9]),
        sum(edge5[,7])/sum(edge5[,9]), sum(edge6[,7])/sum(edge6[,9]),
        sum(edge7[,7])/sum(edge7[,9]))
fdr3 = c(sum(edge1[,8])/sum(edge1[,9]), sum(edge2[,8])/sum(edge2[,9]),
         sum(edge3[,8])/sum(edge3[,9]), sum(edge4[,8])/sum(edge4[,9]),
         sum(edge5[,8])/sum(edge5[,9]), sum(edge6[,8])/sum(edge6[,9]),
         sum(edge7[,8])/sum(edge7[,9]))

samplesize = c(300, 600, 1000, 1500, 2000, 4000, 6000)

plot(samplesize, pw1, xlab = 'Sample sizes', ylab = 'Rate of detection', type="l", 
     lty=3, lwd=3, col = 'purple', ylim = c(0,1))
points(samplesize, pw1, pch=19, lwd=3, col = 'purple')
lines(samplesize, pw2, lty=1, lwd=3, col = 'red')

points(samplesize, pw2, pch = 8,lwd=3, col = 'red')
lines(samplesize, pw3, lty=4, lwd=3, col = 'blue')
points(samplesize, pw3, pch = 18,lwd=3, col = 'blue')
legend("topleft", legend=c("SEM-2SLS-stepwise-ref", "SEM-2SLS-stepwise", 'SEM-naive-2SLS'),
       col=c('purple',"red", 'blue'), lty = c(2,1,4), pch = c(19,8,18),
       lwd = 3, cex = 1)


plot(samplesize, fdr1, xlab = 'Sample sizes', ylab = 'Rate of false detection', type="l", 
     lty=3, lwd=3, col = 'purple', ylim = c(0,1))
points(samplesize, fdr1, pch=19, lwd=3, col = 'purple')
lines(samplesize, fdr2, lty=1, lwd=3, col = 'red')
points(samplesize, fdr2, pch = 8,lwd=3, col = 'red')
lines(samplesize, fdr3, lty=4, lwd=3, col = 'blue')
points(samplesize, fdr3, pch = 18,lwd=3, col = 'blue')
legend("bottomleft", legend=c("SEM-2SLS-stepwise-ref", "SEM-2SLS-stepwise", 'SEM-naive-2SLS'),
       col=c('purple',"red", 'blue'), lty = c(2,1,4), pch = c(19,8,18),
       lwd = 3, cex = 1)


