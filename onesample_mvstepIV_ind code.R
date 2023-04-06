onesample_mvstepIV_ind = function(Y, Z, n, gamma_hat){
  r = dim(gamma_hat)[2]
  testbic = NULL
  set.seed(1234)
  for (i in 1:dim(Z)[2]){
    l = rep(0, dim(Z)[2])
    l[i] = 1
    Z22 = matrix(nrow = dim(Z)[1], ncol = dim(Z)[2])
    for (j in 1:dim(Z)[1]){
      Z22[j,] = Z[j,]*l
    }
    lm_stage2 = lm(Y ~ 0 + cbind(Z22,Z%*%gamma_hat))
    testbic[i] = n*log(sum((lm_stage2$residuals)^2)/n) + log(n)*sum(l)
  }
  
  whichIV = NULL
  whichIV[1] = which.min(testbic)
  BICtest = NULL
  BICtest[1] = testbic[which.min(testbic)]
  for (j in 2:(dim(Z)[2])){
    testbic = NULL
    for (i in 1:(dim(Z)[2])){
      l = rep(0, dim(Z)[2])
      l[whichIV] = 1
      l[i] = 1
      Z22 = matrix(nrow = dim(Z)[1], ncol = dim(Z)[2])
      for (k in 1:dim(Z)[1]){
        Z22[k,] = Z[k,]*l
      }
      lm_stage2 = lm(Y ~ 0 + cbind(Z22,Z%*%gamma_hat))
      testbic[i] = n*log(sum((lm_stage2$residuals)^2)/n)+log(n)*sum(l)
    }
    whichIV[j] = which.min(testbic)
    BICtest[j] = testbic[which.min(testbic)]
    if(whichIV[j] == whichIV[j-1]) break; 
  }
  which.invalid = whichIV[!duplicated(whichIV)]
  K = sort(which.invalid)
  
  Z22 = Z[,K]
  lm_stage2 = lm(Y ~ 0 + cbind(Z22,Z%*%gamma_hat))
  betaest = as.numeric(tail(summary(lm_stage2)$coef[,1],r))
  sigma_u2 = sum((lm_stage2$residuals)^2)/n
  Dhat = Z%*%gamma_hat
  X = cbind(Z22, Dhat)
  Varbeta = tail(diag(ginv(t(X)%*%X)), n = r)*sigma_u2
  betase =  as.numeric(sqrt(Varbeta))
  my_list = list('beta_est' = betaest, 'beta_se' = betase, 'invalid IVs' = K, 'no. of invalid IV' = length(K))
  return(my_list)
}
