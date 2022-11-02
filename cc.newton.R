# Some useful keyboard shortcuts for package authoring:
#
#   Install Package:           'Cmd + Shift + B'
#   Check Package:             'Cmd + Shift + E'
#   Test Package:              'Cmd + Shift + T'

cc.newton <- function (x, y){
  options(max.print=999999)
  nlogL_log <- function(b0,b1){
    # b0
    # b1
    sum(y*(b0+b1*x))-sum(log(1+exp(b0+b1*x)))
  }
  nor_MLE_of_Newtown <- function (x, itr, b00=0,b11=0) {
    # input: x-vector of observations; itr- iteration times; b00,b11 - initial value
    # output: MLE estimate and maximum log-likelihood value
    b0<- rep(0,itr+1)
    b1<- rep(0,itr+1)
    b0[1] <- b00
    b1[1] <- b11
    for (i in 1:itr){
      #b[n+1] = b[n] - l(bn)/l'(bn)
      b0[i+1] = b0[i] - (sum(y)-sum(exp(b0[i]+b1[i]*x)/(1+exp(b0[i]+b1[i]*x))))/
        (-sum((exp(b0[i]+b1[i]*x))/(exp(b0[i]+b1[i]*x)+1)^2))

      #sigma2[i+1] = sigma[i] - sigma[i] + (1/n * sum((xi - mu)^2))
      b1[i+1]=b1[i] - (sum(x*y)-sum(x*exp(b0[i]+b1[i]*x)/(1+exp(b0[i]+b1[i]*x))))/
        (-sum(((x^2)*exp(b0[i]+b1[i]*x))/(exp(b0[i]+b1[i]*x)^2)))

    }
    data.frame(b0=b0, b1=b1,log.lik.value=nlogL_log(b0,b1))
  }
  return(nor_MLE_of_Newtown(x,1000))
}
