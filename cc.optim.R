cc.optim <- function(x,y){
  nlogL_logstic <- function(paravec){
    # paravec[1] - b0
    # paravec[2] - b1
    cat("paravec=", paravec)
    sum(y*paravec[1])+sum(y*paravec[2]*x)-sum(log(1+exp(paravec[1]+paravec[2]*x)))
  }
  return(optim(c(0,0), #because we have two parameters in the function;
               fn=nlogL_logstic,
               method = "L-BFGS-B",
               control = list(fnscale = -1),
               hessian = FALSE))
}
