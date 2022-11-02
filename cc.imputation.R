
# Some useful keyboard shortcuts for package authoring:
#
#   Install Package:           'Cmd + Shift + B'
#   Check Package:             'Cmd + Shift + E'
#   Test Package:              'Cmd + Shift + T'

cc.imputed <- function(data, m){
  library(mice)
  imputed_Data100 <- mice(data, m, maxit = 5, method = 'pmm', seed = 500)
  fit100=with(imputed_Data100,lm(Ozone ~ Wind + Solar.R + Temp))
  d = matrix(nrow = m, ncol = ncol(data))
  for (i in seq(1,m)){
    for (j in seq(1,4)){
      d[i,j]=fit100$analyses[[i]][[1]][[j]]
      #print(i)
    }
  }
  return(d)
}
