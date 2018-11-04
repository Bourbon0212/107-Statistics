Prop_CI_Diff <- function(n1, p1, n2, p2, confd){
  #Group1 Simu
  simu.phat1 = c()
  for (i in 1:10000) {
    phat = sum(sample(c(0,1), n1, replace = T, prob = c(1-p1,p1)))/n1
    simu.phat1[i] = phat
  }
  
  #Group2 Simu
  simu.phat2 = c()
  for (j in 1:10000) {
    phat = sum(sample(c(0,1), n2, replace = T, prob = c(1-p2,p2)))/n2
    simu.phat2[j] = phat
  }
  
  #Diff Phat
  diff.phat = simu.phat1 - simu.phat2
  diff.phat = sort(diff.phat)
  
  #
  low.per = ceiling(((1-confd)/2)*10000)
  low.vlue = diff.phat[low.per]
  high.per = ceiling((1-((1-confd)/2))*10000)
  high.vlue = diff.phat[high.per]
  
  results = c(paste((1-confd)/2*100,"%"), paste((1-((1-confd)/2))*100,"%"), low.vlue, high.vlue)
  
  mean.p = mean(diff.phat)
  sd.p = sd(diff.phat)
  
  CI = list(CI = matrix(results, nrow = 2, byrow = T), mean = mean.p, sd = sd.p )
  
  return(CI)
}

Prop_CI_Diff(1467, 0.624, 1575, 0.490, 0.95)
