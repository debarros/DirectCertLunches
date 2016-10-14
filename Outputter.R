#Outputter.R

Outputter = function(DirectCert, SMS, MatchScores, n = 5){
  output = data.frame(NA)
  output[,colnames(SMS[,1:21])] = NA
  output = output[-1,-1]
  
  for (i in 1:12){DirectCert[,i] = as.character(DirectCert[,i])}
  
  for (i in 1:21){SMS[,i] = as.character(SMS[,i])}
  
  q = rep("", times = 21)
  
  for (i in 1:nrow(SMS)){
    x = DirectCert[names(tail(sort(MatchScores[i,]),n)),]
    x[,13:21] = NA
    colnames(x) = colnames(output)
    y = SMS[i,1:21]
    output = rbind(output, y)
    output = rbind(output, x)
    output = rbind(output, q)
  }
  
  write.csv(output, file = "DirectCertMatches.csv")
}