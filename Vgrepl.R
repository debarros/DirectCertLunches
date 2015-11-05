#VbetterGrepl.R

#### betterGrepl ####
betterGrepl = function(pattern, x, ignore.case = T){
  y = grepl(pattern, x, ignore.case = ignore.case)
  if(sum(is.na(y))>0){
    y[is.na(y)] = 0
    y = as.logical(y)
  }
  return(y)
}





#### Vgrepl ####
Vgrepl = Vectorize(FUN = betterGrepl, vectorize.args = "pattern")