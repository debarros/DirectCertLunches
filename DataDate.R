#DataDate.R


#### DataDate ####
DataDate = function(CompArray, row.data, col.data, firstdim, seconddim, datePart, dimensions = c(1,2), messageLevel = 0){
  
  if(messageLevel > 0) message("starting DataDate function")
  
  if(messageLevel > 1) message("set the part of date")
  
  if (datePart == "Month"){
    form = "%m"
  }else if (datePart == "Year"){
    form = "%Y"
  } else {
    form = "%d"
  }
  
  if(messageLevel > 1) message("collect info from row.data and col.data")
  
  row.info = row.data[,firstdim[1]]
  col.info = col.data[,seconddim[1]]
  
  if(messageLevel > 1) message("extract the relevant date part from each set and compare them")
  
  #I don't remember the purpose of the next two lines
  q = row.info[match(x = unlist(dimnames(CompArray)[dimensions[1]]), table = rownames(row.data))] 
  r = col.info[match(x = unlist(dimnames(CompArray)[dimensions[2]]), table = rownames(col.data))]
  
  #Extract the relevant date part
  q = format(x = q, format = form)
  r = format(x = r, format = form)
  
  #Compare the date parts from the two sets
  output = VbetterComp(q, r, messageLevel = messageLevel - 1)
  dimnames(output) = dimnames(CompArray)[dimensions]
  
  gc()
  
  if(messageLevel > 0) message("Ending DataDate function")
  
  return(output)
}