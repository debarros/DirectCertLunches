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
  
  row.info = row.data[,firstdim]
  col.info = col.data[,seconddim]
  
  if(messageLevel > 1) message("go through this weird process")
  
  q = row.info[match(x = unlist(dimnames(CompArray)[dimensions[1]]), table = rownames(row.data))]
  r = col.info[match(x = unlist(dimnames(CompArray)[dimensions[2]]), table = rownames(col.data))]
  q = format(x = q, format = form)
  r = format(x = r, format = form)
  output = VbetterComp(q, r, messageLevel = messageLevel - 1)
  dimnames(output) = dimnames(CompArray)[dimensions]
  
  if(messageLevel > 0) message("Ending DataDate function")
  
  return(output)
}