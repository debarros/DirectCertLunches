#DataDate.R

#### DataDate ####
DataDate = function(CompArray, row.data, col.data, firstdim, seconddim, datePart, dimensions = c(1,2)){
  if (datePart == "Month"){form = "%m"
  }else if (datePart == "Year"){form = "%Y"
  }else {form = "%d"}
  
  row.info = row.data[,firstdim]
  col.info = col.data[,seconddim]
  q = row.info[match(x = unlist(dimnames(CompArray)[dimensions[1]]), table = rownames(row.data))]
  r = col.info[match(x = unlist(dimnames(CompArray)[dimensions[2]]), table = rownames(col.data))]
  q = format(x = q, format = form)
  r = format(x = r, format = form)
  output = VbetterComp(q, r)
  dimnames(output) = dimnames(CompArray)[dimensions]
  return(output)
}