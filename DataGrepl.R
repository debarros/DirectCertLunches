#DataGrepl.R

#### DataGrepl ####

# This function does the following:
#  1) takes two dimensions of an array
#  2) pulls their dimnames
#  3) matches those to the rows names of two separate data.frames
#  4) pulls data values from the selected rows using specified variables
#  5) uses regular expressions to match patterns from each selected value from the first data.frame to each selected value of the second
#  6) outputs a logical matrix showing which patterns from the first data.frame found matches in which values from the second

DataGrepl = function(CompArray, row.data, col.data, firstdim, seconddim, dimensions = c(1,2), messageLevel = 0){
  
  if(messageLevel > 0) message("starting DataGrepl function")
  
  row.info = row.data[,firstdim]
  col.info = col.data[,seconddim]
  q = row.info[match(x = unlist(dimnames(CompArray)[dimensions[1]]), table = rownames(row.data))]
  r = col.info[match(x = unlist(dimnames(CompArray)[dimensions[2]]), table = rownames(col.data))]
  output = t(Vgrepl(pattern = q, x = r))
  dimnames(output) = dimnames(CompArray)[dimensions]
  
  if(messageLevel > 0) message("Ending DataGrepl function")
  
  return(output)
}