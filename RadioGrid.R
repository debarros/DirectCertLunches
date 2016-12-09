#RadioGrid.R

#MultiRadio creates a grid of radio buttons, so that the user can make one selection for each row
#RowNames is the set of elements that each need a selection
#ColumNames is the set of options to choose from for each element of RowNames
#gridName is the name of the grid, and is appended to the RowName to create the inputId
#selections is the set of default selections from ColumnNames for the elements of RowNames
#width is not currently used

MultiRadio = function(RowNames, ColumnNames, gridname, selections = NULL, width = NULL, gridnameLast = F){
  print("running MultiRadio function")
  
  if(is.null(selections)){selections = rep(ColumnNames[1], times = length(RowNames))}
  
  if(is.numeric(selections) & is.numeric(ColumnNames)){
    selections[selections > ColumnNames[length(ColumnNames)]] = ColumnNames[length(ColumnNames)]
    selections[!(selections %in% ColumnNames)] = ColumnNames[1]}
  
  badselections = selections[!(selections %in% ColumnNames)]
  if(length(badselections)>0){stop(paste0(
    "Error in Multiradio.  The following selections are not in ColumnNames: ",
    paste0(badselections, collapse = " ") ))}
  
  
  if(length(selections) != length(RowNames)){stop(paste0(
    "Error in Multiradio.  The number of selections must match the number of RowNames.  ",
    "There were ", length(selections), " selections and ", length(RowNames), " Rownames." ))}
  
  x = list()
  for (i in 1:length(RowNames)){
    if(gridnameLast){thisInputId = paste0(RowNames[i], gridname)
    } else {thisInputId = paste0(gridname,RowNames[i])}
    x[[i]] = radioButtons(
      inputId = thisInputId,
      label = RowNames[i],
      choices = ColumnNames,
      selected = selections[i], 
      inline = TRUE)
  }
  
  print("done running MultiRadio function")
  return(x)
}