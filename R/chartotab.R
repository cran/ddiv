## This function exchange one single character cell to tabular data set
char_to_tab <- function (merged_data){
  tab_row <- as.data.frame(strsplit(as.character(merged_data), "#"))
  tabular_data <- c()
  for (i in 1:length(tab_row[,1])) {
    tab_col <- as.data.frame(strsplit(as.character(tab_row[i,]), "\\*"))
    names(tab_col) <- "row"
    tab_col <- as.data.frame(t(tab_col))
    tabular_data <- rbind(tabular_data, tab_col)
  }
  colnames(tabular_data) <- as.character(unlist(tabular_data[1,]))
  tabular_data <- tabular_data[-c(1), ]
  return(tabular_data)
}

