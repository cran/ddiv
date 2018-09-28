## This function exchange tabular data set into one single characteric cell
##
tab_to_char <- function (tabular_data)
{
  tabular_col_names <- as.data.frame(names(tabular_data))

  data_row_merge <- as.data.frame(apply(t(tabular_data), 2, paste, collapse = "*"))
  data_col_merge <- as.data.frame(apply(t(data_row_merge), 1, paste, collapse = "#"))

  colname_merge <- as.data.frame(apply((tabular_col_names), 2, paste, collapse = "*"))
  merged_data <- as.character(paste0(colname_merge[, 1], "#", data_col_merge[, 1]))
  return(merged_data)

}

