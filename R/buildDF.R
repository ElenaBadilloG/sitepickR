

#' Prepare nested dataset
#' @importFrom magrittr "%>%"
#' @export
#' @param df dataframe
#' @param unit_ID character; unit column name
#' @param subUnit_ID character; unit column name
#' @return processed df
buildDF <- function(df, unit_ID, subUnit_ID){
  
  # Assign standard variable name to unit/subunit columns:
  df$unit_ID <- df[,unit_ID]
  df$subUnit_ID <- df[,subUnit_ID]
  
  unit_ID ="unit_ID"
  subUnit_ID ="subUnit_ID"
  
  # Get unit size (i.e.its number of sub-units):
  dfU <- df %>%
    dplyr::group_by(unit_ID) %>%
    dplyr::summarise(unitSize = dplyr::n())
  
  df_ <- suppressMessages(dplyr::left_join(df, dfU))

  return(df_)
}

