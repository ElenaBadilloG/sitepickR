############ Helper functions for selectMatch() [sitepickR Package - Testing Version]  ######
################### Elena Badillo-Goicoechea, Robert Olsen, and Elizabeth A. Stuart, September 2022 ###############
##########################################################################################################

#' @importFrom magrittr "%>%"
#' 
#' @export
#' @param df dataframe
#' @param unitID character; unit column name
#' @param subunitID character; unit column name
#' @return processed df
makeDF <- function(df, unitID, subunitID){
  
  # Assign standard variable name to unit/subunit columns:
  df$unitID <- df[,unitID]
  df$subunitID <- df[,subunitID]
  
  unitID ="unitID"
  subunitID ="subunitID"
  
  # Get unit size (i.e.its number of sub-units):
  dfU <- df %>%
    dplyr::group_by(unitID) %>%
    dplyr::summarise(unitSize = dplyr::n())
  
  df_ <- suppressMessages(dplyr::left_join(df, dfU))

  return(df_)
}

