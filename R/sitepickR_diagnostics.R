##########################################################################################################
############ Wrapper functions for getting balance diagnostics figures for selectMatch() [sitepickR Package - Testing Version]  ######
################### Robert Olsen, Elizabeth A. Stuart & Elena Badillo-Goicoechea, August 2022 ###############
##########################################################################################################


# Love plot for sampled units vs. all units in population

#' @title unitLovePlot
#' SMD between sampled units and all units in population
#' @name unitLovePlot
#' @param smOut list; selectMatch() output
#' @param title character; user-specified figure title
#' @return ggplot object
#' @export
unitLovePlot <- function(smOut,
                         title="Standardized Mean Difference: \n Initially Selected Units vs. Population"){
  
  return(smOut[[4]] +
           ggplot2::xlim(-1, 1) +
           ggplot2::ggtitle(title) +
           ggplot2::theme(title=ggplot2::element_text(size=10),
                          axis.text.x=ggplot2::element_text(size=9),
                          axis.text.y=ggplot2::element_text(size=9)))
}


# Balance of sampled vs. replacement units (figure)

#' @title matchBalance
#' Balance between sampled units vs. all units in population, for each covariate of interest
#' @name matchBalance
#' @param smOut list; selectMatch() output
#' @param title character; user-specified figure title
#' @return ggplot object
#' @export
matchBalance <- function(smOut,
                            title="Standardized Mean Difference: Replacement Unit Groups (1...K) vs. Originally Selected Units"){
  
  return(smOut[[5]][[2]] + 
           ggplot2::ylim(-2, 2) +
           ggplot2::ggtitle(title) +
           ggplot2::theme(title=ggplot2::element_text(size=10),
                                                                     axis.text.x=ggplot2::element_text(size=9),
                                                                     axis.text.y=ggplot2::element_text(size=9)))
}

#' @title matchFreq
#' Distribution of successful matches among original units
#' @name matchFreq
#' @param smOut list; selectMatch() output
#' @param title character; user-specified figure title
#' @return ggplot object
#' @export
matchFreq <- function(smOut, 
                      title="Match Frequency per Original Unit"){
  
  gMF <- smOut[[6]][[2]] + 
    ggplot2::ggtitle(title) +
    ggplot2::theme(title=ggplot2::element_text(size=10),
                   axis.text.x=ggplot2::element_text(size=9),
                   axis.text.y=ggplot2::element_text(size=9))
  
  return(gMF)
}
  
  
#' @title matchCount
#' Percentage of successful matches in each replacement unit group, 1...K
#' @name matchCount
#' @param smOut list; selectMatch() output
#' @param title character; user-specified figure title
#' @return ggplot object
#' @export
matchCount  <- function(smOut, 
                           title="Percentage of Successful Matches per Unit Group"){
  
  gMC <- smOut[[7]][[2]] + 
    ggplot2::ylim(0, 110) + 
    ggplot2::ggtitle(title) +
    ggplot2::theme(title=ggplot2::element_text(size=10),
           axis.text.x=ggplot2::element_text(size=9),
           axis.text.y=ggplot2::element_text(size=9))
  
  return(gMC)
}


#' @title subUnitBalance
#' Sub-unit balance between initially selected units and all units in population, for each covariate of interest
#' @name subUnitBalance
#' @param smOut list; selectMatch() output
#' @param title character; user-specified figure title
#' @return ggplot object
#' @export
subUnitBalance  <- function(smOut,
                               title="Subunits from Original and Replacement Unit Groups vs. Population (SMD)"){
  
  return(smOut[[8]][[2]] + 
           ggplot2::ylim(-2, 2) +
           ggplot2::ggtitle(title) +
           ggplot2::theme(title=ggplot2::element_text(size=10),
                                                            axis.text.x=ggplot2::element_text(size=9),
                                                            axis.text.y=ggplot2::element_text(size=9)) +
           ggplot2::theme(strip.text.x = ggplot2::element_text(size = 6)))
}



#' @title getSummary
#' Build summary tables, with unit/match/sub-unit balance between initially selected units and a target population, for each covariate of interest
#' @name getSummary
#' @param smOut list; selectMatch() output
#' @param diagnostic numeric; balance Diagnostic:
#'   "unitBal" = original unit balance,
#'   "matchBal" = match balance,
#'   "matchFreq" = sucessful match frequency,
#'   "matchCount" = match success count by replacement group,
#'   "subunitBal" =sub-unit balance
#' @return ggplot object
#' @export
getSummary  <- function(smOut, diagnostic){
  
  if(diagnostic == "unitBal") return(smOut[[3]]) else {
    if(diagnostic ==  "matchBal" ) return(smOut[[5]][[1]]) else {
      if(diagnostic == "matchFreq") return(smOut[[6]][[1]]) else {
        if(diagnostic == "matchCount") return(smOut[[7]][[1]]) else {
         if(diagnostic == "subunitBal") return(smOut[[8]][[1]]) else {
        }}}}}}
  
