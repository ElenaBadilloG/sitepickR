##########################################################################################################
############ Wrapper functions for getting balance diagnostics figures for selectMatch() [sitepickR Package - Testing Version]  ######
################### Robert Olsen, Elizabeth A. Stuart & Elena Badillo-Goicoechea, August 2022 ###############
##########################################################################################################


# Love plot for sampled units vs. all units in population

#' SMD between sampled units and all units in population
#' @name unitLovePlot
#' @param m.out list; selectMatch() output
#' @return ggplot object
#' @export
unitLovePlot <- function(m.out,
                         title="Standardized Mean Difference: \n Initially Selected Units vs. Population"){
  
  return(m.out[[4]] +
           ggplot2::xlim(-1, 1) +
           ggplot2::ggtitle(title) +
           ggplot2::theme(title=ggplot2::element_text(size=10),
                          axis.text.x=ggplot2::element_text(size=9),
                          axis.text.y=ggplot2::element_text(size=9)))
}


# Balance of sampled vs. replacement units (figure)

#' Figure: SMD between sampled units vs. all units in population, for each covariate of interest
#' @name matchBalance
#' @param m.out list; selectMatch() output
#' @return ggplot object
#' @export
matchBalance <- function(m.out,
                            title="Standardized Mean Difference: Replacement Unit Groups (1...K) vs. Originally Selected Units"){
  
  return(m.out[[5]][[2]] + 
           ggplot2::ylim(-2, 2) +
           ggplot2::ggtitle(title) +
           ggplot2::theme(title=ggplot2::element_text(size=10),
                                                                     axis.text.x=ggplot2::element_text(size=9),
                                                                     axis.text.y=ggplot2::element_text(size=9)))
}

# Successful unit matches

#' Percentage of successful matches in each replacement unit group, 1...K
#' @name matchCount
#' @param m.out list; selectMatch() output
#' @return ggplot object
#' @export
matchCount  <- function(m.out, 
                           title="Percentage of Successful Matches per Unit Group"){
  
  gMC <- m.out[[6]][[2]] + 
    ggplot2::ylim(0, 110) + 
    ggplot2::ggtitle(title) +
    ggplot2::theme(title=ggplot2::element_text(size=10),
           axis.text.x=ggplot2::element_text(size=9),
           axis.text.y=ggplot2::element_text(size=9))
  
  return(gMC)
}


# Balance of original sub-units vs. sub-units from replacement unit groups (figure)
#' Sub-unit balance between initially selected units and all units in population, for each covariate of interest
#' @name subUnitBalance
#' @param m.out list; selectMatch() output
#' @return ggplot object
#' @export
subUnitBalance  <- function(m.out,
                               title="Subunits from Original and Replacement Unit Groups vs. Population (SMD)"){
  
  return(m.out[[7]][[2]] + 
           ggplot2::ylim(-2, 2) +
           ggplot2::ggtitle(title) +
           ggplot2::theme(title=ggplot2::element_text(size=10),
                                                            axis.text.x=ggplot2::element_text(size=9),
                                                            axis.text.y=ggplot2::element_text(size=9)) +
           ggplot2::theme(strip.text.x = ggplot2::element_text(size = 6)))
}



#' Summary tables, with unit/matches/sub-unit balance between initially selected units and all units in population, for each covariate of interest
#' @name summary
#' @param m.out list; selectMatch() output
#' @param balMeasure numeric; balance Diagnostic (1=original unit balance,
#'  2=match balance, 3=match success percent, 4=sub-unit balance)
#' @return ggplot object
#' @export
summary  <- function(m.out, balMeasure){
  
  if(balMeasure == 1) return(m.out[[3]]) else {
    if(balMeasure == 2) return(m.out[[5]][[1]]) else {
      if(balMeasure == 3) return(m.out[[6]][[1]]) else {
        if(balMeasure == 4) return(m.out[[7]][[1]]) else {
        }}}}}
  
