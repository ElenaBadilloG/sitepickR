##########################################################################################################
############ Wrapper functions for getting balance diagnostics figures for selectMatch() [sitepickR Package - Testing Version]  ######
################### Robert Olsen, Elizabeth A. Stuart & Elena Badillo-Goicoechea, August 2022 ###############
##########################################################################################################

#' @importFrom magrittr "%>%"
#' 
#' 
#' 
#' Balance table for sampled units vs. all units in population
#'
#' Table with average SMD across all covariates of interest between original units and target population
#' @export
#' @param m.out list; selectMatch() output
#' @return ggplot object
getUnitBalanceTab <- function(m.out){
  
  return(m.out[[3]])
}

#' Love plot for sampled units vs. all units in population (figure)

#' SMD between sampled units and all units in population
#' @export
#' @param m.out list; selectMatch() output
#' @return ggplot object
getUnitLovePlot <- function(m.out){
  
  return(m.out[[4]] + ggplot2::xlim(-1, 1) +
           ggplot2::theme(title=ggplot2::element_text(size=10),
                          axis.text.x=ggplot2::element_text(size=9),
                          axis.text.y=ggplot2::element_text(size=9)))
}

#' Balance of sampled vs. replacement units (figure)

#' SMD between sampled units vs. all units in population, for each covariate of interest
#' @export
#' @param m.out list; selectMatch() output
#' @return ggplot object
getUnitReplacementBalance <- function(m.out, title=NULL){
  
  return(m.out[[5]] + ggplot2::ylim(-2, 2) + ggplot2::theme(title=ggplot2::element_text(size=10),
                                                                     axis.text.x=ggplot2::element_text(size=9),
                                                                     axis.text.y=ggplot2::element_text(size=9)))
}

#' Successful unit matches (figure)

#' Percentage of successful matches vs. original units in each replacement unit group
#' @export
#' @param m.out list; selectMatch() output
#' @return ggplot object
getMatchCount  <- function(m.out, title=NULL){
  
  return(m.out[[6]] + ggplot2::ylim(0, 110) + ggplot2::theme(title=ggplot2::element_text(size=10),
                                                             axis.text.x=ggplot2::element_text(size=9),
                                                             axis.text.y=ggplot2::element_text(size=9)))
}


#' Balance of original sub-units vs. sub-units from replacement unit groups (figure)

#' SMD between sampled units vs. all units in population, for each covariate of interest
#' @export
#' @param m.out list; selectMatch() output
#' @return ggplot object
getSubUnitBalance  <- function(m.out){
  
  return(m.out[[7]] + ggplot2::ylim(-2, 2) + ggplot2::theme(title=ggplot2::element_text(size=10),
                                                            axis.text.x=ggplot2::element_text(size=9),
                                                            axis.text.y=ggplot2::element_text(size=9)) +
           ggplot2::theme(strip.text.x = ggplot2::element_text(size = 6)))
}


