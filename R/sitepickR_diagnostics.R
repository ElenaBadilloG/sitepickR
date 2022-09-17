##########################################################################################################
############ Wrapper functions for getting balance diagnostics figures for selectMatch() [sitepickR Package - Testing Version]  ######
################### Robert Olsen, Elizabeth A. Stuart & Elena Badillo-Goicoechea, August 2022 ###############
##########################################################################################################

#' @importFrom magrittr "%>%"
#' 
#' 
#' 
# Balance table for sampled units vs. all units in population
# Table with average SMD across all covariates of interest between original units and target population
#' @export
#' @param m.out list; selectMatch() output
#' @return dataframe
unitBalanceTab <- function(m.out){
  
  return(m.out[[3]])
}

# Love plot for sampled units vs. all units in population

# SMD between sampled units and all units in population
#' @export
#' @param m.out list; selectMatch() output
#' @return ggplot object
unitLovePlot <- function(m.out){
  
  return(m.out[[4]] +
           ggplot2::xlim(-1, 1) +
           ggplot2::theme(title=ggplot2::element_text(size=10),
                          axis.text.x=ggplot2::element_text(size=9),
                          axis.text.y=ggplot2::element_text(size=9)))
}


# Balance between sampled units and all units in population

#' Summary table: SMD between sampled units and population, for each covariate of interest
#' @export
#' @param m.out list; selectMatch() output
#' @return ggplot object
matchBalanceTab <- function(m.out, title=NULL){
  
  return(m.out[[5]][[1]]) ## beautify
  
}


# Balance of sampled vs. replacement units (figure)

#' Figure: SMD between sampled units vs. all units in population, for each covariate of interest
#' @export
#' @param m.out list; selectMatch() output
#' @return ggplot object
matchBalanceFig <- function(m.out, title=NULL){
  
  return(m.out[[5]][[2]] + 
           ggplot2::ylim(-2, 2) +
           ggplot2::theme(title=ggplot2::element_text(size=10),
                                                                     axis.text.x=ggplot2::element_text(size=9),
                                                                     axis.text.y=ggplot2::element_text(size=9)))
}

# Successful unit matches (figure)

#' Table: Percentage of successful matches in each replacement unit group, 1...K
#' @export
#' @param m.out list; selectMatch() output
#' @return dataframe
matchCountTab  <- function(m.out, title=NULL){
  
  gMCt <- m.out[[6]][[1]]
  
  return(gMCt)
}
#' Percentage of successful matches in each replacement unit group, 1...K
#' @export
#' @param m.out list; selectMatch() output
#' @return ggplot object
matchCountFig  <- function(m.out, title=NULL){
  
  gMC <- m.out[[6]][[2]] + 
    ggplot2::ylim(0, 110) + 
    ggplot2::theme(title=ggplot2::element_text(size=10),
           axis.text.x=ggplot2::element_text(size=9),
           axis.text.y=ggplot2::element_text(size=9))
  
  return(gMC)
}


# Balance of original sub-units vs. sub-units from replacement unit groups (figure)
# ' Sub-unit balance between initially selected units and all units in population, for each covariate of interest
#' @export
#' @param m.out list; selectMatch() output
#' @return ggplot object
subUnitBalanceFig  <- function(m.out){
  
  return(m.out[[7]] + 
           ggplot2::ylim(-2, 2) +
           ggplot2::theme(title=ggplot2::element_text(size=10),
                                                            axis.text.x=ggplot2::element_text(size=9),
                                                            axis.text.y=ggplot2::element_text(size=9)) +
           ggplot2::theme(strip.text.x = ggplot2::element_text(size = 6)))
}


