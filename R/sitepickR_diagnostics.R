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
#' @name unitBalanceTab
#' @param m.out list; selectMatch() output
#' @return dataframe
#' @export
unitBalanceTab <- function(m.out){
  
  return(m.out[[3]])
}

# Love plot for sampled units vs. all units in population

# SMD between sampled units and all units in population
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


# Balance between sampled units and all units in population

#' Summary table: SMD between sampled units and population, for each covariate of interest
#' @name matchBalanceTab 
#' @param m.out list; selectMatch() output
#' @return ggplot object
#' @export
matchBalanceTab <- function(m.out){
  
  return(m.out[[5]][[1]]) ## beautify
  
}


# Balance of sampled vs. replacement units (figure)

# Figure: SMD between sampled units vs. all units in population, for each covariate of interest
#' @name matchBalanceFig
#' @param m.out list; selectMatch() output
#' @return ggplot object
#' @export
matchBalanceFig <- function(m.out,
                            title="Standardized Mean Difference: Replacement Unit Groups (1...K) vs. Originally Selected Units"){
  
  return(m.out[[5]][[2]] + 
           ggplot2::ylim(-2, 2) +
           ggplot2::ggtitle(title) +
           ggplot2::theme(title=ggplot2::element_text(size=10),
                                                                     axis.text.x=ggplot2::element_text(size=9),
                                                                     axis.text.y=ggplot2::element_text(size=9)))
}

# Successful unit matches (figure)

#' Table: Percentage of successful matches in each replacement unit group, 1...K
#' @param m.out list; selectMatch() output
#' @return dataframe
#' @export
matchCountTab  <- function(m.out, title=NULL){
  
  gMCt <- m.out[[6]][[1]]
  
  return(gMCt)
}

#' Percentage of successful matches in each replacement unit group, 1...K
#' @param m.out list; selectMatch() output
#' @return ggplot object
#' @export
matchCountFig  <- function(m.out, 
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
# ' Sub-unit balance between initially selected units and all units in population, for each covariate of interest
#' @param m.out list; selectMatch() output
#' @return ggplot object
#' @export
subUnitBalanceFig  <- function(m.out,
                               title="Subunits from Original and Replacement Unit Groups vs. Population (SMD)"){
  
  return(m.out[[7]][[2]] + 
           ggplot2::ylim(-2, 2) +
           ggplot2::ggtitle(title) +
           ggplot2::theme(title=ggplot2::element_text(size=10),
                                                            axis.text.x=ggplot2::element_text(size=9),
                                                            axis.text.y=ggplot2::element_text(size=9)) +
           ggplot2::theme(strip.text.x = ggplot2::element_text(size = 6)))
}


