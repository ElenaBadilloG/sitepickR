##########################################################################################################
############ Wrapper functions for getting balance diagnostics figures for selectMatch() [sitepickR Package - Testing Version]  ######
################### Robert Olsen, Elizabeth A. Stuart & Elena Badillo-Goicoechea, August 2022 ###############
##########################################################################################################

#' @importFrom magrittr "%>%"
#' 
## 1. SMD between sampled units and all units in population  (table)
#' @export
#' @param m.out list; selectMatch() output
#' @return ggplot object
getUnitBalanceTab <- function(m.out){
  
  return(m.out[[3]])
}

## 2. SMD between sampled units and all units in population (figure)
#' @export
#' @param m.out list; selectMatch() output
#' @return ggplot object
getUnitLovePlot <- function(m.out){
  
  return(m.out[[4]] + ggplot2::xlim(-1, 1) +
           ggplot2::theme(title=ggplot2::element_text(size=10),
                          axis.text.x=ggplot2::element_text(size=9),
                          axis.text.y=ggplot2::element_text(size=9)))
}

# 3. SMD between sampled and replacement units (figure)

#' @export
#' @param m.out list; selectMatch() output
#' @return ggplot object
getUnitReplacementBalance <- function(m.out, title=NULL){
  
  return(m.out[[5]] + ggplot2::ylim(-2, 2) + ggplot2::theme(title=ggplot2::element_text(size=10),
                                                                     axis.text.x=ggplot2::element_text(size=9),
                                                                     axis.text.y=ggplot2::element_text(size=9)))
}

# 4. Percentage of successful matches vs. original units in each replacement unit group (figure)

#' @export
#' @param m.out list; selectMatch() output
#' @return ggplot object
getMatchCount  <- function(m.out, title=NULL){
  
  return(m.out[[6]] + ggplot2::ylim(0, 110) + ggplot2::theme(title=ggplot2::element_text(size=10),
                                                             axis.text.x=ggplot2::element_text(size=9),
                                                             axis.text.y=ggplot2::element_text(size=9)))
}


## 5. SMD between subunits from sampled and from replacement units (figure)

#' @export
#' @param m.out list; selectMatch() output
#' @return ggplot object
getSubUnitBalance  <- function(m.out){
  
  return(m.out[[7]] + ggplot2::ylim(-2, 2) + ggplot2::theme(title=ggplot2::element_text(size=10),
                                                            axis.text.x=ggplot2::element_text(size=9),
                                                            axis.text.y=ggplot2::element_text(size=9)) +
           ggplot2::theme(strip.text.x = ggplot2::element_text(size = 6)))
}


