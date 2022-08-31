##########################################################################################################
############ Balance diagnostics Functions for selectMatch() [sitepickR Package - Testing Version]  ######
################### Elizabeth A. Stuart, Robert Olsen & Elena Badillo-Goicoechea, May 2022 ###############
##########################################################################################################


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

  return(m.out[[4]] + xlim(-1, 1) + theme(title=element_text(size=12),
                                                axis.text.x=element_text(size=10),
                                                axis.text.y=element_text(size=10)))
}

# 3. SMD between sampled and replacement units (figure)

#' @export
#' @param m.out list; selectMatch() output
#' @return ggplot object
getUnitReplacementBalance <- function(m.out, title=NULL){

  return(m.out[[5]] + ylim(-2, 2) + theme(title=element_text(size=12),
                                              axis.text.x=element_text(size=10),
                                              axis.text.y=element_text(size=10)))
}

# 4. Percentage of successful matches vs. original units in each replacement unit group (figure)

#' @export
#' @param m.out list; selectMatch() output
#' @return ggplot object
getMatchCount  <- function(m.out, title=NULL){

  return(m.out[[6]] + ylim(0, 110) + theme(title=element_text(size=12),
                                           axis.text.x=element_text(size=10),
                                           axis.text.y=element_text(size=10)))
}


## 5. SMD between subunits from sampled and from replacement units (figure)

#' @export
#' @param m.out list; selectMatch() output
#' @return ggplot object
getSubUnitBalance  <- function(m.out){

  return(m.out[[7]] + ylim(-2, 2) + theme(title=element_text(size=12),
                                              axis.text.x=element_text(size=10),
                                              axis.text.y=element_text(size=10)) +
           theme(strip.text.x = element_text(size = 6)))
}

