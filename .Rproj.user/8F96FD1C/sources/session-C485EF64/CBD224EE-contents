#' @importFrom magrittr "%>%"
#' 
#' 
#' 
#' 
#### Get SMD for a given variable:
#' @noRd
#' @param df dataframe
#' @param var character; column name of variable of interest
#' @return numeric; standardized mean difference
getSMD <- function(df, var){

  dsel <- dplyr::filter(df, Selected == 1)
  popMean <- mean(df[,var], na.rm=T)
  popSD <- stats::sd(df[,var], na.rm=T)

  SMD <- ((stats::weighted.mean(dsel[,var], w = dsel$w, na.rm=T)) - popMean) / popSD

  return(SMD)

}

##### Covariate SMD between Units and Population:
#' @noRd
#' @param dfSU
#' @param unit_vars
#' @param exactMatchVars
#' @return ggplot2::ggplot object
unitSampBalance_ <- function(dfSU, unit_vars, exactMatchVars){

  # Get weighted SMD w/r to population for each covariate:

  unitPlotVars <- unique(c("unitSize", setdiff(unit_vars, exactMatchVars)))
  unitSMDs <- as.data.frame(unlist(lapply(unitPlotVars, function(x) getSMD(dplyr::distinct_at(dfSU,
                                                              c("unitID", "Selected", unitPlotVars, "w")), x))))

  unitSMDs$unit_vars <- unique(c("unitSize", setdiff(unit_vars, exactMatchVars)))
  colnames(unitSMDs) <- c("SMD", "Covariate")
  unitSMDs$Covariate <- factor(unitSMDs$Covariate, levels=unique(c("unitSize", setdiff(unit_vars, exactMatchVars))))

  # Build balance table (SMD) (user output #3):
  unitSampBalTab <- dplyr::select(unitSMDs, c("Covariate", "SMD"))

  # Build love plot (user output #4):
  unitSampLvPLt <- ggplot2::ggplot(unitSMDs, ggplot2::aes(x=SMD, y=Covariate)) +
    ggplot2::geom_dotplot(binaxis='y', fill="#0F3957", dotsize = 0.95) +
    ggplot2::ggtitle("") +
    ggplot2::xlim(-0.5, 0.5) +
    ggplot2::geom_vline(xintercept=0) +
    ggplot2::theme_bw()

  return(list(unitSampBalTab, unitSampLvPLt))
}

#### Covariate SMD between Replacement (1,...,nth) and Initially selected (0) unit groups:
#' @noRd
#' @param mUnits
#' @param unitNumVars
#' @param nRepUnits
#' @return ggplot2::ggplot object
matchBalance_ <- function(mUnits, unitNumVars, nRepUnits){

    dfOrig <- as.data.frame(mUnits %>% dplyr::filter(unitGroup == 0) %>%
                              dplyr::select(c("unitID", "unitSize", dplyr::all_of(unitNumVars))))
    dfOrig <- dplyr::summarise_at(dfOrig, c("unitSize", dplyr::all_of(unitNumVars)), mean)

    dfOrigsd <- as.data.frame(mUnits %>% dplyr::filter(unitGroup == 0) %>%
                                dplyr::select(c("unitID", "unitSize", dplyr::all_of(unitNumVars))))
    dfOrigsd <- dplyr::summarise_at(dfOrigsd, c("unitSize", dplyr::all_of(unitNumVars)), sd)

    # Aggregate all potential units, grouped into replacement category i.e. original (0), 1,,,.n.
    dfU10g <- mUnits %>% dplyr::select(c("unitID","unitGroup",  "unitSize", dplyr::all_of(unitNumVars))) %>%
      dplyr::group_by_at(c("unitGroup")) %>%
      dplyr::summarise_at(c("unitSize", unitNumVars), mean)

    # Get (non-weighted) SMD of each group against original:
    for(col in c("unitSize", dplyr::all_of(unitNumVars))){
      dfU10g[,paste(col, " ", sep="")] <- (dfU10g[,col] - dfOrig[,col]) / dfOrigsd[,col]
    }

    dfU10gPlt <- dplyr::select(dfU10g, "unitGroup", c("unitGroup", unlist(lapply(colnames(dfU10g),
                                                                      function (x) {if(stringr::str_detect(x, " ")) {return(x)}}))))
    dfU10gPlt[,'All Covariates \n (average of the absolute SMDs)'] <- rowMeans(dataf.abs <- dfU10gPlt[,(2:ncol(dfU10gPlt))] %>%
                                                                        dplyr::select_if(is.numeric) %>%
                                                                         abs(), na.rm=T)

    dfU10gPlt <- dplyr::filter(dfU10gPlt, unitGroup!=0)
    dfU10gPlt <- reshape2::melt(dfU10gPlt, id.vars="unitGroup")


    # Line chart (user output #5):
    u10gPlt <- ggplot2::ggplot(dfU10gPlt, ggplot2::aes(x=unitGroup, y=value)) +
      ggplot2::geom_line(color="#0F3957")  +
      ggplot2::ggtitle("") +
      ggplot2::geom_hline(yintercept = 0, style="dashed", color="grey") +
      ggplot2::ylab("SMD") +
      ggplot2::xlab("Replacement Unit Group") +
      ggplot2::scale_x_continuous(breaks=c(1:nRepUnits), 
                                  labels=unlist(lapply(unique(dfU10gPlt$unitGroup), 
                                          function(x) {paste0("Repl. Group", x)}))) +
      ggplot2::theme_bw() +
      ggplot2::theme(axis.text.x = ggplot2::element_text(angle=90)) +
      ggplot2::facet_wrap(facets=~variable)

    return(list(dfU10gPlt, u10gPlt))
}

##### % Of successful matches per replacement group:
#' @noRd
#' @param replacementUnits
#' @param nRepUnits
#'@return ggplot2::ggplot object
matchCount_ <- function(replacementUnits, nRepUnits){

  completeunitGroups <- t(as.data.frame(t(as.data.frame(1 - colMeans(is.na(replacementUnits)))*100)))
  completeunitGroups <- as.data.frame(completeunitGroups)

  completeunitGroups$UnitGroup <- as.character(c(0:nRepUnits))
  colnames(completeunitGroups) <- c("Perc_Matches", "UnitGroup")
  completeunitGroups <- dplyr::filter(completeunitGroups, UnitGroup != 0)
  completeunitGroups$UnitGroup <- factor(completeunitGroups$UnitGroup, levels=as.character(c(1:nRepUnits)))

  # Bar chart (user output #6):
  u10barUnitPerc <- ggplot2::ggplot(completeunitGroups, ggplot2::aes(x=UnitGroup, y=Perc_Matches)) +
    
    ggplot2::geom_col(color="black", fill="#0F3957") +
    ggplot2::theme_bw() +
    ggplot2::ylab("% Matches") +
    ggplot2::xlab("Unit Group") +
    ggplot2::ggtitle("") +
    ggplot2::scale_x_discrete(labels=unlist(lapply(completeunitGroups$UnitGroup, 
                                          function(x) {paste0("Repl. Group", x)}))) +
    ggplot2::theme(axis.text.x = ggplot2::element_text(angle=90))

    return(list(completeunitGroups, u10barUnitPerc))
}


#### Covariate SMD between Sub-units and Population:
#' @noRd
#' @param df_
#' @param dfSU
#' @param mUnits
#' @param subUnitTable
#' @param subunitNumVars
#' @param RepUnits
#' @return ggplot2::ggplot object
subUnitBalance_ <- function(df_, dfSU, mUnits, subUnitTable, subunitNumVars, nRepUnits){
  selectedSubunits <- reshape2::melt(subUnitTable)
  colnames(selectedSubunits) <- c("ID", "subunitID")

  dfsub <- stats::na.omit(dplyr::left_join(selectedSubunits, dfSU, by = "subunitID"))
  dfsub <- dplyr::distinct(dplyr::inner_join(dfsub, dplyr::select(mUnits, c("unitID", "unitGroup")), by = "unitID"))


  dfPOP <- as.data.frame(df_ %>%
                           dplyr::select(c("unitID", dplyr::all_of(subunitNumVars))))
  dfPOP <- dplyr::summarise_at(dfPOP, subunitNumVars, mean)

  dfPOPsd <- as.data.frame(df_ %>%
                             dplyr::select(c("unitID", "unitSize", dplyr::all_of(subunitNumVars))))
  dfPOPsd <- dplyr::summarise_at(dfPOPsd, subunitNumVars, sd)

  dfSub10g <- dfsub %>% dplyr::select(c("unitID","unitGroup",  "unitSize",
                                        dplyr::all_of(subunitNumVars))) %>%
    dplyr::group_by_at(c("unitGroup")) %>%
    dplyr::summarise_at(subunitNumVars, mean)

  for(col in subunitNumVars){
    dfSub10g[,paste(col, " ", sep="")] <- (dfSub10g[,col] - dfPOP[,col]) / dfPOPsd[,col]
  }

  dfSub10gPlt <- dplyr::select(dfSub10g, "unitGroup",
                        c("unitGroup", unlist(lapply(colnames(dfSub10g),
                                              function (x) {if(stringr::str_detect(x, " ")) {return(x)}}))))
  dfSub10gPlt[,'All Covariates \n (average of the absolute SMDs)'] <-
                        rowMeans(dataf.abs <- dfSub10gPlt[,(2:ncol(dfSub10gPlt))] %>%
                                                     dplyr::select_if(is.numeric) %>%
                                                                                 abs(), na.rm=T)

  dfSub10gPlt <- reshape2::melt(dfSub10gPlt, id.vars="unitGroup")
  

  # Line chart (user output #7):
  sub10gPlt <- ggplot2::ggplot(dfSub10gPlt, ggplot2::aes(x=unitGroup, y=value)) +
    ggplot2::geom_line(color="#0F3957")  +
    ggplot2::ggtitle("") +
    ggplot2::geom_hline(yintercept = 0, style="dashed", color="grey") +
    ggplot2::ylab("SMD") +
    ggplot2::xlab("Unit Group") +
    ggplot2::theme_bw() +
    ggplot2::scale_x_continuous(breaks = unique(dfSub10gPlt$unitGroup),
              labels=c("Original", unlist(lapply(unique(dfSub10gPlt$unitGroup)[2:(nRepUnits+1)], 
                     function(x) {paste0("Repl.Group", x)})))) +
    ggplot2::theme(axis.text.x = ggplot2::element_text(angle=90)) +
    ggplot2::facet_wrap(facets=~variable)

  return(list(dfSub10gPlt, sub10gPlt))
}
