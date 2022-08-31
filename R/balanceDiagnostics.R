# Helper. Get SMD for a given variable:

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

## 1. Covariate SMD between Units and Population:

#' @noRd
#' @param dfSU
#' @param unit_vars
#' @param exact_match_vars
#' @return ggplot object
unitSampBalance <- function(dfSU, unit_vars, exact_match_vars){

  # Get weighted SMD w/r to population for each covariate:

  unitPlotVars <- unique(c("unitSize", setdiff(unit_vars, exact_match_vars)))
  unitSMDs <- as.data.frame(unlist(lapply(unitPlotVars, function(x) getSMD(distinct_at(dfSU,
                                                              c("unit_ID", "Selected", unitPlotVars, "w")), x))))

  unitSMDs$unit_vars <- unique(c("unitSize", setdiff(unit_vars, exact_match_vars)))
  colnames(unitSMDs) <- c("SMD", "Covariate")
  unitSMDs$Covariate <- factor(unitSMDs$Covariate, levels=unique(c("unitSize", setdiff(unit_vars, exact_match_vars))))

  # Build balance table (SMD) (user output #3):
  unitSampBalTab <- select(unitSMDs, c("Covariate", "SMD"))

  # Build love plot (user output #4):
  unitSampLvPLt <- ggplot(unitSMDs, aes(x=SMD, y=Covariate)) +
    geom_dotplot(binaxis='y', fill="red", dotsize = 0.75) +
    ggtitle("Standardized Mean Difference: \n Selected Units vs. Population") +
    xlim(-0.5, 0.5) +
    geom_vline(xintercept=0) +
    # annotate("text", x = 0.5, y = 4.5, label = paste("N=", nUnitSamp, sep=""), size=2) +
    # annotate("text", x = 0.5, y = 4, label = paste("sizeFlag=", as.character(sizeFlag), sep=""), size=2) +
    theme_bw()

  return(list(unitSampBalTab, unitSampLvPLt))
}

## 2. Covariate SMD between Replacement (1,...,nth) and Initially selected (0) unit groups:

#' @noRd
#' @param mUnits
#' @param unitNumVars
#' @param nRepUnits
#' @return ggplot object
matchBalance <- function(mUnits, unitNumVars, nRepUnits){

    dfOrig <- as.data.frame(mUnits %>% filter(unitGrp == 0) %>%
                              select(c("unit_ID", "unitSize", all_of(unitNumVars))))
    dfOrig <- dplyr::summarise_at(dfOrig, c("unitSize", all_of(unitNumVars)), mean)

    dfOrigsd <- as.data.frame(mUnits %>% filter(unitGrp == 0) %>%
                                select(c("unit_ID", "unitSize", all_of(unitNumVars))))
    dfOrigsd <- dplyr::summarise_at(dfOrigsd, c("unitSize", all_of(unitNumVars)), sd)

    # Aggregate all potential units, grouped into replacement category i.e. original (0), 1,,,.n.
    dfU10g <- mUnits %>% dplyr::select(c("unit_ID","unitGrp",  "unitSize", all_of(unitNumVars))) %>%
      dplyr::group_by_at(c("unitGrp")) %>%
      dplyr::summarise_at(c("unitSize", unitNumVars), mean)

    # Get (non-weighted) SMD of each group against original:
    for(col in c("unitSize", all_of(unitNumVars))){
      dfU10g[,paste(col, " ", sep="")] <- (dfU10g[,col] - dfOrig[,col]) / dfOrigsd[,col]
    }

    dfU10gPlt <- dplyr::select(dfU10g, "unitGrp", c("unitGrp", unlist(lapply(colnames(dfU10g),
                                                                      function (x) {if(stringr::str_detect(x, " ")) {return(x)}}))))
    dfU10gPlt[,'All Covariates \n (average of the absolute SMDs)'] <- rowMeans(dataf.abs <- dfU10gPlt[,(2:ncol(dfU10gPlt))] %>%
                                                                        dplyr::select_if(is.numeric) %>%
                                                                         abs(), na.rm=T)

    dfU10gPlt <- reshape2::melt(dfU10gPlt, id.vars="unitGrp")


    # Line chart (user output #5):
    u10gPlt <- ggplot(dfU10gPlt, aes(x=unitGrp, y=value)) +
      geom_line(color="red")  +
      ggtitle("Replacement Units (1...K) vs. All Initially Sampled Units (SMD)") +
      geom_hline(yintercept = 0, style="dashed", color="grey") +
      ylab("Standardized Mean Difference") +
      xlab("Replacement Sample") +
      # annotate("text", x = 6, y = 0.8, label = paste("N=", nUnitSamp, sep=""), size=2) +
      # annotate("text", x = 6, y = 0.70, label = paste("sizeFlag=", as.character(sizeFlag), sep=""), size=2) +
      # annotate("text", x = 6, y = 0.60, label = paste("caliper(SD)=", as.character(calipValue), sep=""), size=2) +
      # annotate("text", x = 6, y = 0.50, label = paste("ReplaceMatches=", as.character(replaceFlag), sep=""), size=2) +
      scale_x_continuous(breaks=c(0:nRepUnits), limits=c(0, nRepUnits)) +
      theme_bw() +
      facet_wrap(facets=~variable)

    return(u10gPlt)
}

### 3. % Of successful matches per replacement group:

#' @noRd
#' @param replacementUnits
#' @param nRepUnits
#'@return ggplot object
matchCount <- function(replacementUnits, nRepUnits){

  completeUnitGrps <- t(as.data.frame(t(as.data.frame(1 - colMeans(is.na(replacementUnits)))*100)))
  completeUnitGrps <- as.data.frame(completeUnitGrps)
  completeUnitGrps$UnitGroup <- as.character(c(0:nRepUnits))
  colnames(completeUnitGrps) <- c("Perc_Matches", "UnitGroup")
  completeUnitGrps$UnitGroup <- factor(completeUnitGrps$UnitGroup, levels=as.character(c(0:nRepUnits)))

  # Bar chart (user output #6):
  u10barUnitPerc <- ggplot(completeUnitGrps, aes(x=UnitGroup, y=Perc_Matches)) +
      geom_col(color="gray", fill="salmon") +
      theme_bw() +
      # annotate("text", x = 8.5, y = 95, label = paste("N=", nUnitSamp, sep=""), size=2) +
      # annotate("text", x = 8.5, y = 88, label = paste("sizeFlag=", as.character(sizeFlag), sep=""), size=2) +
      # annotate("text", x = 8.5, y = 81, label = paste("caliper(SD)=", as.character(calipValue), sep=""), size=2) +
      # annotate("text", x = 8.5, y = 76, label = paste("ReplaceMatches=", as.character(replaceFlag), sep=""), size=2) +
      ggtitle("% of Successful Matches per Unit Group")

    return(u10barUnitPerc)
}


### 4. Covariate SMD between Sub-units and Population:

#' @noRd
#' @param df_
#' @param dfSU
#' @param mUnits
#' @param subUnitTable
#' @param subunitNumVars
#' @param RepUnits
#' @return ggplot object
subUnitBalance <- function(df_, dfSU, mUnits, subUnitTable, subunitNumVars, nRepUnits){
  selectedSubunits <- reshape2::melt(subUnitTable)
  colnames(selectedSubunits) <- c("ID", "subUnit_ID")

  dfsub <- stats::na.omit(dplyr::left_join(selectedSubunits, dfSU, by = "subUnit_ID"))
  dfsub <- dplyr::distinct(dplyr::inner_join(dfsub, select(mUnits, c("unit_ID", "unitGrp")), by = "unit_ID"))


  dfPOP <- as.data.frame(df_ %>%  dplyr::select(c("unit_ID", all_of(subunitNumVars))))
  dfPOP <- dplyr::summarise_at(dfPOP, subunitNumVars, mean)

  dfPOPsd <- as.data.frame(df_ %>%  dplyr::select(c("unit_ID", "unitSize", all_of(subunitNumVars))))
  dfPOPsd <- dplyr::summarise_at(dfPOPsd, subunitNumVars, sd)

  dfSub10g <- dfsub %>% select(c("unit_ID","unitGrp",  "unitSize", all_of(subunitNumVars))) %>%
    dplyr::group_by_at(c("unitGrp")) %>%
    dplyr::summarise_at(subunitNumVars, mean)

  for(col in subunitNumVars){
    dfSub10g[,paste(col, " ", sep="")] <- (dfSub10g[,col] - dfPOP[,col]) / dfPOPsd[,col]
  }

  dfSub10gPlt <- select(dfSub10g, "unitGrp", c("unitGrp", unlist(lapply(colnames(dfSub10g),
                                                     function (x) {if(stringr::str_detect(x, " ")) {return(x)}}))))
  dfSub10gPlt[,'All Covariates \n (average of the absolute SMDs)'] <- rowMeans(dataf.abs <- dfSub10gPlt[,(2:ncol(dfSub10gPlt))] %>%
                                                     dplyr::select_if(is.numeric) %>%
                                                                                 abs(), na.rm=T)

  dfSub10gPlt <- reshape2::melt(dfSub10gPlt, id.vars="unitGrp")

  # Line chart (user output #7):
  sub10gPlt <- ggplot(dfSub10gPlt, aes(x=unitGrp, y=value)) +
    geom_line(color="blue")  +
    ggtitle("Subunits from Original and Replacement Units vs. Population (SMD)") +
    geom_hline(yintercept = 0, style="dashed", color="grey") +
    ylab("Standardized Mean Difference") +
    xlab("Replacement Sample") +
    # annotate("text", x = 6, y = 1.25, label = paste("N=", nUnitSamp, sep=""), size=2) +
    # annotate("text", x = 6, y = 0.85, label = paste("sizeFlag=", as.character(sizeFlag), sep=""), size=2) +
    # annotate("text", x = 6, y = 0.55, label = paste("caliper(SD)=", as.character(calipValue), sep=""), size=2) +
    # annotate("text", x = 6, y = 0.35, label = paste("ReplaceMatches=", as.character(replaceFlag), sep=""), size=2) +
    scale_x_continuous(breaks=c(0:nRepUnits), limits=c(0, nRepUnits)) +
    theme_bw() +
    facet_wrap(facets=~variable)

  return(sub10gPlt)
}
