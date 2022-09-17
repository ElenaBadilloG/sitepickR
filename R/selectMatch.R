
##########################################################################################################
####################### selectMatch() [sitepickR Package - Testing Version]  ######################
################### Robert Olsen, Elizabeth A. Stuart & Elena Badillo-Goicoechea, August 2022 ###############
##########################################################################################################

#' @importFrom magrittr "%>%"

# Helper 1. Obtain best unit matches

#' @noRd
#' @param dfSU dataframe
#' @param sizeFlag character; unit column name
#' @param unitVars character; unit column name
#' @param exactMatchVars character; unit column name
#' @param calipMatchVars character; unit column name
#' @param calipValue character; unit column name
#' @param matchDistance character; unit column name
#' @param nRepUnits character; unit column name
#' @param replaceFlag character; unit column name
#' @param calipers character; unit column name
#' @return list: matches matrix, df used to run match
getMatches <- function(dfSU, sizeFlag, unitVars, exactMatchVars,
                       calipMatchVars, calipValue, matchDistance, nRepUnits, 
                       replaceFlag, calipers){
  
  # Prepare dataset for matching:
  if(sizeFlag==TRUE){
    
    unitVars <- c("unitSize", tidyselect::all_of(unitVars))
    calipMatchVars <- c("unitSize", tidyselect::all_of(calipMatchVars))}
  
  calipers = rep(calipValue, length(calipMatchVars))
  names(calipers) <- calipMatchVars
  
  if(!is.null(exactMatchVars)){
    
    dfMatch <- dfSU %>% 
              dplyr::distinct() %>% 
              dplyr::select(c("unitID", "unitSize", "Selected",
                                                tidyselect::all_of(c(unitVars, exactMatchVars)))) %>% 
              dplyr::group_by_at(c("unitID", "Selected", tidyselect::all_of(exactMatchVars))) %>%
              dplyr::summarise_at(c("unitSize", tidyselect::all_of(dplyr::setdiff(unitVars, exactMatchVars))),
                                  mean)
       
  } else{
    
    dfMatch <- dfSU %>% 
               dplyr::distinct() %>% 
               dplyr::select(c("unitID", "unitSize", "Selected",
                                                tidyselect::all_of(unitVars))) %>% 
                dplyr::group_by_at(c("unitID", "Selected")) %>%
                dplyr::summarise_at(c("unitSize", unitVars), mean)
       }
  
  
  # Find matches for each unit (casewise and relaxing calliper when needed):
  
  ## Case 1: No callipers:
  
  if(is.null(calipers)) {
    
    unitMatch <- MatchIt::matchit(as.formula(paste("Selected ~ ", paste(unitVars, collapse= "+"))),
                         data = dfMatch,
                         distance = matchDistance,
                         ratio = nRepUnits,
                         replace = replaceFlag)
  } else {
    
    ## Case 2: Callipers  & repeating units after matching is allowed:
    
    if(!is.null(calipers) & replaceFlag == TRUE){ # Match with calipers (case NULL/numeric vector):
      
      #Do matching with replacement with the caliper:
      m1 <- MatchIt::matchit(as.formula(paste("Selected ~ ", paste(unitVars, collapse= "+"))),
                    data = dfMatch,
                    distance = matchDistance,
                    ratio = nRepUnits,
                    replace = replaceFlag,
                    caliper = calipers,
                    std.caliper = rep(TRUE,length(calipers)))
      
      #Second round of matching without a caliper:
      m2 <- MatchIt::matchit(as.formula(paste("Selected ~ ", paste(unitVars, collapse= "+"))),
                    data = dfMatch,
                    distance = matchDistance,
                    ratio = nRepUnits,
                    replace = replaceFlag)
      
      #For each treated unit, fill in match matrix with matches from
      #match without caliper to get 10 matches total; avoid repeating matches:
      
      for (i in rownames(m1$match.matrix)) {
        #Which of the 10 requested matches were not found
        nas <- is.na(m1$match.matrix[i,])
        if (any(nas)) {
          m1$match.matrix[i, nas] <- dplyr::setdiff(m2$match.matrix[i,],
                                             m1$match.matrix[i, !nas])[1:sum(nas)]
        }
      }
      
      # Re-compute the weights using the updates match.matrix
      m1$weights <- MatchIt:::weights.matrix(m1$match.matrix, m1$treat)
      
      unitMatch <- m1} else {
        
        ## Case 3: Callipers & repeating units after matching is not allowed:
        if(!is.null(calipers) & replaceFlag==FALSE) {
          
          #Do matching with replacement with the caliper:
          m1 <- MatchIt::matchit(as.formula(paste("Selected ~ ", paste(unitVars, collapse= "+"))),
                        data = dfMatch,
                        distance = matchDistance,
                        ratio = nRepUnits,
                        replace = replaceFlag,
                        caliper = calipers,
                        std.caliper = rep(TRUE,length(calipers)))
          
          #Second round of matching without a caliper on unmatched units
          m2 <- MatchIt::matchit(as.formula(paste("Selected ~ ", paste(unitVars, collapse= "+"))),
                        data = dfMatch,
                        distance = matchDistance,
                        ratio = nRepUnits,
                        replace = replaceFlag)
          #discard = m1$treat == 0 & m1$weights > 0) # Unnecessary restriction?
          
          #For each treated unit, fill in match matrix with matches from
          #match without caliper to get 10 matches total
          for (i in rownames(m1$match.matrix)) {
            #Which of the 10 requested matches were not found
            nas <- is.na(m1$match.matrix[i,])
            if (any(nas)) {
              m1$match.matrix[i, nas] <- m2$match.matrix[i,1:sum(nas)]
            }
          }
          
          #Re-compute weights and subclasses from new match.matrix
          m1$weights <- MatchIt:::weights.matrix(m1$match.matrix, m1$treat)
          m1$subclass <- MatchIt:::mm2subclass(m1$match.matrix, m1$treat)
          
          unitMatch <- m1} 
        
      }
  }
  
  return(list(unitMatch, dfMatch))
}


# Helper 2. Recover a unit ID from its assiged unit index after matching

#' @param idxCol integer; index of a given unit in output matrix after matching
#' @param units dataframe; unit level dataframe with all unit level covariates  relevant for selection and matching.
#' @return vector of original IDs
getUnitID <- function(idxCol, units){
  res=c()
  for(i1 in 1:length(idxCol)){
    uiD = NA
    for(i2 in 1:nrow(units)){
      if(!is.na(idxCol[i1]) & !is.na(units$selectedunitIDx[i2]))
      {
        if (idxCol[i1] == units$selectedunitIDx[i2]){
          uiD <- units$unitID[i2]
        }
        res[i1] <- uiD} else{res[i1] <- NA}
    }
  }
  return(res)
}


#' Initial Unit Selection
#' @noRd
#' @param df_
#' @param unitVars
#' @param exactMatchVars
#' @param nUnitSamp
#' @param sizeFlag
#' @return dataframe with selection status and selection probabilities
sampleUnits <- function(df_, unitVars, exactMatchVars, nUnitSamp,  sizeFlag){
  # Select units (1 = Selected, 0 = Non selected) via nested cube sampling:

  if(sizeFlag == TRUE) {SEL = 1} else {SEL = 2}

  dfSampledU  <- as.data.frame(sampling::balancedcluster(df_[,dplyr::setdiff(c("unitSize", unitVars),
                                                                      exactMatchVars)],
                                                         m=nUnitSamp,
                                                         cluster=df_$unitID,
                                                         selection=SEL,
                                                         comment=FALSE,
                                                         method=SEL))
  dfSampledU$unitID <- df_$unitID

  dfSampledU <- dfSampledU %>%
    dplyr::mutate(Selected = V1,
           InclusionProb = V2)  %>%
    dplyr::select(unitID, Selected, InclusionProb)

  dfSU <- suppressMessages(dplyr::right_join(dplyr::distinct(dfSampledU), df_, by="unitID"))
  


  return(dfSU)
}



#' @noRd
#' @param subUnitLookup
#' @param replacementUnits
#' @param subunitSampVars
#' @param nsubUnits 
#' @return subunit lookup table
sampleSubUnits <- function(df_, subUnitLookup, replacementUnits, subunitSampVars, nsubUnits){

  subUnitTable <- dplyr::distinct(dplyr::select(reshape2::melt(replacementUnits,
                                                        measure.vars=colnames(replacementUnits)), c("value")))
  subUnitTable <- dplyr::filter(subUnitTable, !is.na(value))
  subUnitTable$sub_units = NA
  colnames(subUnitTable) <- c("unitID", "sub_units")
  subUnitTable <- dplyr::distinct(subUnitTable)

  # Sample sub-units for each potential unit:
  for(i in 1:nrow(subUnitTable)){

    un_ <- subUnitTable$unitID[i]
    df_IDs <- dplyr::filter(subUnitLookup, unitID==un_)

    if(nrow(df_IDs) <= nsubUnits) {subUnitTable$sub_units[i] <- list(df_IDs$subunitID)} else {

      df_ID <- dplyr::filter(df_, unitID==un_)

      PIK=rep(nsubUnits/nrow(df_ID), times=nrow(df_ID))
      s=sampling::samplecube(as.matrix(df_ID[, subunitSampVars]),
                             pik=PIK,
                             order=1,
                             comment=F)
      subUnitTable$sub_units[i] <- list(df_IDs[(1:length(PIK))[s==1],]$subunitID)

    }
  }

  return(subUnitTable)
}


#  MAIN FUNCTION: selectMatch()

#' Two-level sample selection
#' 
#'Carries out a two-level sample selection where the possibility of an initially selected
#'site not wanting to participate is anticipated, and the site is optimally replaced. 
#'The procedure aims to reduce the bias (and/or loss of generalizability) with respect to the target population.
#' @export
#' @param df dataframe; sub-unit level dataframe with both sub-unit and unit level variables
#' @param unitID character; name of unit ID column
#' @param subunitID character; name of sub-unit ID column
#' @param unitVars vector; column names of unit level variables  to match units on
#' @param nUnitSamp numeric; number of units to be initially randomly selected
#' @param nRepUnits numeric; number of replacement units to find for each selected unit
#' @param nsubUnits numeric; number of sub-units to be randomly selected for each unit
#' @param subunitSampVars vector;  column names of unit level variables  to sample units on
#' @param calipValue numeric; number of standard deviations to be used as caliper for matching units on calipMatchVars
#' @param seedN numeric; seed number to be used for sampling. If NA, calls set.seed(); default = NA
#' @param exactMatchVars vector; column names of categorical variables on which units must be matched exactly. Must be present in 'unitVars'; default = NULL
#' @param calipMatchVars vector; column names of continuous variables on which units must be matched within a specified caliper. Must be present in 'unitVars'; default = NULL
#' @param matchDistance character; MatchIt::matchit distance parameter to obtain optimal matches (nearest neigboors); default = "mahalanois"
#' @param sizeFlag logical; if TRUE, sampling is made proportional to unit size; default = TRUE
#' @param replaceFlag logical; if TRUE, get matches with replacement; default = TRUE
#' @param writeOut logical; if TRUE, writes a .csv file for each output table; default = TRUE
#' @param replacementUnitsFilename character; csv filename for saving {unit:replacement} directory when writeOut == TRUE; default = "replacementUnits.csv"
#' @param subUnitTableFilename character; csv filename for saving {unit:replacement} directory when writeOut == TRUE; default = "subUnitTable.csv"
#' @return list with: 1) table of the form: {selected unit i: (unit i replacements)}, 2) table of the form: {potential unit i:(unit i sub-units)}, 3) balance diagnostics.
selectMatch <- function(df,
                        unitID,
                        subunitID,
                        subunitSampVars,
                        unitVars,
                        nUnitSamp,
                        nRepUnits,
                        nsubUnits,
                        exactMatchVars=NULL,
                        calipMatchVars=NULL,
                        calipValue = 0.2,
                        seedN = NA,
                        matchDistance = "mahalanobis",
                        sizeFlag = TRUE,
                        replaceFlag = TRUE,
                        writeOut = TRUE,
                        replacementUnitsFilename = "replacementUnits.csv",
                        subUnitTableFilename = "subUnitTable.csv"

)
{

  if(!is.na(seedN)) {
    set.seed(seedN)}


  ### 1. INITIAL UNIT SELECTION: Select units (1 = Selected, 0 = Non selected) via nested cube sampling

  dfSU <- sampleUnits(df, unitVars, exactMatchVars, nUnitSamp,  sizeFlag)

  # calculate appropriate weights for unit balance diagnstics :
  if(sizeFlag == TRUE){dfSU$w <- 1 / dfSU$InclusionProb} else {dfSU$w <- 1}

  # Create a subunit lookup table of the form: {unit U:[all U sub_units]}:
  units <- dplyr::distinct(dplyr::select(dfSU, "unitID"))
  units$selectedunitIDx <- rownames(units)

  subUnitLookup <- dplyr::distinct(suppressMessages(dplyr::inner_join(units,
                            dplyr::select(df, c("unitID", "subunitID")))))
  subUnitLookup <- dplyr::distinct(subUnitLookup)


  ### 2. FIND BEST MATCHES FOR ALL INITIALLY SELECTED UNITS

  rmatches <- getMatches(dfSU, sizeFlag, unitVars, exactMatchVars,
                          calipMatchVars, calipValue, matchDistance, nRepUnits, replaceFlag, calipers)

  unitMatch <- rmatches[[1]]
  dfMatch <- rmatches[[2]]

  # Build selected unit/replacements {unit U:[U best unit matches list]} directory (user output #1):

  replacementUnits <- as.data.frame(unitMatch[[1]])
  replacementUnits <- dplyr::distinct(replacementUnits)
  replacementUnits$selectedunitIDx <- rownames(replacementUnits)
  colnames(replacementUnits)[1:nRepUnits] <- unlist(lapply(colnames(replacementUnits)[1:nRepUnits] ,
                                                           function (x) stringr::str_replace(x, "V", "Unit_replacement_")))
  replacementUnits <- dplyr::select(replacementUnits, c(selectedunitIDx, colnames(replacementUnits)[1:nRepUnits]))


  # Re-map original unit ID names for unit row indeces:
  for(col in colnames(replacementUnits)){
    replacementUnits[,col] = getUnitID(replacementUnits[,col], units)
  }
  replacement_unit_cols <- colnames(replacementUnits)[2:ncol(replacementUnits)]
  colnames(replacementUnits) <- c("unitID", replacement_unit_cols)


  ### 3. SELECT SUB-UNITS FOR SELECTED / REPLACEMENT UNITS:

  subUnitTable <- sampleSubUnits(df, subUnitLookup, replacementUnits, subunitSampVars, nsubUnits)

  # Build  directory of the form: {potential unit U:[U sub-unit list]} (user output #2):

  subUnitTable <- subUnitTable %>%
    tidyr::unnest(sub_units) %>%
    dplyr::group_by_at(c("unitID")) %>%
    dplyr::mutate(key = dplyr::row_number()) %>%
    tidyr::spread(key, sub_units)

  colnames(subUnitTable) <- c("unitID", sapply(colnames(subUnitTable)[2:ncol(subUnitTable)],
                                                function (x) paste("Sub_unit", x, "_ID", "")))
  subUnitTable <- subUnitTable[,c(1:(nsubUnits+1))]


  ### 4. BALANCE DIAGNOSTICS

  #1. Covariate SMD between Units and Population:

  unitBal <- unitSampBalance(dfSU, unitVars, exactMatchVars)

  unitSampBalTab <- unitBal[[1]]
  unitSampBalance_ <- unitBal[[2]]

  #2.Covariate SMD between Replacement (1,...,nth) and Initially selected (0) unit groups:

  unitNumVars <- tidyselect::all_of(dplyr::setdiff(unitVars,
                                exactMatchVars))
  
  # Recover unit groups from MatchIt::matchit Output:

  matches <- MatchIt::get_matches(unitMatch,
                         distance = "distance",
                         weights = "weights",
                         subclass = "subclass",
                         id = "id",
                         data = dfMatch,
                         include.s.weights = TRUE)

  mUnits <- dplyr::inner_join(dfMatch, dplyr::select(matches, c("unitID", "subclass", "weights")), by ="unitID")

  mUnits$unitGrp <- NA
  for(i in 1:nrow(mUnits)){
    if(is.na(mUnits$unitGrp[i])){
      for(j in 0:nRepUnits){
        if(mUnits$unitID[i] %in% replacementUnits[,(j+1)]){
          mUnits$unitGrp[i] = j}

      }} else{next}
  }


  # Calculate difference between each unit replacement group (1,..., n) and initially selected units:
  
  matchBalance_ <- matchBalance(mUnits, unitNumVars, nRepUnits)


  ### 3. % Of successful matches per replacement group:

  matchCount_ <- matchCount(replacementUnits, nRepUnits)

  ### 4. Covariate SMD between Sub-units and Population:
  
  if(!is.null(exactMatchVars)){
    subunitNumVars <- c("unitSize", tidyselect::all_of(dplyr::setdiff(c(unitVars, subunitSampVars),
                                                 exactMatchVars)))} else{
    subunitNumVars <- c("unitSize", tidyselect::all_of(c(unitVars, subunitSampVars))) }

  subUnitBalance_ <- subUnitBalance(df, dfSU, mUnits, subUnitTable, subunitNumVars, nRepUnits)

  # 5. PREPARE OUTPUT

    # Write csv files:
    if(writeOut==TRUE){

      # unit:{replacements} directory (user output):
      write.csv(replacementUnits, replacementUnitsFilename)

      # unit:{subunits} directory (user output):
      write.csv(subUnitTable,  subUnitTableFilename)

    }

  # Output objects (7) into list:
  mainRes = list(replacementUnits, subUnitTable, # {selected unit: unit replacements} & {unit:subunits} lookup tables
                 unitSampBalTab, unitSampBalance_, # balance table & love plot for selected units vs. population
                 matchBalance_, # SMD for unit groups vs. initial units (table and figure)
                 matchCount_, # barchart with % of successful matches per unit group
                 subUnitBalance_ # SMD line charts for subunits from each unit groups vs. population

                )

  return(mainRes)
}












