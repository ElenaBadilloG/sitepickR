## ---- include = FALSE---------------------------------------------------------
options(rmarkdown.html_vignette.check_title = FALSE) 
knitr::opts_chunk$set(warning = FALSE, message = FALSE, fig.width=7, fig.height=5) 
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)

## -----------------------------------------------------------------------------

library(sitepickR)

## -----------------------------------------------------------------------------
dfCCD <- sitepickR::dfCCD
head(dfCCD)

## -----------------------------------------------------------------------------
PATH = "SOME-USER-FOLDER/" 
replDISTRICTS = paste0(PATH, "/replacementUnits.csv") # {initial district:[replacements]} directory
dictSCHOOLS = paste0(PATH, "/subUnitTable.csv") # {district: [schools]} directory

## -----------------------------------------------------------------------------
seed = 1234 

## -----------------------------------------------------------------------------
unitSamp_varsCCD <- c("w.pct.frlunch", "w.pct.black", "w.pct.hisp", "w.pct.female") 

## -----------------------------------------------------------------------------
subUnitSamp_varsCCD <- c("sch.pct.frlunch", "sch.pct.black", "sch.pct.hisp", "sch.pct.female")

## -----------------------------------------------------------------------------
exact_match_vars <-  c("distr.type")

## -----------------------------------------------------------------------------
calip_match_vars <-  c("w.pct.black", "w.pct.hisp", "w.pct.female")

## -----------------------------------------------------------------------------

m.out <- selectMatch(df=dfCCD, # dataset
                       unit_ID="LEAID", # column name of district ID
                       subUnit_ID="NCESSCH", # column name of school ID
                       unit_vars=unitSamp_varsCCD,
                       subUnit_samp_vars=subUnitSamp_varsCCD,
                       exact_match_vars=NULL,
                       calip_match_vars=calip_match_vars , 
                       nUnitSamp = 100, # original district sample size
                       nRepUnits = 10, # number of desired matches per selected district
                       nsubUnits = 5, # number of schools to sample from each candidate district
                       calipValue = 0.2, # 
                       seedN = seed,
                       matchDistance = "mahalanobis",
                       sizeFlag = TRUE,
                       replaceFlag = FALSE, #match without replacement
                       writeOut = FALSE, # write out a csv file for: 1) matched units and 2) selected schools
                       replacementUnitsFilename = replDISTRICTS,
                       subUnitTableFilename = dictSCHOOLS
)

## ----fig.width=5, fig.height=5------------------------------------------------
getUnitLovePlot(m.out)

## -----------------------------------------------------------------------------
getUnitReplacementBalance(m.out)

## -----------------------------------------------------------------------------
getMatchCount(m.out)

## -----------------------------------------------------------------------------
getSubUnitBalance(m.out)

