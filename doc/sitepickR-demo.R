## ---- include = FALSE---------------------------------------------------------
options(rmarkdown.html_vignette.check_title = FALSE)
knitr::opts_chunk$set(warning = FALSE, message = FALSE, fig.width=8, fig.height=5) 
knitr::opts_chunk$set(
  collapse = FALSE,
  comment = "#>"
)

## -----------------------------------------------------------------------------
if(!require(devtools)){
    install.packages("devtools", repos = "http://cran.us.r-project.org")
}

if(!require(sitepickR)){
    devtools::install_github("ElenaBadilloG/sitepickR")
    }

## -----------------------------------------------------------------------------
library(sitepickR)

## ---- results='asis'----------------------------------------------------------
rawCCD <- sitepickR::rawCCD
knitr::kable(head(rawCCD), format = "html")

## ---- results='asis'----------------------------------------------------------
dfCCD <- prepDF(rawCCD,
                 unitID="LEAID", subunitID="NCESSCH")
knitr::kable(dfCCD[1:10,(ncol(dfCCD)-5-1):ncol(dfCCD)], format = "html")

## -----------------------------------------------------------------------------
seed <- 1122 

## -----------------------------------------------------------------------------
calip <- 0.2 # maximum standard deviations of covariate distance around target population

## -----------------------------------------------------------------------------
Nu <- 100 # district sample size
K <- 10 # number of matches per district
Ns <- 5 # number of schools per potential district

## -----------------------------------------------------------------------------
uSampVarsCCD <- c("w.pct.frlunch", "w.pct.black", "w.pct.hisp", "w.pct.female") 

## -----------------------------------------------------------------------------
suSampVarsCCD <- c("sch.pct.frlunch", "sch.pct.black", "sch.pct.hisp", "sch.pct.female")

## -----------------------------------------------------------------------------
exactMatchVars <-  c("distr.type")

## -----------------------------------------------------------------------------
calipMatchVars <-  c("w.pct.black", "w.pct.hisp", "w.pct.female")

## -----------------------------------------------------------------------------
smOut <- selectMatch(df = dfCCD, # user dataset
                       unitID = "LEAID", # column name of unit ID in user dataset
                       subunitID = "NCESSCH", # column name of sub-unit ID in user dataset
                       unitVars = uSampVarsCCD, # name of unit level covariate columns
                       subunitSampVars = suSampVarsCCD, # name of sub-unit level covariate columns
                       exactMatchVars = exactMatchVars, # unit level categorical covariates on which to match exactly
                       calipMatchVars = calipMatchVars, # unit level numeric covariates on which to match within a radius
                       nUnitSamp = Nu, # original unit sample size
                       nRepUnits = K, # number of desired matches per initially selected unit
                       nsubUnits = Ns, # number of sub-units to sample from each candidate unit
                       calipValue = calip, # maximum distance on which to match specified unit level covariates (calipMatchVars)
                       seedN = seed, # random seed number
                       matchDistance = "mahalanobis", # metric used for matching units
                       sizeFlag = TRUE,
                       repFlag = FALSE, # pick matches without repetition
                       writeOut = FALSE, # write out a csv file for: 1) matched units and 2) selected sub-units
                       replacementUnitsFilename = "replacementsTable.csv", # filename for {districtA: [districtA replacement list]} table
                       subUnitTableFilename = "schoolsDirectory.csv" # filename for {districtA: [districtA schools]} table
)

## ---- results='asis'----------------------------------------------------------
districtReplTable <- smOut[[1]]
knitr::kable(head(districtReplTable), format = "html")

## ---- results='asis'----------------------------------------------------------
schoolDirectory <- smOut[[2]]
knitr::kable(head(dplyr::filter(schoolDirectory, !is.na(Sub_unit5_ID))), format = "html")


## ----fig.width=6, fig.height=5------------------------------------------------
unitLovePlot(smOut,
   title = "Standardized Mean Difference: \n Initially Selected Units vs. Population")

## ---- results='asis'----------------------------------------------------------
unitBalanceTab <- getSummary(smOut, diagnostic="unitBal")

knitr::kable(head(unitBalanceTab, 10), format = "html")

## ----fig.width=9, fig.height=6------------------------------------------------
matchBalance(smOut, 
  title = "Standardized Mean Difference: \
  Replacement Unit Groups (1...K) vs. Originally Selected Units")

## ---- results='asis'----------------------------------------------------------
matchBalanceTab <- getSummary(smOut, diagnostic="matchBal")
knitr::kable(head(matchBalanceTab, 20), format = "html")

## ----fig.width=7, fig.height=6------------------------------------------------
matchFreq(smOut,
             title="Match Frequency per Original Unit")

## ----fig.width=7, fig.height=6------------------------------------------------
matchCount(smOut,
             title="% of Successful Matches per Unit Group")

## ---- results='asis'----------------------------------------------------------
matchFreqTab <- getSummary(smOut, diagnostic="matchFreq")

knitr::kable(matchFreqTab, format = "html")

## ---- results='asis'----------------------------------------------------------
matchCountTab <- getSummary(smOut, diagnostic="matchCount")

knitr::kable(matchCountTab, format = "html")

## ----fig.width=9, fig.height=6------------------------------------------------
subUnitBalance(smOut,
     title="Standardized Mean Difference: \n Sub-units from Original + Replacement Unit Groups vs. Population")

## ---- results='asis'----------------------------------------------------------
subUnitBalanceTab <- getSummary(smOut, diagnostic="subunitBal")
knitr::kable(subUnitBalanceTab, format = "html")

