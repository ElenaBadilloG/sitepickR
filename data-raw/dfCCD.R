
# data-raw/dfCCD.R
# Data import and processing pipeline

#' Common Core of Data (CCD) data for California schools (2017-18).
#'
#' A pre-processed dataset containing key variables from administrative data compiled by the CCD, aggregated at 
#' the district and school level for public schools in California for the 2017 and 2018 school years.
#'
#' @format A data frame with 1890 rows and 13 variables:
#' \describe{
#'   \item{LEAID}{school district unique identifier}
#'   \item{NCESSCH}{school unique identifier}
#'   \item{w.pct.frlunch}{percentage of students in the school district who are under free/reduced price lunch program; weighted by school size.}
#'   \item{w.pct.black}{percentage of students in the school district who are Black; weighted by school size.}
#'   \item{w.pct.hisp}{percentage of students in the school district who are Hispanic; weighted by school size.}
#'   \item{w.pct.female}{percentage of students in the school district who are female; weighted by school size.}
#'   \item{sch.pct.frlunch}{percentage of students in the school who are under free/reduced price lunch program.}
#'   \item{sch.pct.black}{percentage of students in the school who are Black.}
#'   \item{sch.pct.hisp}{percentage of students in the school who are Hispanic.}
#'   \item{sch.pct.female}{percentage of students in the school who are female.}
#' }
#' @source \url{https://nces.ed.gov/ccd//}


# Read pre-processed data:
dfCCD <- read.csv("rawdfCAL.csv")

# Add a toy unit level categorical:
dfG <- dfCCD  %>% dplyr::group_by(LEAID) %>%
  summarise(unitSize = n())

dfG$distr.type <- sample(c("A", "B", "C", "D"),
                         size = length(unique(dfCCD$LEAID)), 
                         prob = c(0.25, 0.25, 0.25, 0.25), replace = TRUE)

dfCCD <- suppressMessages(dplyr::left_join(dfCCD, dfG))[,-1]


usethis::use_data(dfCCD, dfCCD, overwrite = TRUE)

