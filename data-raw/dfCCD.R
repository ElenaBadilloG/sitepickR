
# data-raw/dfCCD.R
# Data import and processing pipeline

library(magrittr)


# Read pre-processed data:
dfCCD <- read.csv("rawdfCAL.csv")

# Add a toy unit level categorical:
dfG <- dfCCD  %>% dplyr::group_by(LEAID) %>%
  summarise(unitSize = n())

dfG$distr.type <- sample(c("A", "B", "C", "D"),
                         size = length(unique(dfCCD$LEAID)), 
                         prob = c(0.25, 0.25, 0.25, 0.25), replace = TRUE)

dfCCD <- suppressMessages(dplyr::left_join(dfCCD, dfG))

# Data cleaning code here...
# (Do NOT put data analysis code here!)

# This should be the last line.
# Note that names are unquoted.
# I like using overwrite = T so everytime I run the script the 
# updated objects are saved, but the default is overwrite = F
usethis::use_data(dfCCD, dfCCD, overwrite = TRUE)

