
# data-raw/dfCCD.R
# Data import and processing pipeline

# Read pre-processed data:
dfCCD <- read.csv("rawdfCAL.csv")

# Add a toy unit level categorical, "district type":
dfG <- dfCCD  %>% dplyr::group_by(LEAID) %>%
  summarise(unitSize = n())
dummy_dtrct_types <- c("A", "B", "C", "D")
dfG$distr.type <- sample(dummy_dtrct_types,
                         size = length(unique(dfCCD$LEAID)), 
                         prob = rep(1/length(dummy_dtrct_types), length(dummy_dtrct_types)), replace = TRUE)

dfCCD <- suppressMessages(dplyr::left_join(dfCCD, dfG))[,-1]

usethis::use_data(dfCCD, dfCCD, overwrite = TRUE)

