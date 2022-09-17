
# data-raw/rawCCD.R
# Data import and processing pipeline

# Read pre-processed data:
rawCCD <- read.csv("data-raw/rawdfCAL.csv")

# Add a toy unit level categorical, "district type":
dfG <- rawCCD %>% dplyr::group_by(LEAID) %>%
  dplyr::summarise(unitSize = dplyr::n())
dummy_dtrct_types <- c("A", "B", "C", "D")
dfG$distr.type <- sample(dummy_dtrct_types,
                         size = length(unique(rawCCD$LEAID)),
                         prob = rep(1/length(dummy_dtrct_types), length(dummy_dtrct_types)), replace = TRUE)

rawCCD <- suppressMessages(dplyr::left_join(rawCCD, dfG))[,-1]

usethis::use_data(rawCCD, rawCCD, overwrite = TRUE)

