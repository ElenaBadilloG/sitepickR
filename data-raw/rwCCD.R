
# data-raw/rwCCD.R
# Data import and processing pipeline

# Read pre-processed data:
rwCCD <- read.csv("data-raw/rawdfCAL.csv")

# Add a toy unit level categorical, "district type":
dfG <- rwCCD %>% dplyr::group_by(LEAID) %>%
  summarise(unitSize = n())
dummy_dtrct_types <- c("A", "B", "C", "D")
dfG$distr.type <- sample(dummy_dtrct_types,
                         size = length(unique(rwCCD$LEAID)),
                         prob = rep(1/length(dummy_dtrct_types), length(dummy_dtrct_types)), replace = TRUE)

rwCCD <- suppressMessages(dplyr::left_join(rwCCD, dfG))[,-1]

usethis::use_data(rwCCD, overwrite = TRUE)

