#
## Study 1
#

# load libraries
library(tidyverse)

# read in data
file_path <- "./private-data/study-1-data.csv"
unfiltered_raw_df <- read.csv(file_path, na.strings = "")
raw_df <- unfiltered_raw_df[c(4:nrow(unfiltered_raw_df)), ]
raw_df$BUPAS_ID <- trimws(raw_df$BUPAS_ID)

# join with psychology pool data
source("./scripts/bellevue-participant-pool.R")

joined_df <- BU_psych_df %>%
  select(MTurkCode, Sex, PoliParty, conservatism, 
         PoliSoc, PoliEcon, PoliFore, MFQharm,
         MFQfairness, MFQloyalty, MFQauthority, MFQpurity,
         Age_1, just_world_belief, Ethnicity) %>%
  mutate_at(.vars = "MTurkCode", .funs = as.character) %>%
  # mutate_at(.vars = "MTurkCode", .funs = trimws) %>%
  rename(BUPAS_ID = "MTurkCode") %>%
  filter(!is.na(BUPAS_ID)) %>%
  right_join(raw_df, by="BUPAS_ID", na_matches = "never")

# pre-process data
moral_df <- joined_df %>%
  mutate(
    across(
      .cols = starts_with("Q", ignore.case = FALSE),
      .fns = list(~recode(.x,
                          "Moral" = -1,
                          "Immoral" = 1,
                          "Not related to morality" = 0)),
      .names = "{.col}_r"
    )
  ) %>%
  # calculate composite variables
  mutate(
    totalMoral = rowSums(
      select(., starts_with("Q", ignore.case = FALSE)) == "Moral", 
      na.rm = TRUE),
    totalImmoral = rowSums(
      select(., starts_with("Q", ignore.case = FALSE)) == "Immoral", 
      na.rm = TRUE),
    totalAmoral = rowSums(
      select(., starts_with("Q", ignore.case = FALSE)) == "Not related to morality", 
      na.rm = TRUE),
    moralFilter = totalMoral + totalImmoral)

# save data as csv
write_csv(moral_df, "./public-data/study-1-public-data.csv")
