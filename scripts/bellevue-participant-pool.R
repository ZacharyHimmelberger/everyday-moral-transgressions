## Process Data for Bellevue University Psychology Subject Pool

# load libraries
library(tidyverse)

# file path
file_path <- "./private-data/bellevue-participant-pool-data.csv"

# read in data
# skip first two rows after header
psych_pool_df_col_names <- read_csv(file_path, 
                                    n_max = 0, 
                                    show_col_types = FALSE) %>% 
  names()

psych_pool_df <- read_csv(file_path, 
                          col_names = psych_pool_df_col_names, 
                          skip = 3, 
                          show_col_types = FALSE)

# pre-process data
BU_psych_df <- psych_pool_df %>%
  # recode sex (NOT NEEDED)
  mutate_at(.vars = c("Sex"), 
            .funs = recode,
            "0" = "male",
            "1" = "female",
            "2" = "both",
            "3" = "neither",
            "4" = "preferNotToSay") %>%
  # recode political party (NOT NEEDED)
  mutate_at(.vars = c("PoliParty"), 
            .funs = recode,
            "1" = "republican",
            "2" = "democrat",
            "3" = "libertarian",
            "4" = "green",
            "5" = "independent",
            "6" = "constitution",
            "7" = "other") %>%
  # make moral scenario column a factor
  mutate_at(.vars = c("Sex", "PoliParty"), .funs = as.factor) %>%
  # create mean conservative column
  mutate(
    across(
      .cols = c(PoliEcon, PoliSoc, PoliFore),
      .fns = list(~recode(.x,
                          "Very Conservative" = 7,
                          "Conservative" = 6,
                          "Somewhat Conservative" = 5,
                          "Moderate" = 4,
                          "Somewhat Liberal" = 3,
                          "Liberal" = 2,
                          "Very Liberal" = 1)),
      .names = "{.col}_recoded"
    )
  ) %>%
  mutate(conservatism = rowMeans(
    select(., PoliSoc_recoded, PoliEcon_recoded, PoliFore_recoded), 
    na.rm = TRUE)) %>%
  # create mean just world belief
  mutate(
    across(
      .cols = starts_with("GBJWS", ignore.case = FALSE),
      .fns = list(~recode(.x,
                          "Strongly Disgree\n(1)" = 1,
                          "2" = 2,
                          "3" = 3,
                          "4" = 4,
                          "5" = 5,
                          "Strongly Agree\n(6)" = 6)),
      .names = "{.col}_recoded"
    )
  ) %>%
  mutate(just_world_belief = rowMeans(
    select(., GBJWS1_recoded, GBJWS2_recoded, GBJWS3_recoded,
           GBJWS4_recoded, GBJWS5_recoded, GBJWS6_recoded,
           GBJWS7_recoded), 
    na.rm = TRUE)) %>%
  # create summed MFQ column
  mutate(
    across(
      .cols = starts_with("MFQ"),
      .fns = list(~recode(.x,
                          "extremely relevant" = 6,
                          "extremely revelant" = 6,
                          "Strongly agree" = 6,
                          "very relevant" = 5,
                          "very revelant" = 5,
                          "Moderately agree" = 5,
                          "somewhat relevant" = 4,
                          "somewhat revelant" = 4,
                          "Slightly agree" = 4,
                          "slightly relevant" = 3,
                          "slightly revelant" = 3,
                          "Slightly disagree" = 3,
                          "not very relevant" = 2,
                          "not very revelant" = 2,
                          "Moderately disagree" = 2,
                          "not at all relevant" = 1,
                          "not at all revelant" = 1,
                          "Strongly disagree" = 1)),
      .names = "{.col}_recoded"
    )
  ) %>%
  mutate(
    MFQharm = rowSums(select(., MFQ1_recoded, MFQ7_recoded, MFQ12_recoded, 
                             MFQ17_recoded, MFQ23_recoded, MFQ28_recoded), 
                      na.rm = FALSE),
    MFQfairness = rowSums(select(., MFQ2_recoded, MFQ8_recoded, MFQ13_recoded, 
                                 MFQ18_recoded, MFQ24_recoded, MFQ29_recoded), 
                      na.rm = FALSE),
    MFQloyalty = rowSums(select(., MFQ3_recoded, MFQ9_recoded, MFQ14_recoded, 
                                MFQ19_recoded, MFQ25_recoded, MFQ30_recoded), 
                      na.rm = FALSE),
    MFQauthority = rowSums(select(., MFQ4_recoded, MFQ10_recoded, MFQ15_recoded, 
                                  MFQ20_recoded, MFQ26_recoded, MFQ31_recoded), 
                      na.rm = FALSE),
    MFQpurity = rowSums(select(., MFQ5_recoded, MFQ11_recoded, MFQ16_recoded, 
                               MFQ21_recoded, MFQ27_recoded, MFQ32_recoded), 
                      na.rm = FALSE))
