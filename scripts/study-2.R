#
## Study 2
#

# import libraries
library(tidyverse)

# load data
file_paths <- c(
  "./private-data/study-2-benedictine-data.csv",
  "./private-data/study-2-bellevue-data.csv"
)

raw_df_col_names <- read_csv(
  file_paths[1], 
  n_max = 0, 
  show_col_types = FALSE
) %>% 
  names()

df1 <- read_csv(
  file_paths[1], 
  col_names = raw_df_col_names, 
  skip = 1, 
  show_col_types = FALSE
) %>%
  mutate(college = "Benedictine")

df2 <- read_csv(
  file_paths[2], 
  col_names = raw_df_col_names, 
  skip = 3, 
  show_col_types = FALSE
) %>%
  mutate(college = "Bellevue")

df2$likely_dups <- NA

# examine missing data
missing_df <- rbind(df1, df2) %>%
  filter(
    consent == "I understand and agree to continue" | consent == "Continue",
    Debrief == "Continue" | is.na(.$Debrief),
    is.na(likely_dups)
  ) %>%
  select(clif_harm_e_01_1:graham_harm_01_2) %>%
  mutate(na = rowSums(is.na(.)))

table(missing_df$na)

# merge datasets and clean data
df <- rbind(df1, df2) %>%
  mutate(total_na =  rowSums(is.na(.))) %>%
  # remove participants with excessive missing data
  filter(
    total_na < 20,
    consent == "I understand and agree to continue" | consent == "Continue",
    Debrief == "Continue" | is.na(.$Debrief),
    is.na(likely_dups)
  ) %>%
  # recode variables
  mutate(
    across(
      clif_harm_e_01_1:graham_harm_01_2, 
      ~ifelse(.x == "Not at all\n0\n", 0, .x)      
    ),
    across(
      clif_harm_e_01_1:graham_harm_01_2, 
      ~ifelse(.x == "Extremely\n5\n", 5, .x)      
    ),
    across(
      clif_harm_e_01_1:graham_harm_01_2,
      ~as.numeric(.)
    )
  ) %>%
  # create composite variables
  mutate(
    clif_harm_wrong_mean = rowMeans(
      select(., 
             starts_with("clif_harm") & ends_with("1")
      ), 
      na.rm = TRUE
    ),
    clif_fair_wrong_mean = rowMeans(
      select(., 
             starts_with("clif_fair") & ends_with("1")
      ), 
      na.rm = TRUE
    ),
    clif_pure_wrong_mean = rowMeans(
      select(., 
             starts_with("clif_pure") & ends_with("1")
      ), 
      na.rm = TRUE
    ),
    graham_harm_wrong_mean = rowMeans(
      select(., 
             starts_with("graham_harm") & ends_with("1")
      ), 
      na.rm = TRUE
    ),
    graham_pure_wrong_mean = rowMeans(
      select(., 
             starts_with("graham_pure") & ends_with("1")
      ), 
      na.rm = TRUE
    ),
    graham_fair_wrong_mean = rowMeans(
      select(., 
             starts_with("graham_fair") & ends_with("1")
      ), 
      na.rm = TRUE
    ),
    clif_wrong_mean = rowMeans(
      select(., 
             starts_with("clif") & ends_with("1")
      ), 
      na.rm = TRUE
    ),
    graham_wrong_mean = rowMeans(
      select(., 
             starts_with("graham") & ends_with("1")
      ), 
      na.rm = TRUE
    ),
    emt_wrong_mean = rowMeans(
      select(., 
             starts_with("emt") & ends_with("1")
      ), 
      na.rm = TRUE
    ),
    clif_harm_likely_mean = rowMeans(
      select(., 
             starts_with("clif_harm") & ends_with("2")
      ), 
      na.rm = TRUE
    ),
    clif_fair_likely_mean = rowMeans(
      select(., 
             starts_with("clif_fair") & ends_with("2")
      ), 
      na.rm = TRUE
    ),
    clif_pure_likely_mean = rowMeans(
      select(., 
             starts_with("clif_pure") & ends_with("2")
      ), 
      na.rm = TRUE
    ),
    graham_harm_likely_mean = rowMeans(
      select(., 
             starts_with("graham_harm") & ends_with("2")
      ), 
      na.rm = TRUE
    ),
    graham_pure_likely_mean = rowMeans(
      select(., 
             starts_with("graham_pure") & ends_with("2")
      ), 
      na.rm = TRUE
    ),
    graham_fair_likely_mean = rowMeans(
      select(., 
             starts_with("graham_fair") & ends_with("2")
      ), 
      na.rm = TRUE
    ),
    clif_likely_mean = rowMeans(
      select(., 
             starts_with("clif") & ends_with("2")
      ), 
      na.rm = TRUE
    ),
    graham_likely_mean = rowMeans(
      select(., 
             starts_with("graham") & ends_with("2")
      ), 
      na.rm = TRUE
    ),
    emt_likely_mean = rowMeans(
      select(., 
             starts_with("emt") & ends_with("2")
      ), 
      na.rm = TRUE
    ),
    age = replace(age, which(age > 100), NA)
  )

# convert to long form
df_long_wrongness <- df %>%
  mutate(
    id = factor(1:nrow(.))
  ) %>%
  select(
    id,
    starts_with("clif") & ends_with("1"),
    starts_with("emt") & ends_with("1"),
    starts_with("graham") & ends_with("1")
  ) %>%
  pivot_longer(
    cols = !id,
    names_to = "scenario",
    values_to = "wrongness"
  )

df_long_typicality <- df %>%
  mutate(
    id = factor(1:nrow(.))
  ) %>%
  select(
    id,
    starts_with("clif") & ends_with("2"),
    starts_with("emt") & ends_with("2"),
    starts_with("graham") & ends_with("2")
  ) %>%
  pivot_longer(
    cols = !id,
    names_to = "scenario",
    values_to = "typicality"
  )

df_long <- df_long_wrongness %>%
  mutate(
    typicality = df_long_typicality$typicality
  ) %>%
  mutate(
    scenario_3 = factor(
      case_when(
        grepl("^clif", .$scenario) ~ "clif",
        grepl("^emt", .$scenario) ~ "emt",
        grepl("^graham", .$scenario) ~ "graham"
      ),
      levels = c("emt", "clif", "graham")
    ),
    scenario_7 = factor(
      case_when(
        grepl("^clif_harm", .$scenario) ~ "clif_harm",
        grepl("^clif_fair", .$scenario) ~ "clif_fair",
        grepl("^clif_pure", .$scenario) ~ "clif_pure",
        grepl("^graham_harm", .$scenario) ~ "graham_harm",
        grepl("^graham_fair", .$scenario) ~ "graham_fair",
        grepl("^graham_pure", .$scenario) ~ "graham_pure",
        grepl("^emt", .$scenario) ~ "emt",
      ),
      levels = c("emt", 
                 "clif_harm", "clif_fair", "clif_pure",
                 "graham_harm", "graham_fair", "graham_pure")
    )
  ) %>%
  na.omit()

# save data as csv
write_csv(df, "./public-data/study-2-public-data-wide.csv")
write_csv(df_long, "./public-data/study-2-public-data-long.csv")
