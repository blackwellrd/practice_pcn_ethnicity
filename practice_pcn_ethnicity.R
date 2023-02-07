# =========================================================================== #
#                                                                             #
# Title:        Practice and PCN Level Ethnicity 2021 Converter               #
# Filename:     practice_pcn_ethnicity.R                                      #
# Description:  Convert 2021 Census Ethnicity data at Lower-layer Super       #
#               Output Area (LSOA) into Practice and PCN level using GP       #
#               Practice Registration by LSOA and PCN Membership data         #
# Author:       Richard Blackwell                                             #
# Email:        richard.blackwell@swahsn.com                                  #
# Date:         2023-07-02                                                    #
#                                                                             #
# =========================================================================== #

# Load libraries
# ==============

if(!require(tidyverse)){
  install.packages('tidyverse')
  library(tidyverse)
}

if(!require(readxl)){
  install.packages('readxl')
  library(readxl)
}

# Load data sources
# =================

# LSOA 2011 to LSOA 2021 lookup data - import data, select relevant fields and rename
df_lsoa11_lsoa21_lu <- read.csv('.\\data\\LSOA_(2011)_to_LSOA_(2021)_to_Local_Authority_District_(2022)_Lookup_for_England_and_Wales.csv') %>%
  select(1, 3, 5) %>% 
  rename_with(.fn = function(x){c('lsoa11cd', 'lsoa21cd', 'chgind')}) %>%
  # Ony *English* LSOAs
  filter(grepl('^E',lsoa11cd))

# Current English GP Practices - import data, select relevant fields, filter data and rename fields
df_practice <- read.csv('.\\data\\epraccur.csv', header = FALSE) %>%
  select(1, 2, 10, 13, 15, 26) %>%
  rename_with(.fn = function(x){c('practice_code', 'practice_name', 'postcode', 'status_code', 'subicb_code', 'prescribing_code')}) %>%
  # Only *Active* practices with a prescribing setting of *GP Practice*
  filter(status_code == 'A' & prescribing_code == 4) %>%
  select(-c('status_code', 'prescribing_code'))

# Current English PCNs - import data, select relevant fields, filter data and rename fields
df_pcn <- read_excel(path = '.\\data\\ePCN.xlsx', sheet = 'PCNDetails') %>%
  select(1, 2, 3, 6, 12) %>%
  rename_with(.fn = function(x){c('pcn_code', 'pcn_name', 'subicb_code', 'close_date', 'postcode')}) %>%
  # Only *Active* PCNs 
  filter(is.na(close_date)) %>%
  select(-close_date)
df_pcn_member <- read_excel(path = '.\\data\\ePCN.xlsx', sheet = 'PCN Core Partner Details') %>%
  select(1, 5, 10) %>%
  rename_with(.fn = function(x){c('practice_code', 'pcn_code', 'depart_date')}) %>%
  # Only GP Practices that are a *Current* member of a PCN
  filter(is.na(depart_date)) %>%
  select(-depart_date)

# GP practice registration data - import data, select relevant fields and rename fields
df_practice_popn <- read.csv('.\\data\\gp-reg-pat-prac-lsoa-all.csv') %>%
  select(3, 5, 7) %>%
  rename_with(.fn = function(x){c('practice_code', 'lsoa11cd', 'reg_popn')}) %>%
  # Only patients with a valid *LSOA* code
  filter(lsoa11cd!='NO2011')

# Detailed Version Commented Out - If required uncomment and comment out summary version below
# # Census 2021 Ethnicity data - import data, select relevant fields and rename fields
# df_ethnicity <- read.csv('.\\data\\census2021-ts021-lsoa.csv') %>%
#   select(3, 4, 6:10, 12:14, 16:19, 21:25, 27:28) %>%
#     rename_with(.fn = function(x){c(
#       # Geography
#       'lsoa21cd',
#       # Total Population
#       'total_popn',
#       # Asian
#       'asian_bangladeshi', 'asian_chinese', 'asian_indian', 'asian_pakistani', 'asian_other',
#       # Black
#       'black_african', 'black_caribbean', 'black_other',
#       # Mixed
#       'mixed_white_asian', 'mixed_white_black_african', 'mixed_white_black_caribbean', 'mixed_other',
#       # White
#       'white_british', 'white_irish', 'white_gypsy_irish_traveller', 'white_roma', 'white_other',
#       # Other
#       'other_arab', 'other_other')})

# Census 2021 Ethnicity data - import data, select relevant fields and rename fields
df_ethnicity <- read.csv('.\\data\\census2021-ts021-lsoa.csv') %>%
  select(3, 4, 20) %>%
    rename_with(.fn = function(x){c(
      # Geography
      'lsoa21cd',
      # Total Population
      'total_popn',
      # White
      'total_white')})

# Map ethnicity data to 2011 LSOA
# ===============================

# Unchanged
df_unchanged <- df_ethnicity %>% 
  inner_join(
    df_lsoa11_lsoa21_lu %>% filter(chgind=='U'),
    by = c('lsoa21cd' = 'lsoa21cd')
  ) %>%
  select(lsoa11cd, chgind, total_popn, total_white)

# Split
df_split <- df_ethnicity %>% 
  inner_join(
    df_lsoa11_lsoa21_lu %>% filter(chgind=='S'),
    by = c('lsoa21cd' = 'lsoa21cd')
  ) %>%
  group_by(lsoa11cd, chgind) %>%
  summarise(total_popn = sum(total_popn), total_white = sum(total_white), .groups = 'keep') %>%
  ungroup() %>%
  select(lsoa11cd, chgind, total_popn, total_white)

# Irregular
# There are 9 areas affected by irregular mapping and a manual exercise leaves the following 
# as the best fit, all are essentially 1:1 unchanged mapping even thought the boundaries have changed
# slightly
df_irregular <- df_ethnicity %>% 
  inner_join(
    data.frame(
      lsoa11cd = c('E01008187','E01023508','E01023679','E01023768','E01023964','E01027506'),
      lsoa21cd = c('E01035624','E01035609','E01035581','E01035582','E01035608','E01035637'),
      chgind = rep('U', 6)
    ),
    by = c('lsoa21cd' = 'lsoa21cd')
  ) %>%
  select(lsoa11cd, chgind, total_popn, total_white)

# Merged
# In order to unpick the merged volumes we can calculate the overall proportion of the
# old areas combined as a new area and apply these proportions to the 2021 volume to
# deconstruct the merge. In order to do this we need the practice population to calculated those proportions
df_tmp <- df_practice_popn %>% 
  group_by(lsoa11cd) %>% 
  summarise(reg_popn = sum(reg_popn), .groups = 'keep') %>%
  ungroup() %>%
  inner_join(
    df_lsoa11_lsoa21_lu %>% filter(chgind=='M'),
    by = c('lsoa11cd' = 'lsoa11cd')
  )

df_merged <- df_ethnicity %>% 
  inner_join(
    df_tmp %>%
      inner_join(
        df_tmp %>% 
          group_by(lsoa21cd) %>% 
          summarise(total_popn = sum(reg_popn), .groups = 'keep') %>%
          ungroup(),
        by = c('lsoa21cd' = 'lsoa21cd')
      ) %>% 
      transmute(lsoa11cd, lsoa21cd, chgind, pct_popn = reg_popn / total_popn),
    by = c('lsoa21cd' = 'lsoa21cd')
  ) %>%
  transmute(
    lsoa11cd, chgind, 
    total_popn = total_popn * pct_popn,
    total_white = total_white * pct_popn
  )

df_ethnicity <- df_unchanged %>%
  bind_rows(df_split) %>%
  bind_rows(df_irregular) %>%
  bind_rows(df_merged) %>%
  mutate(pct_non_white = 1 - (total_white / total_popn))

# Tidy up a little
rm(list = c('df_tmp', 'df_unchanged', 'df_split', 'df_irregular', 'df_merged'))

# Calculate non-white popn of practices
# =====================================
df_practice_ethnicity <- df_practice_popn %>% 
  left_join(
    df_ethnicity %>% select(lsoa11cd, pct_non_white),
    by = c('lsoa11cd' = 'lsoa11cd')
  ) %>%
  mutate(non_white_popn = reg_popn * pct_non_white) %>%
  group_by(practice_code) %>%
  summarise(
    non_white_popn = sum(non_white_popn),
    reg_popn = sum(reg_popn),
    .groups = 'keep') %>%
  ungroup() %>%
  mutate(pct_non_white = non_white_popn / reg_popn)

# Calculate non-white popn of PCNs
# ================================
df_pcn_ethnicity <- df_practice_ethnicity %>% 
  left_join(
    df_pcn_member,
    by = c('practice_code' = 'practice_code')
  ) %>%
  group_by(pcn_code) %>%
  summarise(
    non_white_popn = sum(non_white_popn),
    reg_popn = sum(reg_popn),
    .groups = 'keep') %>%
  ungroup() %>%
  mutate(pct_non_white = non_white_popn / reg_popn)

# Add in practice details
# =======================

# Add in the practice name and postcode from the current English GP practice data
df_practice_ethnicity <- df_practice_ethnicity %>% 
  left_join(
    df_practice,
    by = c('practice_code' = 'practice_code')
  ) %>%
  select(
    practice_code,
    practice_name,
    postcode,
    subicb_code,
    non_white_popn,
    reg_popn,
    pct_non_white
  ) %>%
  replace_na(list(practice_code = 'Unknown', postcode = 'Unknown', subicb_code = 'UNK'))

# Add in PCN details
# ==================

# Add in the pcn name and postcode and sub-ICB code from the current English PCN data
df_pcn_ethnicity <- df_pcn_ethnicity %>% 
  left_join(
    df_pcn,
    by = c('pcn_code' = 'pcn_code')
  ) %>%
    select(
      pcn_code,
      pcn_name,
      postcode,
      subicb_code,
      non_white_popn,
      reg_popn,
      pct_non_white
    ) %>%
    replace_na(list(pcn_name = 'Unknown', postcode = 'Unknown', subicb_code = 'UNK'))

# Output data
# ===========

dir.create('./outputs', showWarnings = FALSE, recursive = TRUE)
write.csv(df_practice_ethnicity, './outputs/practice_ethnicity.csv', row.names = FALSE)
write.csv(df_pcn_ethnicity, './outputs/pcn_ethnicity.csv', row.names = FALSE)
