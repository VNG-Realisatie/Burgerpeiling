
#-----------------------------------------------------------------------------------------------

#Typology

#-----------------------------------------------------------------------------------------------

message("Typology...")

#boundaries are based on all Burgerpeilingen of the past 2 years, update 13 april 2023 
#optimal binning 
#qol_bin <- optbin::optbin(df_ss$qol_score, 4,na.rm = T, metric=c('mse'), max.cache=6^31)

#hist(qol_bin)
#summary(qol_bin)

#part_bin <- optbin::optbin(df_ss$part_score, 4,na.rm = T, metric=c('mse'), max.cache=6^31)

#hist(part_bin)
#summary(part_bin)

#access to threshold
#qol_bin[["thr"]][1]

#crosstab <- xtabs(~ qol_score + part_score, data = df_ss)
#groups <- split(crosstab, rownames(crosstab))
#crosstab

#-----------------------------------------------------------------------------------------------

# Recode variables using case_when for readability
df_ss <- df_ss %>%
  mutate(
    qol_score_bin = case_when(
      is.na(qol_score) ~ NA_integer_,
      qol_score <= 3 ~ 1,
      qol_score <= 4 ~ 2,
      qol_score <= 5 ~ 3,
      TRUE ~ 4
    ),
    part_score_bin = case_when(
      is.na(part_score) ~ NA_integer_,
      part_score <= 1 ~ 1,
      part_score <= 3 ~ 2,
      part_score <= 4 ~ 3,
      TRUE ~ 4
    ),
    typology = case_when(
      part_score_bin > 2 & qol_score_bin > 2 ~ 1, # weerbaren
      part_score_bin < 3 & qol_score_bin > 2 ~ 2, # buitenstaanders
      part_score_bin > 2 & qol_score_bin < 3 ~ 3, # compenseerders
      part_score_bin < 3 & qol_score_bin < 3 ~ 4, # kwetsbaren
      TRUE ~ NA_integer_
    ),
    zorwekkend = case_when(
      part_score_bin < 2 & qol_score_bin < 2 ~ 1,
      is.na(part_score_bin) | is.na(qol_score_bin) ~ NA_integer_,
      TRUE ~ 0
    ),
    zelfredzaam = case_when(
      typology > 2 ~ 0,
      typology > 0 ~ 1,
      is.na(part_score) ~ NA_integer_,
      TRUE ~ NA_integer_
    ),
    dv01_red0 = if_else(zelfredzaam == 0, dv01, NA_real_),
    dv01_red1 = if_else(zelfredzaam == 1, dv01, NA_real_),
    dv06_red0 = if_else(zelfredzaam == 0, dv06, NA_real_),
    dv06_red1 = if_else(zelfredzaam == 1, dv06, NA_real_),
    zw12_red0 = if_else(zelfredzaam == 0, zw12, NA_real_),
    zw12_red1 = if_else(zelfredzaam == 1, zw12, NA_real_),
    hostman_ss_red0 = if_else(zelfredzaam == 0, hostman_ss, NA_real_),
    hostman_ss_red1 = if_else(zelfredzaam == 1, hostman_ss, NA_real_),
    gelukkig = case_when(
      zw00 > 6 ~ 1,
      is.na(zw00) ~ NA_integer_,
      TRUE ~ 0
    )
  )

# Convert to survey design
df_ss_weight <- df_ss %>%
  as_survey_design(ids = 1, # 1 for no cluster ids 
                   weights = weging, # weight added
                   strata = NULL) # sampling was simple (no strata)

# Calculate survey means
df_ss_vital <- df_ss_weight %>%
  group_by(GEOITEM, PERIOD) %>%
  summarize(
    typology_pin1 = survey_mean(typology == 1, na.rm = TRUE, vartype = vt) * 100,
    typology_pin2 = survey_mean(typology == 2, na.rm = TRUE, vartype = vt) * 100,
    typology_pin3 = survey_mean(typology == 3, na.rm = TRUE, vartype = vt) * 100,
    typology_pin4 = survey_mean(typology == 4, na.rm = TRUE, vartype = vt) * 100,
    zorgwekkend_pin1 = survey_mean(zorwekkend == 1, na.rm = TRUE, vartype = vt) * 100
  ) %>%
  mutate(across(starts_with("typology_pin"), ~ if_else(. == 0, NA_real_, .))) %>%
  mutate(
    typology_pin2 = if_else(is.na(typology_pin1) | typology_pin1 > 65, NA_real_, typology_pin2),
    typology_pin3 = if_else(is.na(typology_pin1) | typology_pin1 > 65, NA_real_, typology_pin3),
    typology_pin4 = if_else(is.na(typology_pin1) | typology_pin1 > 65, NA_real_, typology_pin4),
    typology_pin1 = if_else(typology_pin1 > 65, NA_real_, typology_pin1),
    zorgwekkend_pin1 = if_else(is.na(typology_pin1) | typology_pin1 > 65, NA_real_, zorgwekkend_pin1)
  )
