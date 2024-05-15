
#-----------------------------------------------------------------------------------------------

#Typology

#-----------------------------------------------------------------------------------------------

message("Typology...")

#Quality of life
#boundaries are based on all Burgerpeilingen of the past 2 years
#optimal binning 
# Perform optimal binning with 4 bins, removing NA values, using mean squared error as the metric
qol_bin <- optbin::optbin(df_ss$qol_score, 4, na.rm = TRUE, metric = c('mse'), max.cache = 6^31)

# Extract the bin thresholds
qol_bin_breaks <- c(-Inf, qol_bin$thr, Inf)

# Assign each qol_score to a bin
df_ss$qol_score_bin <- cut(df_ss$qol_score, breaks = qol_bin_breaks, labels = FALSE, include.lowest = TRUE)

# Plot the histogram of the binned data
hist(as.numeric(df_ss$qol_score_bin), main = "Histogram of Quality of Life Scores", xlab = "Bins", ylab = "Frequency", col = "blue")




#Participation
# Perform optimal binning on part_score with 4 bins
part_bin <- optbin::optbin(df_ss$part_score, 4, na.rm = TRUE, metric = c('mse'), max.cache = 6^31)

# Extract the bin thresholds for part_score
part_bin_breaks <- c(-Inf, part_bin$thr, Inf)

# Assign each part_score to a bin
df_ss$part_score_bin <- cut(df_ss$part_score, breaks = part_bin_breaks, labels = FALSE, include.lowest = TRUE)

# Plot the histogram of the binned part_score data
hist(as.numeric(df_ss$part_score_bin), main = "Histogram of Participation Scores", xlab = "Bins", ylab = "Frequency", col = "green")


# Create a crosstab of part_bin and qol_bin
crosstab <- xtabs(~ qol_score_bin + part_score_bin, data = df_ss)
groups <- split(crosstab, rownames(crosstab))
crosstab

#-----------------------------------------------------------------------------------------------

# Recode variables using case_when for readability
df_ss <- df_ss %>%
  mutate(
    #since 2023-05-15 we use optimal binning (past 2 years)
   # qol_score_bin = case_when(
  #    is.na(qol_score) ~ NA_integer_,
   #   qol_score <= 3 ~ 1,
    #  qol_score <= 4 ~ 2,
    #  qol_score <= 5 ~ 3,
    #  TRUE ~ 4
  #  ),
  #  part_score_bin = case_when(
   #   is.na(part_score) ~ NA_integer_,
   #   part_score <= 1 ~ 1,
  #    part_score <= 3 ~ 2,
   #   part_score <= 4 ~ 3,
   #   TRUE ~ 4
   # ),
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


# Get the current date
current_date <- Sys.Date()

# Define the filename with the current date
filename <- paste0("PLOTS/typology_histogram_", current_date, ".png")

# Save the histogram to the specified file
png(filename)
hist(df_ss$typology, main = paste0("Histogram of Typology\n",current_date), xlab = "Typology", ylab = "Frequency", col = "blue")
dev.off()

# Optional: Print the filename to confirm the file was saved
print(paste("Histogram saved to", filename))

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
