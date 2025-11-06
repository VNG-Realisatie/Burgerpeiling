#-----------------------------------------------------------------------------------------------

# Typology

#-----------------------------------------------------------------------------------------------

message("Typology...")

# Quality of life
# boundaries are based on all Burgerpeilingen of the past 2 years

# 2024-12-04
# qol_bin_breaks: -Inf    3    4    6    7  Inf
# part_bin_breaks: -Inf    1    3    4    7  Inf

# Flexible binning: fixed breaks OR optbin (MSE)
# - typol_overrule = TRUE  -> use fixed classes (your case_when thresholds)
# - typol_overrule = FALSE -> use optbin::optbin(..., metric="mse")
# You can customize fixed breaks and optbin args if needed.

apply_typology_binning <- function(df_ss,
                                   typol_overrule = TRUE,
                                   fixed_breaks = list(
                                     qol  = c(-Inf, 3, 4, 5, Inf),
                                     part = c(-Inf, 1, 3, 4, Inf)
                                   ),
                                   optbin_nbins = 4,
                                   optbin_metric = "mse",
                                   optbin_max_cache = 6^31) {
  stopifnot(all(c("qol_score","part_score") %in% names(df_ss)))
  
  if (isTRUE(typol_overrule)) {
    # --- Fixed classes (manual thresholds) ---
    df_ss <- df_ss %>%
      mutate(
        qol_score_bin = case_when(
          is.na(qol_score)   ~ NA_integer_,
          qol_score <= fixed_breaks$qol[2] ~ 1,
          qol_score <= fixed_breaks$qol[3] ~ 2,
          qol_score <= fixed_breaks$qol[4] ~ 3,
          TRUE ~ 4
        ),
        part_score_bin = case_when(
          is.na(part_score)  ~ NA_integer_,
          part_score <= fixed_breaks$part[2] ~ 1,
          part_score <= fixed_breaks$part[3] ~ 2,
          part_score <= fixed_breaks$part[4] ~ 3,
          TRUE ~ 4
        )
      )
    return(df_ss)
  }
  
  # --- Data-driven classes via optbin ---
  # Helper to run optbin safely and return breaks
  .opt_breaks <- function(x, nbins = 4, metric = "mse", max_cache = 6^31) {
    # optbin::optbin(x, nbins, ...) uses positional nbins
    ob <- tryCatch(
      optbin::optbin(x, nbins, na.rm = TRUE, metric = metric, max.cache = max_cache),
      error = function(e) e
    )
    if (inherits(ob, "error") || is.null(ob$thr)) {
      return(NULL)
    }
    c(-Inf, ob$thr, Inf)
  }
  
  qol_breaks  <- .opt_breaks(df_ss$qol_score,  nbins = optbin_nbins, metric = optbin_metric, max_cache = optbin_max_cache)
  part_breaks <- .opt_breaks(df_ss$part_score, nbins = optbin_nbins, metric = optbin_metric, max_cache = optbin_max_cache)
  
  # Fallback to fixed if optbin failed for any reason (too few unique values, etc.)
  if (is.null(qol_breaks))  qol_breaks  <- fixed_breaks$qol
  if (is.null(part_breaks)) part_breaks <- fixed_breaks$part
  
  df_ss$qol_score_bin  <- cut(df_ss$qol_score,  breaks = qol_breaks,  labels = FALSE, include.lowest = TRUE)
  df_ss$part_score_bin <- cut(df_ss$part_score, breaks = part_breaks, labels = FALSE, include.lowest = TRUE)
  
  df_ss
}

# -------------------- Usage --------------------
# Choose whether to overrule with fixed classes:
typol_overrule <- TRUE  # set FALSE to use optbin

# Apply binning
df_ss <- apply_typology_binning(
  df_ss,
  typol_overrule = typol_overrule,
  # optional tweaks:
  fixed_breaks = list(qol=c(-Inf,3,4,5,Inf), part=c(-Inf,1,3,4,Inf)),
  # optbin_nbins = 4, optbin_metric = "mse"
)

# Continue with your recodes (unchanged)
df_ss <- df_ss %>%
  mutate(
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
hist(df_ss$typology, main = paste0("Histogram of Typology\n", current_date), xlab = "Typology", ylab = "Frequency", col = "blue")
dev.off()

# Optional: Print the filename to confirm the file was saved
print(paste("Histogram saved to", filename))

# Convert to survey design
df_ss_weight <- df_ss %>%
  as_survey_design(
    ids = 1, # 1 for no cluster ids
    weights = weging, # weight added
    strata = NULL
  ) # sampling was simple (no strata)

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
    # handle incorrect or incomplete inclusion of features in typology
    typology_pin2 = if_else(is.na(typology_pin1) | typology_pin1 > 65, NA_real_, typology_pin2),
    typology_pin3 = if_else(is.na(typology_pin1) | typology_pin1 > 65, NA_real_, typology_pin3),
    typology_pin4 = if_else(is.na(typology_pin1) | typology_pin1 > 65, NA_real_, typology_pin4),
    typology_pin1 = if_else(typology_pin1 > 65, NA_real_, typology_pin1),
    zorgwekkend_pin1 = if_else(is.na(typology_pin1) | typology_pin1 > 65, NA_real_, zorgwekkend_pin1)
  )
