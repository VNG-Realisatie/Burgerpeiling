#-----------------------------------------------------------------------------------------------

# Burgerpeiling – Waar Staat Je Gemeente

#-----------------------------------------------------------------------------------------------

# Deze procedure is bedoeld om de resultaten van de Burgerpeiling te controleren, zoals deze
# (zullen worden) gepresenteerd op Waarstaatjegemeente.nl. Daarnaast biedt de procedure de
# mogelijkheid om aanvullende indicatoren en visualisaties te genereren voor intern gebruik
# of analyses op maat.

# Let op: deze procedure is **niet bedoeld** voor het voorbereiden van gegevens voor
# officiële publicatie op Waarstaatjegemeente.nl.

# Zie de map 'Beschrijving' voor een overzicht van en toelichting op de gebruikte variabelen.

# Laatste update: 2 juli 2025

# Vragen of opmerkingen? Neem gerust contact op met:
# Mark Henry Gremmen – mark.gremmen@vng.nl (VNG)

# Licentie: Creative Commons Attribution-ShareAlike 4.0 International (CC BY-SA 4.0)
# Dit betekent dat hergebruik, aanpassing en commercieel gebruik zijn toegestaan,
# mits correcte bronvermelding én onder dezelfde licentievoorwaarden.
# Voor details, zie: https://creativecommons.org/licenses/by-sa/4.0/


#-----------------------------------------------------------------------------------------------

# Setup environment

#-----------------------------------------------------------------------------------------------

# package isolation in project-specific environment
use_renv <- FALSE # change to TRUE to use renv (default: FALSE)

# Load necessary libraries
source("SRC/packages.R")

# Load global variables
source(here::here("SRC/globals.R"))

#-----------------------------------------------------------------------------------------------

# Set pipeline timer
start_time <- Sys.time()

#-----------------------------------------------------------------------------------------------

# IMPORT

#-----------------------------------------------------------------------------------------------

message("Import Burgerpeiling(en)...")

# read multiple Spss sav, RData and csv-files
# specify the file types to import
file_types <- c("sav", "RData", "csv")
# nullify certain variables
excluded_vars <- c("ch08", "ch09", "wijknr")

# List files and filter by extension
files <- fs::dir_ls(path = data.dir, recurse = FALSE) %>%
  as.character()
files <- files[tools::file_ext(files) %in% file_types]

# Read files
df_list <- purrr::map(files, function(file) {
  ext <- tools::file_ext(file)
  dat <- NULL

  if (ext == "sav") {
    dat <- haven::read_sav(file) %>%
      as_tibble() %>%
      select(-any_of(excluded_vars))
  } else if (ext == "RData") {
    tmp <- get(load(file))
    if (is.data.frame(tmp)) {
      dat <- as_tibble(tmp) %>%
        select(-any_of(excluded_vars))
    }
  } else if (ext == "csv") {
    dat <- read.csv(file, header = TRUE, stringsAsFactors = FALSE) %>%
      as_tibble() %>%
      select(-any_of(excluded_vars))
  }

  if (!is.null(dat)) {
    dat$file_id <- basename(file) # add source filename
  }

  return(dat)
})

# Harmonize column names
all_colnames <- unique(unlist(purrr::map(df_list, names)))

# Align and coerce all to character (temporarily)
df_aligned <- purrr::map(df_list, function(df) {
  if (is.null(df)) {
    return(NULL)
  }
  missing <- setdiff(all_colnames, names(df))
  df[missing] <- NA
  df <- df[all_colnames] # reorder columns
  mutate(df, across(everything(), as.character))
})

# Combine all
df <- bind_rows(df_aligned)

df <- df %>%
  relocate(file_id, .before = 1)

# store for reference
df_import_merged <- df

# weight available?
weight.exists <- any(colnames(df) == "weging")
if (weight.exists == FALSE) {
  stop("column weging does not exist!")
}

#-----------------------------------------------------------------------------------------------

# VALIDATION AND PREPERATION

#-----------------------------------------------------------------------------------------------

source(here::here("SRC/preparation.R"))

#-----------------------------------------------------------------------------------------------

# SUBSETTING

#-----------------------------------------------------------------------------------------------

message("Subsetting...")

df <- df %>%
  # municipality id exists
  filter(!is.na(GEOITEM)) %>%
  # filter specific municipality
  # filter(GEOITEM==828) %>%
  # year
  filter(PERIOD >= 2022) %>%
  # Weight lower than 6
  filter(!is.na(weging) & weging < 6) # %>%
# valid variables (see SRC>preparation.R)
# select(any_of(var_vec))

# Differentiate between numeric and non-numeric cols
# Step 1: Identify actual numeric columns
numeric_cols <- unlist(lapply(df, is.numeric))

# Step 2: Enhanced function to detect numeric-like columns with ',' as decimal
qualify_numeric <- function(x) {
  if (is.numeric(x)) {
    return(TRUE)
  }
  # Replace comma with dot and remove spaces
  x_clean <- gsub(",", ".", x)
  x_clean <- gsub("\\s", "", x_clean)
  x_num <- suppressWarnings(as.numeric(x_clean))
  prop_numeric <- sum(!is.na(x_num)) / length(x_num)
  return(prop_numeric >= 0.30)
}

# Step 3: Apply across all columns
qualified_numeric_cols <- unlist(lapply(df, qualify_numeric))

# See which variables were upgraded
upgraded <- names(df)[qualified_numeric_cols & !numeric_cols]
cat("\nUpgraded to numeric based on content: ", paste(upgraded, collapse = ", "))

# Step 4: Create final boolean vector of numeric-like columns
final_numeric_cols <- qualified_numeric_cols

# Step 5: Use for subsetting
df_numeric <- df[, final_numeric_cols]
df_string <- df[, !final_numeric_cols]

# convert variables to type : numeric
df_numeric <- df_numeric %>%
  mutate(across(everything(), ~ as.numeric(gsub(",", ".", gsub("\\s", "", .)))))

df <- df_numeric

numeric_cols <- sapply(df, is.numeric)
percentage_numeric <- mean(numeric_cols) * 100

cat("\n", round(percentage_numeric, 1), "% of columns are numeric (", sum(numeric_cols), "out of", ncol(df), ")\n")

#-----------------------------------------------------------------------------------------------

# MISSINGS

#-----------------------------------------------------------------------------------------------

source(here::here("SRC/missing.R"))

#-----------------------------------------------------------------------------------------------

# FACTORS AND LEVELS

#-----------------------------------------------------------------------------------------------

source(here::here("SRC/factors.R"))

#-----------------------------------------------------------------------------------------------

# IDENTIFIER

#-----------------------------------------------------------------------------------------------

# set sequence number
df$seq <- row.names(df)

# set unique identifier
df$id <- paste0("BP", df$GEOITEM, "Y", df$PERIOD, "S", df$seq)

# out.file<-paste0(output.dir,"/BP-combined.RData")
# save(df, file = out.file)

# order records by id
# df<-df %>%
#  order(id)

#----------------------------------------------------------------------------------------------

# Number of cases per GEOITEM, PERIOD

#-----------------------------------------------------------------------------------------------

# Create mapping from GEOITEM to GEMEENTE
geoitem_munic_df <- df %>%
  select(GEOITEM, GEMEENTE) %>%
  distinct()

# Count number of cases per GEOITEM and PERIOD
cases_per_geoitem_period <- df %>%
  count(GEOITEM, PERIOD, name = "n_cases")

# Join with municipality names
cases_report <- cases_per_geoitem_period %>%
  left_join(geoitem_munic_df, by = "GEOITEM") %>%
  select(GEOITEM, GEMEENTE, PERIOD, n_cases) %>%
  arrange(GEOITEM, PERIOD)

write.csv(cases_report, file.path(output.dir, "respons_per_municipality.csv"), row.names = FALSE)

# reporting municipalities
munic.active <- levels(factor(df$GEMEENTE))
geoitem.active <- as.numeric(levels(factor(df$GEOITEM)))

cat("reporting municipalities: ", munic.active, "\n")

#-----------------------------------------------------------------------------------------------

# isolate from merged file
# geoitem.sep<-geoitem.active

# df_clean<- df %>%
#  filter(!GEOITEM %in% geoitem.sep)

# write_sav(df_clean, "BP-unique1922.sav")

#-----------------------------------------------------------------------------------------------

# RECODE

#-----------------------------------------------------------------------------------------------

source(here::here("SRC/recode.R"))

#-----------------------------------------------------------------------------------------------

# Multiple response sets

#-----------------------------------------------------------------------------------------------

source(here::here("SRC/mrsets.R"))

#-----------------------------------------------------------------------------------------------

# WEIGHT

#-----------------------------------------------------------------------------------------------

message("apply Weight...")

# apply weight to df
df_weight <- df %>%
  srvyr::as_survey_design(
    ids = 1, # 1 for no cluster ids
    weights = weging, # weight added
    strata = NULL
  ) # sampling was simple (no strata)

# Report variability as one or more of: standard error ("se", default),
# confidence interval ("ci"), variance ("var") or coefficient of variation ("cv").
vt <- NULL

#-----------------------------------------------------------------------------------------------

# AGGREGATE

#-----------------------------------------------------------------------------------------------

# MEAN (group by gemeente, jaar)
mean_cols <- c(
  "wl01", "wl16", "bo06", "dv01", "dv06", "dv10", "zw00", "zw02", "zw12", "sc02"
  # aanvullende module tevredenheid met het leven
  # ,"sa01","sa02","sa03"
)

source(here::here("SRC/mean.R"))

#-----------------------------------------------------------------------------------------------

# PERCENTAGE IN (group by  gemeente, jaar)
source(here::here("SRC/pin.R"))

#-----------------------------------------------------------------------------------------------

# SCALE SCORES

#-----------------------------------------------------------------------------------------------

# scale score definitions
# https://www.waarstaatjegemeente.nl/Jive/ViewerReportContents.ashx?report=wsjg_bp_bijlage

# row wise operations
# parallel processing

message("Init parallel processing")

cluster <- multidplyr::new_cluster(parallel::detectCores() - 1)
multidplyr::cluster_library(cluster, c("tidyverse", "furrr"))
e <- 10
multidplyr::cluster_copy(cluster, "e")

source(here::here("SRC/ss_recode.R"))

#-----------------------------------------------------------------------------------------------

# TYPOLOGY

#-----------------------------------------------------------------------------------------------

source(here::here("SRC/typology.R"))

#-----------------------------------------------------------------------------------------------

# MEAN scales cores

#-----------------------------------------------------------------------------------------------

source(here::here("SRC/ss_mean.R"))

#-----------------------------------------------------------------------------------------------

# MERGE dataframes

#-----------------------------------------------------------------------------------------------

message("Merge...")

# respondent level
df_respond <- cbind(df, df_ss)

# gemeente level

df_list <- list(df_aggr_mn, df_aggr_pin, df_ss_aggr, df_ss_type_aggr, df_ss_age_aggr, df_ss_vital)
merge_key <- c("GEOITEM", "PERIOD")

# Merge data frames using reduce
merged_df <- reduce(df_list, function(x, y) merge(x, y, by = merge_key, all = TRUE))

df_munic <- cbind(merged_df, mr_sets)

#-----------------------------------------------------------------------------------------------

# WSJG

#-----------------------------------------------------------------------------------------------

source(here::here("SRC/wsjg.R"))

#-----------------------------------------------------------------------------------------------

# EXPORT

#-----------------------------------------------------------------------------------------------

# regular export
# csv
munic.csv <- paste0(output.dir, "/BP_munic.csv")
write.table(df_munic, file = munic.csv, quote = TRUE, sep = ";", dec = ",", row.names = FALSE)

# rdata gemeente
munic.df <- paste0(output.dir, "/BP_munic.RData")
save(df_munic, file = munic.df)

# rdata respondent
resp.df <- paste0(output.dir, "/BP_respond.RData")
save(df_respond, file = resp.df)

#-----------------------------------------------------------------------------------------------

# wsjg export

# df_export<-df_export %>% filter(PERIOD>2021)

# csv
wsjg.csv <- paste0(output.dir, "/BP_WSJG.csv")
write.table(df_export, file = wsjg.csv, quote = TRUE, sep = ";", dec = ",", row.names = FALSE)
# alternative version without quotes
wsjg.csv2 <- paste0(output.dir, "/BP_WSJG_NOQUOTES.csv")
write.table(df_export, file = wsjg.csv2, quote = FALSE, sep = ";", dec = ",", row.names = TRUE)

# RData
wsjg.df <- paste0(output.dir, "/BP_WSJG.RData")
save(df_export, file = wsjg.df)

# RData
# all vars + schaalscore per respondent
wsjg.ss.df <- paste0(output.dir, "/BP_WSJG_SS.RData")
save(df_ss, file = wsjg.ss.df)

#----------------------------------------------------------------------------------------------

# Plotting typology

#----------------------------------------------------------------------------------------------


# source(here::here('SRC/plotting_typology.R'))


#----------------------------------------------------------------------------------------------

# Debugging

#----------------------------------------------------------------------------------------------

end_time <- Sys.time()
process_time <- end_time - start_time

message("Finished, in ", process_time, "...")

rlang::last_error()
rlang::last_trace()
