
#-----------------------------------------------------------------------------------------------

# Missing values

#-----------------------------------------------------------------------------------------------

message("Checking for missing values...")

# 1. Replace values >10 or <= -1 with NA, but only if df has >=8 columns
if (ncol(df) >= 8) {
  df <- df %>%
    mutate(across(8:ncol(df), ~ ifelse(. > 10 | . <= -1, NA, .)))
}

# 2. Helper: Get column position safely
get_var_position <- function(df, var_name) {
  match(var_name, names(df))
}

# 3. Max value per column, handle all-NA columns
colMax <- function(df) sapply(df, function(col) if (all(is.na(col))) NA else max(col, na.rm = TRUE))

# 4. Identify variables with exactly 6 categories
cols_with_6_cats <- which(colMax(df) == 6)

# 5. Exclude specific variables (if they exist)
vars_to_exclude <- c("ch02", "ch03")
exclude_positions <- na.omit(sapply(vars_to_exclude, get_var_position, df = df))
cols_with_6_cats <- setdiff(cols_with_6_cats, exclude_positions)

# 6. Recode category 6 to NA in those columns
df[cols_with_6_cats] <- lapply(df[cols_with_6_cats], function(x) ifelse(x == 6, NA, x))

# 7. Recode custom missing categories
missing_cats <- list(
  "7" = c('ch06'),
  "5" = c('wl06', 'zw01_0', 'zw01_1', 'zw01_2', 'zw01_3', 'zw01_4', 'zw01_5', 'zw01_6', 'zw04', 'zw09'),
  "4" = c('wl14', 'zw03', 'zw07', 'zw10_0', 'zw10_1', 'zw10_2'),
  "1" = c('zw13_0', 'zw13_1', 'zw13_10', 'zw13_2', 'zw13_3', 'zw13_4', 'zw13_5', 'zw13_6', 'zw13_7', 'zw13_8', 'zw13_9',
          'zw08_0', 'zw08_1', 'zw08_2', 'zw08_3', 'zw08_4', 'zw08_5', 'zw08_6', 'zw08_7')
)

for (cat_val in names(missing_cats)) {
  target_cols <- missing_cats[[cat_val]]
  # Only include columns that exist in df
  existing_cols <- intersect(target_cols, names(df))
  
  if (length(existing_cols) > 0) {
    df <- df %>%
      mutate(across(all_of(existing_cols), ~ ifelse(. == as.numeric(cat_val), NA, .)))
  } else {
    message(paste0("No matching columns found for category ", cat_val, ". Skipped."))
  }
}

# Ensure result is a data.frame
df <- as.data.frame(df)

message("Missing value processing complete.")
