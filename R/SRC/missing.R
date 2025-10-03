
# -----------------------------------------------------------------------------------------------
# Missing values / categorical recodes (numeric-only)
# - Out-of-range: >10 or <= -1 -> NA, starting at column named "bo01"
# - "Sixth category" rule: for numeric columns that (after cleaning) are codes in {1..6},
#   recode 6 -> NA
# - Custom recodes: set given code values to NA for listed variables
# -----------------------------------------------------------------------------------------------



message("Checking for missing values...")
stopifnot(is.data.frame(df))
n0 <- nrow(df)  # row-count guard

# Precompute numeric mask once (faster for big frames)
.is_num <- vapply(df, is.numeric, TRUE)

# ---- 1) Out-of-range recode starting at column named "bo01" (numeric-only) ----
start_col <- "bo01"
start_idx <- match(start_col, names(df))

if (!is.na(start_idx)) {
  rng_cols <- names(df)[start_idx:ncol(df)]
  rng_cols <- rng_cols[.is_num[start_idx:ncol(df)]]
  
  if (length(rng_cols)) {
    df <- df %>%
      mutate(across(
        all_of(rng_cols),
        ~ {
          bad <- . > 10 | . <= -1
          if (is.integer(.)) replace(., bad, NA_integer_) else replace(., bad, NA_real_)
        }
      ))
  } else {
    message(sprintf("No numeric columns at/after '%s'. Skipped out-of-range recode.", start_col))
  }
} else {
  message(sprintf("Column '%s' not found. Skipped out-of-range recode.", start_col))
}

# ---- 2) Detect numeric columns that are (now) coded in {1..6} (no zeros) ----
# We allow "noise" values to have been removed above; if some remain within 7..10 they
# will disqualify the column (by design). Adjust logic if needed.
.int_like <- function(x) {
  nx <- x[!is.na(x)]
  if (!length(nx)) return(FALSE)
  all(abs(nx - round(nx)) < 1e-8)
}

.is_codes_1_to_6 <- function(x) {
  # assumes numeric
  nx <- x[!is.na(x)]
  if (!length(nx)) return(FALSE)
  .int_like(nx) &&
    min(nx) >= 1 && max(nx) <= 6 &&
    any(nx == 6) # must actually contain a 6 to bother recoding it
}

num_cols <- names(df)[.is_num]
cols_with_6 <- num_cols[vapply(df[num_cols], .is_codes_1_to_6, logical(1))]

# Optional exclusions
vars_to_exclude <- c("ch02", "ch03")
cols_with_6 <- setdiff(cols_with_6, intersect(vars_to_exclude, names(df)))

# ---- 3) Recode category 6 -> NA (type-safe numeric) ----
if (length(cols_with_6)) {
  df <- df %>%
    mutate(across(
      all_of(cols_with_6),
      ~ if (is.integer(.)) replace(., . == 6L, NA_integer_) else replace(., . == 6, NA_real_)
    ))
} else {
  message("No numeric columns with codes limited to {1..6} found (after exclusions).")
}

# ---- 4) Custom missing-category recodes (numeric-only on existing targets) ----
missing_cats <- list(
  "7" = c("ch06"),
  "5" = c("wl06", "zw01_0", "zw01_1", "zw01_2", "zw01_3", "zw01_4", "zw01_5", "zw01_6", "zw04", "zw09"),
  "4" = c("wl14", "zw03", "zw07", "zw10_0", "zw10_1", "zw10_2")
  # add "1" bucket back when needed
)

if (length(missing_cats)) {
  for (cat_val in names(missing_cats)) {
    targets <- intersect(missing_cats[[cat_val]], names(df))
    if (!length(targets)) {
      message(sprintf("No matching columns for category %s. Skipped.", cat_val))
      next
    }
    targets <- targets[.is_num[match(targets, names(df))]]  # numeric-only
    if (!length(targets)) next
    
    v <- as.numeric(cat_val)
    df <- df %>%
      mutate(across(
        all_of(targets),
        ~ if (is.integer(.)) replace(., . == as.integer(v), NA_integer_) else replace(., . == v, NA_real_)
      ))
  }
}

# ---- 5) Safety and finish ----
stopifnot(nrow(df) == n0)
df <- as.data.frame(df)
message("Missing value processing complete.")
