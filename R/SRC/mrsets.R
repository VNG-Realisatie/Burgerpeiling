#-----------------------------------------------------------------------------------------------
# Multiple response sets
#-----------------------------------------------------------------------------------------------

message("Processing multiple response sets...")

df$GEOYR <- paste0(df$GEOITEM, "-", df$PERIOD)
gy <- unique(df$GEOYR)

iter_vw <- list()
iter_vl <- list()
iter_bh <- list()

check_vars_exist <- function(vars, df) {
  all(vars %in% names(df))
}

# Helper: weighted % selected (=1) * 100 for 0/1 dummies
weighted_mean01 <- function(x, w, default = NA_real_) {
  m <- !is.na(x)
  if (!any(m)) return(default)
  sw <- sum(w[m], na.rm = TRUE)
  if (sw == 0) return(default)
  100 * sum(w[m] * (x[m] == 1), na.rm = TRUE) / sw
}

# Helper: compute a one-row tibble of *_pin columns in the given order
compute_mr_cpct <- function(df_i, vars, out_suffix = "_pin", weight_col = "weging",
                            default = NA_real_) {
  w <- if (weight_col %in% names(df_i)) df_i[[weight_col]] else rep(1, nrow(df_i))
  if (!all(vars %in% names(df_i))) {
    # Missing variables: return one row with defaults
    out_names <- paste0(vars, out_suffix)
    return(tibble::as_tibble(setNames(as.list(rep(default, length(out_names))), out_names)))
  }
  
  df_i |>
    dplyr::summarise(
      dplyr::across(
        dplyr::all_of(vars),
        ~ weighted_mean01(.x, w, default = default)
      )
    ) |>
    dplyr::rename_with(~ paste0(.x, out_suffix))
}

for (i in gy) {
  message(paste0("▶️ Processing ", i))
  df_i <- dplyr::filter(df, GEOYR == i)
  
  # ------------------------------
  # Vrijwilligerswerk (zw08_0..zw08_7)
  vw_vars  <- paste0("zw08_", 0:7)
  vw_valid <- paste0(vw_vars, "_pin")
  
  if (check_vars_exist(vw_vars, df_i)) {
    vw <- compute_mr_cpct(df_i, vw_vars) |> dplyr::mutate(GEOYR = i)
  } else {
    warning(paste0("⚠️ Missing VW vars for ", i, "."))
    vw <- tibble::as_tibble(setNames(as.list(rep(NA_real_, length(vw_valid))), vw_valid)) |>
      dplyr::mutate(GEOYR = i)
  }
  iter_vw[[i]] <- vw
  
  # ------------------------------
  # Verenigingsleven (zw05_0..zw05_5)
  vl_vars  <- paste0("zw05_", 0:5)
  vl_valid <- paste0(vl_vars, "_pin")  # keep consistent *_pin naming
  
  if (check_vars_exist(vl_vars, df_i)) {
    vl <- compute_mr_cpct(df_i, vl_vars) |> dplyr::mutate(GEOYR = i)
  } else {
    warning(paste0("⚠️ Missing VL vars for ", i, "."))
    vl <- tibble::as_tibble(setNames(as.list(rep(NA_real_, length(vl_valid))), vl_valid)) |>
      dplyr::mutate(GEOYR = i)
  }
  iter_vl[[i]] <- vl
  
  # ------------------------------
  # Burenhulp (zw13_0..zw13_10)
  bh_vars  <- paste0("zw13_", 0:10)
  bh_valid <- paste0(bh_vars, "_pin")
  
  if (check_vars_exist(bh_vars, df_i)) {
    bh <- compute_mr_cpct(df_i, bh_vars) |> dplyr::mutate(GEOYR = i)
  } else {
    warning(paste0("⚠️ Missing BH vars for ", i, "."))
    bh <- tibble::as_tibble(setNames(as.list(rep(NA_real_, length(bh_valid))), bh_valid)) |>
      dplyr::mutate(GEOYR = i)
  }
  iter_bh[[i]] <- bh
}

# Combine
vw_set <- dplyr::bind_rows(iter_vw)
vl_set <- dplyr::bind_rows(iter_vl)
bh_set <- dplyr::bind_rows(iter_bh)

# -------- Normalize *_pin columns to numeric (EU formats) + recode all-0/100 to NA -----

# Coerce vector x to numeric:
# - trims spaces
# - removes % signs
# - handles thousand sep ('.' or ',') and decimal mark (',' or '.')
eu_numeric <- function(x) {
  if (is.numeric(x)) return(x)
  x <- as.character(x)
  
  # trim spaces and remove % signs
  x <- gsub("\\s+", "", x)
  x <- gsub("%", "", x, fixed = TRUE)
  
  # Two main cases:
  # 1) Comma as decimal (e.g., "1.234,56" or "0,00")
  # 2) Dot as decimal (e.g., "1,234.56" or "0.00")
  
  has_comma <- grepl(",", x, fixed = TRUE)
  # Case 1: decimal comma → remove thousands '.', then replace ',' with '.'
  x[has_comma] <- {
    y <- x[has_comma]
    y <- gsub("\\.", "", y)        # drop thousands dots
    y <- gsub(",", ".", y, fixed = TRUE) # decimal comma -> dot
    y
  }
  # Case 2: decimal dot → remove thousands ',' (if any)
  x[!has_comma] <- {
    y <- x[!has_comma]
    y <- gsub(",", "", y, fixed = TRUE)  # drop thousands commas
    y
  }
  
  suppressWarnings(as.numeric(x))
}

# Decide if a numeric vector is "all 0" or "all 100" (ignoring NA)
col_all_zero_or_100 <- function(x, tol = 1e-8) {
  if (!is.numeric(x)) return(FALSE)
  nz <- !is.na(x)
  if (!any(nz)) return(FALSE)
  all(abs(x[nz] - 0) <= tol) || all(abs(x[nz] - 100) <= tol)
}

# Apply to all *_pin columns in a data frame
normalize_and_recode <- function(df, tol = 1e-8, verbose = TRUE) {
  pin_cols <- names(df)[grepl("_pin$", names(df), perl = TRUE)]
  if (!length(pin_cols)) return(df)
  
  recoded <- character(0)
  for (cn in pin_cols) {
    # Normalize to numeric
    df[[cn]] <- eu_numeric(df[[cn]])
    # Recode if all 0 or all 100
    if (col_all_zero_or_100(df[[cn]], tol = tol)) {
      df[[cn]][!is.na(df[[cn]])] <- NA_real_
      recoded <- c(recoded, cn)
    }
  }
  if (verbose && length(recoded)) {
    message("Re-coded to NA (all 0 or all 100): ", paste(recoded, collapse = ", "))
  }
  df
}

vw_set  <- normalize_and_recode(vw_set)
vl_set  <- normalize_and_recode(vl_set)
bh_set  <- normalize_and_recode(bh_set)


# --- Helpers ------------------------------------------------------------------

# Normalize a vector to numeric, tolerating EU decimal formats and % signs
eu_numeric <- function(x) {
  if (is.numeric(x)) return(x)
  x <- as.character(x)
  x <- gsub("\\s+", "", x)     # strip spaces
  x <- gsub("%", "", x, fixed = TRUE)
  has_comma <- grepl(",", x, fixed = TRUE)
  x[has_comma]  <- gsub("\\.", "", x[has_comma])         # drop thousands dots
  x[has_comma]  <- gsub(",", ".", x[has_comma], fixed=TRUE)  # decimal comma -> dot
  x[!has_comma] <- gsub(",", "", x[!has_comma], fixed=TRUE)  # drop thousands commas
  suppressWarnings(as.numeric(x))
}

# Ensure all *_pin columns are numeric
normalize_pin_cols <- function(df) {
  pin <- grepl("_pin$", names(df), perl = TRUE)
  if (any(pin)) df[pin] <- lapply(df[pin], eu_numeric)
  df
}

# Recode ENTIRE ROW to NA for *_pin columns if all non-NA in that row are 0 or 100
recode_rows_all0_or_100_to_na <- function(df, tol = 1e-8, verbose = TRUE) {
  df <- normalize_pin_cols(df)
  pin_idx <- which(grepl("_pin$", names(df), perl = TRUE))
  if (!length(pin_idx)) return(df)
  
  recoded_rows <- integer(0)
  for (r in seq_len(nrow(df))) {
    x <- unlist(df[r, pin_idx], use.names = FALSE)
    nz <- !is.na(x)
    if (!any(nz)) next
    all0   <- all(abs(x[nz] - 0)   <= tol)
    all100 <- all(abs(x[nz] - 100) <= tol)
    if (all0 || all100) {
      df[r, pin_idx][nz] <- NA_real_
      recoded_rows <- c(recoded_rows, r)
    }
  }
  
  if (verbose && length(recoded_rows)) {
    if ("GEOYR" %in% names(df)) {
      msg_keys <- unique(df$GEOYR[recoded_rows])
      message("Row-wise recode to NA (all 0 or all 100) for GEOYR: ",
              paste(msg_keys, collapse = ", "))
    } else {
      message("Row-wise recode to NA (all 0 or all 100) for rows: ",
              paste(unique(recoded_rows), collapse = ", "))
    }
  }
  df
}

# --- Apply to each set ---------------------------------------------------------

vw_set  <- recode_rows_all0_or_100_to_na(vw_set)
vl_set  <- recode_rows_all0_or_100_to_na(vl_set)
bh_set  <- recode_rows_all0_or_100_to_na(bh_set)
mr_sets <- recode_rows_all0_or_100_to_na(mr_sets)

# Optional: if you want to SEE dots instead of commas when printing
# options(OutDec = ".")



# Merge on GEOYR
mr_sets <- purrr::reduce(list(vw_set, vl_set, bh_set), dplyr::full_join, by = "GEOYR")




message("✅ Multiple response sets processing complete.")
