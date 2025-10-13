
#-----------------------------------------------------------------------------------------------

# Multiple response sets 

#-----------------------------------------------------------------------------------------------

suppressPackageStartupMessages({
  library(dplyr)
  library(tibble)
  library(purrr)
})

message("Processing multiple response sets...")

# --- Helpers ------------------------------------------------------------------

# Robust 0/1 detector -> numeric {0,1,NA}
coerce_01 <- function(x) {
  if (is.logical(x)) return(as.numeric(x))                 # TRUE/FALSE -> 1/0
  if (is.numeric(x)) return(ifelse(is.na(x), NA_real_, as.numeric(x)))
  x <- as.character(x)                                     # character/factor
  x <- trimws(x)
  x[x %in% c("TRUE","T","true","t")]  <- "1"
  x[x %in% c("FALSE","F","false","f")] <- "0"
  suppressWarnings(as.numeric(x))
}

# Weighted % of selected (=1) * 100 for 0/1 dummies
weighted_mean01 <- function(x, w, default = NA_real_) {
  x <- coerce_01(x)
  m <- !is.na(x)
  if (!any(m)) return(default)
  w <- ifelse(is.na(w), 0, w)
  sw <- sum(w[m])
  if (sw == 0) return(default)
  100 * sum(w[m] * (x[m] == 1)) / sw
}

# Compute a one-row tibble of *_pin columns in the given order
compute_mr_cpct <- function(df_i, vars, out_suffix = "_pin", weight_col = "weging",
                            default = NA_real_) {
  w <- if (weight_col %in% names(df_i)) df_i[[weight_col]] else rep(1, nrow(df_i))
  out_names <- paste0(vars, out_suffix)
  if (!all(vars %in% names(df_i))) {
    # Missing variables: return one row with defaults in the correct shape
    return(tibble::as_tibble(setNames(as.list(rep(default, length(out_names))), out_names)))
  }
  df_i |>
    dplyr::summarise(dplyr::across(dplyr::all_of(vars), ~ weighted_mean01(.x, w, default))) |>
    dplyr::rename_with(~ paste0(.x, out_suffix))
}

# Normalize to numeric, tolerating EU decimal formats and % signs
eu_numeric <- function(x) {
  if (is.numeric(x)) return(x)
  x <- as.character(x)
  x <- gsub("\\s+|%", "", x)
  has_comma <- grepl(",", x, fixed = TRUE)
  x[has_comma]  <- gsub("\\.", "", x[has_comma])                 # drop thousands dots
  x[has_comma]  <- gsub(",", ".", x[has_comma], fixed = TRUE)    # decimal comma -> dot
  x[!has_comma] <- gsub(",", "", x[!has_comma],  fixed = TRUE)   # drop thousands commas
  suppressWarnings(as.numeric(x))
}

# Ensure all *_pin columns are numeric
normalize_pin_cols <- function(df) {
  pin <- grepl("_pin$", names(df), perl = TRUE)
  if (any(pin)) df[pin] <- lapply(df[pin], eu_numeric)
  df
}

# Vectorised row-wise recode: if all non-NA *_pin in a row are 0 or 100 -> set those to NA
recode_rows_all0_or_100_to_na <- function(df, tol = 1e-8, verbose = TRUE) {
  df <- normalize_pin_cols(df)
  pin_idx <- which(grepl("_pin$", names(df), perl = TRUE))
  if (!length(pin_idx) || nrow(df) == 0) return(df)
  
  M  <- as.matrix(df[, pin_idx, drop = FALSE])
  nz <- !is.na(M)
  any_nz   <- rowSums(nz) > 0
  all_zero <- any_nz & (rowSums(abs(M - 0)   <= tol, na.rm = TRUE) == rowSums(nz))
  all_hund <- any_nz & (rowSums(abs(M - 100) <= tol, na.rm = TRUE) == rowSums(nz))
  mask_row <- all_zero | all_hund
  
  if (any(mask_row)) {
    rows <- which(mask_row)
    # set only the *_pin cells in those rows to NA
    M_sub <- M[rows, , drop = FALSE]
    nz_sub <- nz[rows, , drop = FALSE]
    M_sub[nz_sub] <- NA_real_
    M[rows, ] <- M_sub
    df[, pin_idx] <- M
    
    if (verbose && "GEOYR" %in% names(df)) {
      message("Row-wise recode to NA (all 0 or all 100) for GEOYR: ",
              paste(unique(df$GEOYR[rows]), collapse = ", "))
    } else if (verbose) {
      message("Row-wise recode to NA (all 0 or all 100) for rows: ",
              paste(rows, collapse = ", "))
    }
  }
  df
}

# --- Build GEOYR key -----------------------------------------------------------

df$GEOYR <- paste0(df$GEOITEM, "-", df$PERIOD)
gy <- unique(df$GEOYR[!is.na(df$GEOYR)])

# --- Iterate per GEOYR ---------------------------------------------------------

iter_vw <- list()
iter_vl <- list()
iter_bh <- list()

for (i in gy) {
  message(paste0("▶️ Processing ", i))
  df_i <- dplyr::filter(df, GEOYR == i)
  
  # Domains
  vw_vars <- paste0("zw08_", 0:7)   # Vrijwilligerswerk
  vl_vars <- paste0("zw05_", 0:5)   # Verenigingsleven
  bh_vars <- paste0("zw13_", 0:10)  # Burenhulp
  
  vw <- compute_mr_cpct(df_i, vw_vars) |> dplyr::mutate(GEOYR = i)
  vl <- compute_mr_cpct(df_i, vl_vars) |> dplyr::mutate(GEOYR = i)
  bh <- compute_mr_cpct(df_i, bh_vars) |> dplyr::mutate(GEOYR = i)
  
  iter_vw[[i]] <- vw
  iter_vl[[i]] <- vl
  iter_bh[[i]] <- bh
}

# --- Combine per domain + normalize columns -----------------------------------

vw_set <- dplyr::bind_rows(iter_vw) |> normalize_pin_cols()
vl_set <- dplyr::bind_rows(iter_vl) |> normalize_pin_cols()
bh_set <- dplyr::bind_rows(iter_bh) |> normalize_pin_cols()

# --- Merge all domains on GEOYR -----------------------------------------------

mr_sets <- purrr::reduce(list(vw_set, vl_set, bh_set), dplyr::full_join, by = "GEOYR")

# --- Single consolidated row-wise recode across ALL *_pin columns --------------

mr_sets <- recode_rows_all0_or_100_to_na(mr_sets)

message("✅ Multiple response sets processing complete.")
