
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

for (i in gy) {
  
  message(paste0("▶️ Processing ", i))
  df_i <- df %>% filter(GEOYR == i)
  
  # ------------------------------
  # Vrijwilligerswerk
  vw_valid <- c("zw08_0_pin", "zw08_1_pin", "zw08_2_pin", "zw08_3_pin",
                "zw08_4_pin", "zw08_5_pin", "zw08_6_pin", "zw08_7_pin")
  vw_vars <- paste0("zw08_", 0:7)
  
  if (check_vars_exist(vw_vars, df_i)) {
    vw_mr <- tryCatch({
      df_i %>%
        tab_cells(mdset(zw08_0 %to% zw08_7)) %>%
        tab_weight(weging) %>%
        tab_stat_cpct() %>%
        tab_pivot()
    }, error = function(e) {
      warning(paste0("❌ Skipped VW for ", i, ": ", e$message))
      NULL
    })
    
    if (!is.null(vw_mr)) {
      vw <- as_tibble(vw_mr[1:length(vw_valid), 2]) %>% t()
      colnames(vw) <- vw_valid
      vw <- as_tibble(vw)
    } else {
      vw <- as_tibble(setNames(as.list(rep(NA, length(vw_valid))), vw_valid))
    }
  } else {
    warning(paste0("⚠️ Missing VW vars for ", i, "."))
    vw <- as_tibble(setNames(as.list(rep(NA, length(vw_valid))), vw_valid))
  }
  vw$GEOYR <- i
  iter_vw[[i]] <- vw
  
  # ------------------------------
  # Verenigingsleven
  vl_valid <- c("zw05_pin0", "zw05_pin1", "zw05_pin2", "zw05_pin3", "zw05_pin4", "zw05_pin5")
  vl_vars <- paste0("zw05_", 0:5)
  
  if (check_vars_exist(vl_vars, df_i)) {
    vl_mr <- tryCatch({
      df_i %>%
        tab_cells(mdset(zw05_0 %to% zw05_5)) %>%
        tab_weight(weging) %>%
        tab_stat_cpct() %>%
        tab_pivot()
    }, error = function(e) {
      warning(paste0("❌ Skipped VL for ", i, ": ", e$message))
      NULL
    })
    
    if (!is.null(vl_mr)) {
      vl <- as_tibble(vl_mr[1:length(vl_valid), 2]) %>% t()
      colnames(vl) <- vl_valid
      vl <- as_tibble(vl)
    } else {
      vl <- as_tibble(setNames(as.list(rep(NA, length(vl_valid))), vl_valid))
    }
  } else {
    warning(paste0("⚠️ Missing VL vars for ", i, "."))
    vl <- as_tibble(setNames(as.list(rep(NA, length(vl_valid))), vl_valid))
  }
  vl$GEOYR <- i
  iter_vl[[i]] <- vl
  
  # ------------------------------
  # Burenhulp
  bh_valid <- c("zw13_0_pin", "zw13_1_pin", "zw13_10_pin", "zw13_2_pin", "zw13_3_pin",
                "zw13_4_pin", "zw13_5_pin", "zw13_6_pin", "zw13_7_pin", "zw13_8_pin", "zw13_9_pin")
  bh_vars <- paste0("zw13_", c(0:9, 10))
  
  if (check_vars_exist(bh_vars, df_i)) {
    bh_mr <- tryCatch({
      df_i %>%
        tab_cells(mdset(zw13_0 %to% zw13_9)) %>%
        tab_weight(weging) %>%
        tab_stat_cpct() %>%
        tab_pivot()
    }, error = function(e) {
      warning(paste0("❌ Skipped BH for ", i, ": ", e$message))
      NULL
    })
    
    if (!is.null(bh_mr)) {
      bh <- as_tibble(bh_mr[1:length(bh_valid), 2]) %>% t()
      colnames(bh) <- bh_valid
      bh <- as_tibble(bh)
    } else {
      bh <- as_tibble(setNames(as.list(rep(NA, length(bh_valid))), bh_valid))
    }
  } else {
    warning(paste0("⚠️ Missing BH vars for ", i, "."))
    bh <- as_tibble(setNames(as.list(rep(NA, length(bh_valid))), bh_valid))
  }
  bh$GEOYR <- i
  iter_bh[[i]] <- bh
}

# Combine
vw_set <- bind_rows(iter_vw)
vl_set <- bind_rows(iter_vl)
bh_set <- bind_rows(iter_bh)

# Merge on GEOYR
mr_sets <- reduce(list(vw_set, vl_set, bh_set), full_join, by = "GEOYR")

message("✅ Multiple response sets processing complete.")
