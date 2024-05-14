
#-----------------------------------------------------------------------------------------------

#Missing

#-----------------------------------------------------------------------------------------------

message("Missing values...")

#missing values analysis
#colSums(is.na(df))

# Replace values greater than 10 and less than or equal to -1 with NA for columns from 8 to the last column
df <- df %>%
  mutate(across(8:ncol(df), ~ ifelse(. > 10 | . <= -1, NA, .)))

# Function to get the column position of a variable
get_var_position <- function(df, var_name) {
  which(names(df) == var_name)
}

# Updated function to identify columns with maximum value, handling columns with all NAs
colMax <- function(df) sapply(df, function(col) if (all(is.na(col))) NA else max(col, na.rm = TRUE))

# Identify columns with exactly 6 categories
cols_with_6_cats <- which(colMax(df) == 6)

# Exclude specific columns 'leeftijd' and 'opleiding' from recoding
cols_to_exclude <- c(get_var_position(df, "ch02"), get_var_position(df, "ch03"))
cols_with_6_cats <- cols_with_6_cats[!names(df)[cols_with_6_cats] %in% cols_to_exclude]

# Recode 6th category to NA for variables with exactly 6 categories
df[, cols_with_6_cats] <- lapply(df[, cols_with_6_cats], function(x) ifelse(x == 6, NA, x))

# Recode specific categories to NA
missing_cats <- list(
  "7" = c('ch06'),
  "5" = c('wl06', 'zw01_0', 'zw01_1', 'zw01_2', 'zw01_3', 'zw01_4', 'zw01_5', 'zw01_6', 'zw04', 'zw09'),
  "4" = c('wl14', 'zw03', 'zw07', 'zw10_0', 'zw10_1', 'zw10_2'),
  "1" = c('zw13_0', 'zw13_1', 'zw13_10', 'zw13_2', 'zw13_3', 'zw13_4', 'zw13_5', 'zw13_6', 'zw13_7', 'zw13_8', 'zw13_9', 'zw08_0', 'zw08_1', 'zw08_2', 'zw08_3', 'zw08_4', 'zw08_5', 'zw08_6', 'zw08_7')
)

# Recode missing categories based on the specified positions
for (cat_val in names(missing_cats)) {
  df <- df %>%
    mutate(across(all_of(missing_cats[[cat_val]]), ~ ifelse(. == as.numeric(cat_val), NA, .)))
}

# Ensure df remains a data frame
df <- as.data.frame(df)
