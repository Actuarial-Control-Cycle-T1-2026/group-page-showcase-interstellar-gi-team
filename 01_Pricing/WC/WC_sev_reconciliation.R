library(dplyr)
library(purrr)
library(tidyr)
library(tibble)

# =========================================================
# Data check: wc_sev_model_clean
# Ignore ID + non-modelling fields
# =========================================================

ignore_cols <- c("claim_id", "claim_seq", "claim_count", "policy_id", "worker_id")

data_check <- function(df, ignore_cols) {
  
  df2 <- df %>% dplyr::select(-any_of(ignore_cols))
  
  # -------------------------
  # Numeric summary
  # -------------------------
  numeric_summary <- df2 %>%
    dplyr::select(where(is.numeric)) %>%
    summarise(
      across(
        everything(),
        list(
          min = ~ min(.x, na.rm = TRUE),
          max = ~ max(.x, na.rm = TRUE)
        ),
        .names = "{.col}_{.fn}"
      )
    ) %>%
    pivot_longer(
      cols = everything(),
      names_to = c("variable", ".value"),
      names_sep = "_(?=[^_]+$)"
    )
  
  # -------------------------
  # Character / factor summary
  # -------------------------
  char_summary <- df2 %>%
    dplyr::select(where(~ is.character(.x) || is.factor(.x))) %>%
    map_df(
      ~ tibble(unique_values = paste(sort(unique(as.character(.x))), collapse = ", ")),
      .id = "variable"
    )
  
  list(
    numeric_summary = numeric_summary,
    char_summary = char_summary
  )
}

# =========================================================
# Run check
# =========================================================
res <- data_check(wc_sev_model_clean, ignore_cols)

# =========================================================
# Print for Rmd output
# =========================================================

cat("\n================ SEVERITY DATA: NUMERIC SUMMARY ================\n")
print(res$numeric_summary)

cat("\n================ SEVERITY DATA: CHARACTER SUMMARY ================\n")
print(res$char_summary)