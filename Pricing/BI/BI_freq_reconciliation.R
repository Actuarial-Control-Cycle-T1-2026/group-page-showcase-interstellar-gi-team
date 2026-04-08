library(dplyr)
library(purrr)
library(tidyr)
library(tibble)

data_check <- function(df, ignore_cols = c("policy_id", "worker_id")) {
  
  df2 <- df %>% dplyr::select(-any_of(ignore_cols))
  
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

# Run check
res <- data_check(bi_freq_model_clean)

# 🔥 IMPORTANT: explicitly print
cat("\n================ NUMERIC SUMMARY ================\n")
print(res$numeric_summary)

cat("\n================ CHARACTER SUMMARY ================\n")
print(res$char_summary)