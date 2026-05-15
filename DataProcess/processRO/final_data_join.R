###############################################################################
##                           * DATA JOIN *                              ##
###############################################################################


## RESHAPE EACH DATASET INTO LONG FORMAT ----


prepare_long_data <- function(data, id_cols) {
  data %>%
    pivot_longer(
      cols = -c(any_of(id_cols)),
      names_to = "variable",
      values_to = "value"
    ) %>%
    select(year, authority, class, variable, value, everything())
  
}

ROdata <- prepare_long_data(combined_data_calcs, identifying_revenue)


## REMOVE NO LONGER NEEDED FILES ----
rm(combined_data_calcs, fire_variables_exclusive, grant_variables, identifying_revenue, lgf_vars, variables_to_impute, revenue_data_imputed)
