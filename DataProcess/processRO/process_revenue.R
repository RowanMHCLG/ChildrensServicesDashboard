###############################################################################
##                             * REVENUE *                                   ##
###############################################################################
# Author:     Becky Foster
# Script Description: Revenue Outturn inputs
# -----------------------------------------------------------------------------

# Load in data (this can take some time!)
revenue_data_original <- read_csv(revenue_path)

# Check for missing authorities by year
table(revenue_data_original$missing_outturn, revenue_data_original$year)


# Remove summary rows (we will add these back in later)
revenue_data_original <- revenue_data_original %>%
  filter(!ecode =="Eng") 

# Check that the missing authorities match the numbers in the RO technical release
revenue_data_original %>% filter(missing_outturn==1) %>% reframe(authority, year)

# Label the identifying columns in the database
identifying_revenue <- c("ons_code", "year", "authority", "class", "region")       

## IMPUTED REVENUE DATABASE ----
## Define the variables we want to impute for ----
lgf_vars <- c("RS_NCE_AdultSocialCare", "RS_NCE_ChildrenSocialCare_adj2", "RS_NCE_Highwaysandtransportse", 
              "RS_NCE_HousingservicesGFRAon", "RS_NCE_Culturalandrelatedserv", "RS_NCE_Environmentalandregulat",
              "RS_NCE_Planninganddevelopment", "RS_NCE_Fireandrescueservices", "RS_NCE_Eduservices_adj2nosc", 
              "RS_NCE_Centralservices", "RS_NCE_Otherservices", "RS_Other_IntegratedTransportAu", 
              "RS_Other_WasteDisposalAuthorit", "RS_NCE_PublicHealth")



variables_to_impute <- c(lgf_vars)
variables_to_impute <- unique(variables_to_impute)

fire_variables_exclusive <- names(revenue_data_original) %>% 
  grep("fire", ., ignore.case = TRUE, value = TRUE)

# we want to force these to be 0
grant_variables <- names(revenue_data_original) %>%
  grep("^RG_", ., value = TRUE)


#temporarily having the growth ratio in here manually, as the function doesn't appear to be using the most recent one. ----
calculate_growth_ratios <- function(df, variables_to_impute) {
  
  # Capture start time to evaluate performance
  start_time <- Sys.time()
  
  # Filter out rows with missing outturn
  df_non_missing <- df %>%
    filter(missing_outturn == 0, year %in% outturn_years) %>%
    mutate(year_numeric = as.numeric(sub("/.*", "", year)))
  
  # Get unique years
  unique_years <- sort(unique(df_non_missing$year_numeric))
  
  print(paste("🗓️ The unique years identified are:", paste(unique_years, collapse = ", ")))
  
  # Check that the first year is a full dataset
  first_year <- unique_years[1]
  first_year_str <- paste0(first_year, "/", substr(first_year + 1, 3, 4))
  first_year_complete <- df %>%
    filter(year == first_year_str) %>%
    summarise(all_complete = all(missing_outturn == 0))
  
  if (!first_year_complete$all_complete) {
    warning(paste("⚠️ The first year", first_year_str, "does not have 100% missing_outturn = 0. Imputation may not work correctly."))
  }
  
  # Initialize an empty dataframe for storing growth ratios
  growth_ratios <- data.frame(year = character(), class = character(), variable = character(), growth_ratio = numeric(), stringsAsFactors = FALSE)
  old_variables <- list()
  new_variables <- list()
  
  # Identify old and new variables at the England level
  england_sums <- df_non_missing %>%
    group_by(year) %>%
    summarise(across(all_of(variables_to_impute), ~ sum(.x, na.rm = TRUE)), .groups = "drop")
  
  for (i in 2:length(unique_years)) {
    current_year <- unique_years[i]
    previous_year <- unique_years[i - 1]
    
    # Convert years to year_str format
    current_year_str <- paste0(current_year, "/", substr(current_year + 1, 3, 4))
    previous_year_str <- paste0(previous_year, "/", substr(previous_year + 1, 3, 4))
    
    # Identify old and new variables using England-level totals
    current_year_sums <- england_sums %>% filter(year == current_year_str)
    previous_year_sums <- england_sums %>% filter(year == previous_year_str)
    
    old_vars <- names(current_year_sums)[
      (current_year_sums == 0 | is.na(current_year_sums)) & !(previous_year_sums == 0 | is.na(previous_year_sums))
    ]
    if (length(old_vars) > 0) old_variables[[current_year_str]] <- old_vars
    
    new_vars <- names(current_year_sums)[
      !(current_year_sums == 0 | is.na(current_year_sums)) & (previous_year_sums == 0 | is.na(previous_year_sums))
    ]
    if (length(new_vars) > 0) new_variables[[current_year_str]] <- new_vars
  }
  
  # Loop through years
  for (i in 2:length(unique_years)) {
    current_year <- unique_years[i]
    previous_year <- unique_years[i - 1]
    
    # Convert years to year_str format
    current_year_str <- paste0(current_year, "/", substr(current_year + 1, 3, 4))
    previous_year_str <- paste0(previous_year, "/", substr(previous_year + 1, 3, 4))
    
    # Create a progress bar for each year
    pb_year <- progress_bar$new(
      format = paste("Year", current_year_str, ":current [:bar] :percent ETA: :eta"),
      total = length(unique(df_non_missing$class)),
      clear = FALSE,
      width = 60
    )
    
    # Loop through classes for the specific year
    for (class_name in unique(df_non_missing$class)) {
      # Filter data for the specific class
      class_data <- df_non_missing %>% filter(class == class_name)
      
      # Filter data for the current and previous years, specific to the class
      current_year_data <- class_data %>% filter(year == current_year_str)
      previous_year_data <- class_data %>% filter(year == previous_year_str)
      
      # Get common authorities between the years
      common_authorities <- intersect(
        current_year_data %>% pull(ecode),
        previous_year_data %>% pull(ecode)
      )
      
      # If no common authorities, skip this iteration
      if (length(common_authorities) == 0) next
      
      # Summarize by class for both years
      current_year_sums <- current_year_data %>%
        filter(ecode %in% common_authorities) %>%
        summarise(across(all_of(variables_to_impute), ~ sum(.x, na.rm = TRUE)), .groups = "drop")
      previous_year_sums <- previous_year_data %>%
        filter(ecode %in% common_authorities) %>%
        summarise(across(all_of(variables_to_impute), ~ sum(.x, na.rm = TRUE)), .groups = "drop")
      
      # Loop through all variables and calculate the grossing ratio
      for (var in variables_to_impute) {
        if (var %in% new_variables[[current_year_str]]) {
          cat("❌ Skipping new variable in first year:", var, "in class:", class_name, "year:", current_year_str, "\n")
          growth_ratio <- NA_real_
          next  # Skip new variables only in their first year
        }
        if (var %in% old_variables[[current_year_str]]) {
          cat("❌ Skipping old variable after it no longer exists:", var, "in class:", class_name, "year:", current_year_str, "\n")
          growth_ratio <- NA_real_
          next  # Skip old variables only after they no longer exist
        }
        
        # Calculate the growth ratio
        growth_ratio <- ifelse(
          is.na(previous_year_sums[[var]]) | is.na(current_year_sums[[var]]), NA_real_,
          ifelse(previous_year_sums[[var]] == 0, NA_real_, current_year_sums[[var]] / previous_year_sums[[var]])
        ) #If either year's sum is NA, return NA. If the previous year's sum is 0, return NA instead of Inf or NaN to avoid problems
        
        # Store growth ratio in the results data frame
        growth_ratios <- bind_rows(growth_ratios, data.frame(
          year = current_year_str,
          class = class_name,
          variable = var,
          growth_ratio = growth_ratio
        ))
      }
      
      # Update the progress bar after processing each class for the current year
      pb_year$tick()
    }
  }
  
  # Pivot the data so that variables are columns
  growth_ratios_wide <- growth_ratios %>%
    pivot_wider(names_from = variable, values_from = growth_ratio)
  
  # Print identified old and new variables
  cat("\n⚙️ ========= Old Variables (Phased Out) =========\n")
  for (year in names(old_variables)) {
    cat("📅 Year:", year, "\nVariables:", paste(old_variables[[year]], collapse = ", "), "\n\n")
  }
  
  cat("\n📈 ========= New Variables (Introduced) =========\n")
  for (year in names(new_variables)) {
    cat("📅 Year:", year, "\nVariables:", paste(new_variables[[year]], collapse = ", "), "\n\n")
  }
  
  # Capture end time
  end_time <- Sys.time()
  
  # Calculate and print total running time
  total_time <- end_time - start_time
  cat("\n⏱️ Total running time (seconds):", total_time, "\n")
  
  return(list(growth_ratios = growth_ratios_wide, old_variables = old_variables, new_variables = new_variables))
  
}

## Produce growth ratios and sense check them ----
growth_ratios_result <- calculate_growth_ratios(revenue_data_original, variables_to_impute)
growth_ratios <- growth_ratios_result$growth_ratios
old_variables <- growth_ratios_result$old_variables
new_variables <- growth_ratios_result$new_variables

## Impute for missing values ----
revenue_data_imputed <- fosteR:: impute_missing_values(revenue_data_original, variables_to_impute, growth_ratios, old_variables, new_variables, fire_variables_exclusive, grant_variables)

## Return a dataset that only has identifying columns and the variables we have imputed for ----
#(helps to avoid accidentally using a variable we haven't imputed for)
revenue_data_imputed <- revenue_data_imputed %>% dplyr:: select(all_of(identifying_revenue) ,all_of(variables_to_impute)) %>% filter(year %in% all_outturn_years)


## Correct some incorrect ONS_codes
# North Yorkshire & Somerset
revenue_data_imputed <- revenue_data_imputed %>%
  mutate(ons_code = case_when(
    year >= "2023/24" & authority == "Somerset" ~ "E06000066",
    year >= "2023/24" & authority == "North Yorkshire" ~ "E06000065",
    TRUE ~ ons_code
  ))


# Remove no longer required environment objects
rm(growth_ratios, growth_ratios_result, new_variables, old_variables, revenue_data_original)
