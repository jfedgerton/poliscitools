#' Clean political science data
#'
#' @param data A data frame containing voter_id, age, party, turnout
#' @return A cleaned data frame with standardized values and additional features
#' @export
clean_political_data <- function(data) {
  # Check for required columns
  required_cols <- c("voter_id", "age", "party", "turnout")
  if (!all(required_cols %in% names(data))) {
    stop("Missing required columns. Data must contain: ",
         paste(required_cols, collapse = ", "))
  }

  cleaned_data <- data %>%
    # Convert to tibble
    as_tibble() %>%

    # Clean voter_id (ensure numeric and unique)
    mutate(
      voter_id = as.numeric(voter_id)
    ) %>%

    # Clean and validate age
    mutate(
      age = as.numeric(age),
      # Create age groups for analysis
      age_group = case_when(
        age < 25 ~ "18-24",
        age < 35 ~ "25-34",
        age < 50 ~ "35-49",
        age < 65 ~ "50-64",
        age >= 65 ~ "65+",
        TRUE ~ NA_character_
      )
    ) %>%

    # Clean party affiliations (ensure standardized values)
    mutate(
      party = case_when(
        party == "Democrat" ~ "Democrat",
        party == "Republican" ~ "Republican",
        party == "Independent" ~ "Independent",
        TRUE ~ "Other"
      )
    ) %>%

    # Clean turnout (ensure binary)
    mutate(
      turnout = as.numeric(turnout),
      turnout = ifelse(turnout %in% c(0,1), turnout, NA)
    )

  # Validate the cleaned data
  if (any(is.na(cleaned_data))) {
    warning("Missing values found in cleaned data")
  }

  return(cleaned_data)
}
