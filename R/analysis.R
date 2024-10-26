#' Analyze voter turnout
#'
#' @param data A cleaned data frame containing voter data
#' @return A list containing comprehensive turnout analysis
#' @export
analyze_turnout <- function(data) {
  # Validate input
  if (!all(c("voter_id", "age", "party", "turnout", "age_group") %in% names(data))) {
    stop("Data must contain all required columns including age_group")
  }

  results <- list()

  # Overall turnout statistics
  results$overall <- list(
    total_voters = nrow(data),
    turnout_rate = mean(data$turnout, na.rm = TRUE),
    turnout_se = sd(data$turnout, na.rm = TRUE) / sqrt(nrow(data))
  )

  # Turnout by party
  results$party_turnout <- data %>%
    group_by(party) %>%
    summarise(
      n_voters = n(),
      n_voted = sum(turnout, na.rm = TRUE),
      turnout_rate = mean(turnout, na.rm = TRUE),
      turnout_se = sd(turnout, na.rm = TRUE) / sqrt(n())
    ) %>%
    arrange(desc(turnout_rate))

  # Turnout by age group
  results$age_turnout <- data %>%
    group_by(age_group) %>%
    summarise(
      n_voters = n(),
      n_voted = sum(turnout, na.rm = TRUE),
      turnout_rate = mean(turnout, na.rm = TRUE),
      turnout_se = sd(turnout, na.rm = TRUE) / sqrt(n()),
      mean_age = mean(age, na.rm = TRUE)
    ) %>%
    arrange(desc(turnout_rate))

  # Party by age group cross-tabulation
  results$party_age_turnout <- data %>%
    group_by(party, age_group) %>%
    summarise(
      n_voters = n(),
      turnout_rate = mean(turnout, na.rm = TRUE),
      .groups = 'drop'
    ) %>%
    arrange(party, age_group)

  return(results)
}
