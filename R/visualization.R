#' Visualize party distribution
#'
#' @param data A data frame containing party affiliation data
#' @return A ggplot object
#' @export
visualize_party_distribution <- function(data) {
  # Your visualization logic here
  data_plot <- aggregate(data, turnout ~ party, FUN = "sum")
  ggplot(data_plot, aes(x = party, y = turnout, fill = party)) +
    geom_col() +
    theme_bw() +
    scale_fill_manual(values = c("blue", "gray", "red")) +
    theme(legend.position = "none") +
    labs(x = "", y = "Turnout count")
}
