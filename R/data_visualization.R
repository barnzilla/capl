#' Render a bar plot for a given CAPL-2 domain score, grouped by CAPL-2 interpretative categories.
#'
#' This function renders a bar plot for a given CAPL-2 domain score, grouped by CAPL-2 interpretative categories.
#'
#' @export
#'
#' @importFrom dplyr group_by summarize
#' @importFrom ggplot2 aes element_text geom_bar geom_text ggplot margin position_dodge scale_fill_manual theme theme_minimal 
#' xlab ylab    
#' @importFrom magrittr %>%
#' @importFrom stats complete.cases var
#'
#' @param score A numeric vector.
#' @param interpretation A character vector representing CAPL-2 interpretative categories ("beginning", "progressing", "achieving", "excelling").
#' @param x_label A character vector representing the x-axis label. This argument is set to "Interpretation" by default.
#' @param y_label A character vector representing the y-axis label. This argument is set to "Score" by default.
#' @param colors A character vector representing the color palette for the bars. This argument is set to CAPL-2 branding colors by default
#' (c("#333376", "#00a79d", "#f26522", "#a6ce39", "#747474")).
#'
#' @details
#' Other `capl` functions called by this function include: [validate_character()], [validate_number()] and [capitalize_character()].
#'
#' @examples
#' capl_results <- get_capl_demo_data(n = 25)
#'
#' get_score_by_interpretation_plot(
#'   score = capl_results$capl_score, 
#'   interpretation = capl_results$capl_interpretation,
#'   x_label = "Overall physical literacy interpretation",
#'   y_label = "Overall physical literacy score",
#' )
#'
#' @return Renders a ggplot2 bar plot (if valid).
get_score_by_interpretation_plot <- function(score = NA, interpretation = NA, x_label = "Interpretation", y_label = "Score", colors = c("#333376", "#00a79d", "#f26522", "#a6ce39")) {
  colors <- validate_character(colors)
  if(sum(is.na(colors)) > 0) {
    colors <- c("#333376", "#00a79d", "#f26522", "#a6ce39")
  }
  try(
    if(var(c(length(score), length(interpretation))) != 0 ) {
      stop("[CAPL error]: the score and interpretation arguments must be the same length.")
    } else if(sum(! unique(interpretation) %in% NA) != length(colors)) {
      stop("[CAPL error]: the number of unique values in the interpretation argument and the number of values in the colors argument must be the same length.")
    }  else {
      score <- validate_number(score)
      interpretation <- capitalize_character(interpretation)
      x_label <- validate_character(x_label[1])
      y_label <- validate_character(y_label[1])
      if(is.na(x_label)) {
        x_label <- "Interpretation"
      }
      if(is.na(y_label)) {
        y_label <- "Score"
      }
      df <- data.frame(score, interpretation)
      df$interpretation <- factor(df$interpretation, levels = c("Beginning", "Progressing", "Achieving", "Excelling"))
      df <- df %>% group_by(interpretation) %>% summarize(mean = mean(score), .groups = "drop")
      plot <- ggplot(
        data = df[complete.cases(df),],
        aes(
          x = interpretation,
          y = mean,
          fill = interpretation
        )
      ) + geom_bar(position = "dodge", stat = "identity") +
        geom_text(size = 3.25, aes(label = format(round(mean, 1), nsmall = 1)), position = position_dodge(width = 0.9), vjust = -0.5) +
        xlab(x_label) +
        ylab(y_label) +
        scale_fill_manual(values = colors) +
        theme_minimal() +
        theme(legend.position = "none") +
        theme(axis.title.x = element_text(margin = margin(t = 20, r = 0, b = 0, l = 0))) +
        theme(axis.title.y = element_text(margin = margin(t = 0, r = 20, b = 0, l = 0)))
      return(plot)
    }
  )
}