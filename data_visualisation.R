source("packages_manager.R")
source("helper_functions.R")
source("load_data.R")

make_plot <- function(dates, predictions, observations, msfe, label, scale_y) {
  colors <-
    c(
      "Predictions" = "dark green",
      "Observations" = "steelblue"
    )
  figure <- ggplot() +
    ggtitle(label) +
    # Draw predictions line
    geom_line(
      data = data.frame(dates, predictions),
      aes(
        x = as.Date(dates),
        y = as.numeric(predictions),
        color = "Predictions"
      ),
      size = 1
    ) +

    # Draw observations line
    geom_line(
      data = data.frame(dates, observations),
      aes(
        x = as.Date(dates),
        y = observations,
        color = "Observations"
      ),
      size = 1
    ) +

    # Change x and y titles
    labs(x = "Forecast Date", y = "GDP Growth", color = "Legend") +

    # Set x breaks and the desired format for the date labels
    scale_x_date(date_breaks = "3 months", date_labels = "%m-%Y") +

    # Apply colours
    scale_color_manual(values = colors) +

    # Rotate x axis labels by 45 degrees
    theme(axis.text.x = element_text(angle = 45), axis.title.x = element_blank()) +

    # Add MSFE to the graph
    annotate(
      geom = "text",
      x = as.Date(min(dates) + 180),
      y = scale_y[1] + 5,
      label = glue("MSFE: {round(msfe, digits = 4)}")
    ) +

    # sets a standard scale for the y-axis
    scale_y_continuous(limits = scale_y)
  return(figure)
}

plot_var_imp <- function(model, rows, label) {
  var_imp_df <- varImp(model)$importance
  var_imp_df <- var_imp_df[order(-var_imp_df$Overall), , drop = FALSE]

  # TikZDevice breaks if we use underscores. Replace £ with Find All in Overleaf
  var_imp_df$Variables <- gsub("_", "£", rownames(var_imp_df))

  p <- head(var_imp_df, rows) %>%
    arrange(Overall) %>% # First sort by val. This sort the dataframe but NOT the factor levels
    mutate(Variables = factor(Variables, levels = Variables)) %>% # This trick update the factor levels
    ggplot(aes(x = Variables, y = Overall / 100)) +
    geom_bar(stat = "identity") +
    coord_flip() +
    theme_bw() +
    xlab("") +
    ylab("
    Absolute Coefficients.
    Scaled to be between 0 and 1.
    ") +
    ggtitle(label)

  return(p)
}

box_plots <- function(data) {
  TEX <- TRUE
  width <- 6.3

  cols_0_1_25 <- c()
  cols_0_7_5 <- c()
  cols_5 <- c()
  cols_50 <- c()
  cols_big <- c()
  for (col in 1:ncol(data[, -c(1)])) {
    col <- col + 1
    if (min(data[, c(col)]) >= -0.125 & max(data[, c(col)]) <= 0.125) {
      cols_0_1_25 <- append(cols_0_1_25, col)
    } else if (min(data[, c(col)]) >= -0.75 & max(data[, c(col)]) <= 0.75) {
      cols_0_7_5 <- append(cols_0_7_5, col)
    } else if (min(data[, c(col)]) >= -5 & max(data[, c(col)]) <= 5) {
      cols_5 <- append(cols_5, col)
    } else if (min(data[, c(col)]) >= -50 & max(data[, c(col)]) <= 50) {
      cols_50 <- append(cols_50, col)
    } else {
      cols_big <- append(cols_big, col)
    }
  }

  long_data <- melt(data[, cols_0_1_25])
  long_data$variable <- gsub("_", "£", long_data$variable)
  p <- ggplot(long_data, aes(x = variable, y = value)) +
    geom_violin() +
    coord_flip() +
    theme_bw() +
    ggtitle("Box plots: absolute values less than 0.125") +
    xlab("")
  export_latex("plot", "violin", "0_1_25", p, height = 9.7, width = width, TEX = TEX)


  long_data <- melt(data[, cols_0_7_5])
  long_data$variable <- gsub("_", "£", long_data$variable)
  p <- ggplot(long_data, aes(x = variable, y = value)) +
    geom_violin() +
    coord_flip() +
    theme_bw() +
    ggtitle("Box plots: absolute values less than 0.75") +
    xlab("")
  export_latex("plot", "violin", "0_7_5", p, height = 4.5, width = width, TEX = TEX)

  long_data <- melt(data[, cols_5])
  long_data$variable <- gsub("_", "£", long_data$variable)
  p <- ggplot(long_data, aes(x = variable, y = value)) +
    geom_violin() +
    coord_flip() +
    theme_bw() +
    ggtitle("Box plots: absolute values less than 5") +
    xlab("")
  export_latex("plot", "violin", "5", p, height = 2.3, width = width, TEX = TEX)


  long_data <- melt(data[, cols_50])
  long_data$variable <- gsub("_", "£", long_data$variable)
  p <- ggplot(long_data, aes(x = variable, y = value)) +
    geom_violin() +
    coord_flip() +
    theme_bw() +
    ggtitle("Box plots: absolute values less than 50") +
    xlab("")
  export_latex("plot", "violin", "50", p, height = 1.5, width = width, TEX = TEX)


  long_data <- melt(data[, cols_big])
  long_data$variable <- gsub("_", "£", long_data$variable)
  p <- ggplot(long_data, aes(x = variable, y = value)) +
    geom_violin() +
    coord_flip() +
    theme_bw() +
    ggtitle("Box plots: absolute values more than 50") +
    xlab("")
  export_latex("plot", "violin", "big", p, height = 1.5, width = width, TEX = TEX)
}

plot_ragged <- function(
    msfe_df_2,
    msfe_df_1,
    msfe_df_0,
    msfe_2,
    msfe_1,
    msfe_0,
    msfe_df,
    model) {
  plot_min <- as.Date("2019-12-01", format = "%Y-%m-%d")
  plot_max <- as.Date("2021-09-01", format = "%Y-%m-%d")

  msfe_df_2 <-
    subset(
      msfe_df_2,
      subset = (msfe_df_2$Date >= plot_min & msfe_df_2$Date <= plot_max)
    )
  msfe_df_1 <-
    subset(
      msfe_df_1,
      subset = (msfe_df_1$Date >= plot_min & msfe_df_1$Date <= plot_max)
    )
  msfe_df_0 <-
    subset(
      msfe_df_0,
      subset = (msfe_df_0$Date >= plot_min & msfe_df_0$Date <= plot_max)
    )

  ggplot() +
    geom_line(
      data = msfe_df_2,
      aes(
        x = as.Date(Date) %m+% months(2),
        y = model,
        color = "-2 Predictions"
      ),
      size = 1
    ) +
    geom_line(
      data = msfe_df_1,
      aes(
        x = as.Date(Date) %m+% months(1),
        y = model,
        color = "-1 Predictions"
      ),
      size = 1
    ) +
    geom_line(
      data = msfe_df_0,
      aes(
        x = as.Date(Date),
        y = model,
        color = "-0 Predictions"
      ),
      size = 1
    ) +
    geom_line(
      data = msfe_df_0,
      aes(
        x = as.Date(Date),
        y = GDP,
        color = "Observations"
      ),
      size = 1
    ) +
    scale_x_date(date_breaks = "1 months", date_labels = "%m-%Y") +

    # Rotate x axis labels by 45 degrees
    theme(axis.text.x = element_text(angle = 45), axis.title.x = element_blank()) +
    annotate(
      geom = "text",
      x = as.Date(max(msfe_df_0$Date)) %m+% months(-2),
      y = min(msfe_df_0$GDP) + 6,
      label = glue("-2 Pred. MSFE: {round(msfe_2, digits = 3)}")
    ) +
    annotate(
      geom = "text",
      x = as.Date(max(msfe_df_0$Date)) %m+% months(-2),
      y = min(msfe_df_0$GDP) + 4,
      label = glue("-1 Pred. MSFE: {round(msfe_1, digits = 3)}")
    ) +
    annotate(
      geom = "text",
      x = as.Date(max(msfe_df_0$Date)) %m+% months(-2),
      y = min(msfe_df_0$GDP) + 2,
      label = glue("-0 Pred. MSFE: {round(msfe_0, digits = 3)}")
    )
}

make_msfe_plot <- function(msfe_comparison_df) {
  long_data <- reshape2::melt(msfe_comparison_df[, -c(1)])
  group_by_ragged <- rep(msfe_comparison_df$Distance, 5)
  plot_df <- data.frame(long_data, "Distance" = group_by_ragged)
  return(
    ggplot() +
      geom_bar(
        data = plot_df,
        aes(x = variable, y = value, fill = factor(Distance)),
        stat = "identity",
        position = position_dodge2()
      ) +
      labs(fill = "Month from official quarterly release") +
      xlab("") +
      ylab("MSFE") +
      theme(legend.position = "bottom")
  )
}
