source("packages_manager.R")
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
    theme(axis.text.x = element_text(angle = 45), axis.title.x=element_blank()) +
    
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
  var_imp_df <- var_imp_df[order(-var_imp_df$Overall), , drop=FALSE]
  
  # TikZDevice breaks if we use underscores. Replace £ with Find All in Overleaf
  var_imp_df$Variables <- gsub('_','£', rownames(var_imp_df))

  p <- head(var_imp_df, rows) %>%
    arrange(Overall) %>%    # First sort by val. This sort the dataframe but NOT the factor levels
    mutate(Variables=factor(Variables, levels=Variables)) %>%   # This trick update the factor levels
    ggplot( aes(x=Variables, y=Overall/100)) +
    geom_bar(stat="identity") +
    coord_flip() +
    theme_bw() +
    xlab("")+
    ylab("
    Absolute Coefficients. 
    Scaled to be between 0 and 1.
    ") +
    ggtitle(label)
  
  return(p)
}
