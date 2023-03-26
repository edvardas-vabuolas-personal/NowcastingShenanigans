source("packages_manager.R")
source("load_data.R")

get_figure <- function(dataframe) {
    dataset <- load_data()
    dataset$GDP_QNA_RG <- na.locf(dataset$GDP_QNA_RG, fromLast = TRUE)
    dataframe %>% mutate(Date = as.Date(Date, format = "%d.%m.%Y"))
    dates_for_plot <-
        seq(as.Date("2006-01-01"), as.Date("2010-12-01"), by = "month")
    colors <- c(
        "Predictions" = "steelblue",
        "Observations" = "grey"
    )

    # Set graphs legend to the top
    theme_set(theme_bw() +
        theme(legend.position = "top"))

    plot_en_2010 <- ggplot() +
        # Draw predictions line
        geom_line(
            data = dataframe,
            aes(
                x = as.Date(dates_for_plot),
                y = as.numeric(dataframe$en_pred_2010),
                color = "Predictions"
            ),
            size = 1
        ) +

        # Draw observations line
        geom_line(
            data = dataframe,
            aes(
                x = as.Date(dates_for_plot),
                y = as.numeric(dataset$GDP_QNA_RG),
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

        # Rotate x axis label by 45 degrees
        theme(axis.text.x = element_text(angle = 45)) +

        # Add MSFE to the graph
        annotate(
            geom = "text",
            x = as.Date("2010-01-01"),
            y = -0.2,
            label = paste0("MSFE: ", "test")
        )
    figure <- ggarrange(
        plot_en_2010,
        labels = c("Elastic Net"),
        ncol = 1,
        nrow = 1
    )
    return(figure)
}
