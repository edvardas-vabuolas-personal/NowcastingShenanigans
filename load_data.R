source("packages_manager.R")

load_data <- function(
    drop_irrelevant = TRUE,
    dataset_end_date = FALSE,
    interpolate = FALSE) {
    nowcasting_dataset <- read_excel(
        "230315 Nowcasting Dataset.xlsx",
        sheet = "Nowcasting Dataset",
        col_types = c(
            "date",
            "numeric",
            "numeric",
            "numeric",
            "numeric",
            "numeric",
            "numeric",
            "numeric",
            "numeric",
            "numeric",
            "numeric",
            "numeric",
            "numeric",
            "numeric",
            "numeric",
            "numeric",
            "numeric",
            "numeric",
            "numeric",
            "numeric",
            "numeric",
            "numeric",
            "numeric",
            "numeric",
            "numeric",
            "numeric",
            "numeric",
            "numeric",
            "numeric",
            "numeric",
            "numeric",
            "numeric",
            "numeric",
            "numeric",
            "numeric",
            "numeric",
            "numeric",
            "numeric",
            "numeric",
            "numeric",
            "numeric",
            "numeric",
            "numeric",
            "numeric",
            "numeric",
            "numeric",
            "numeric",
            "numeric",
            "numeric",
            "numeric",
            "numeric",
            "numeric",
            "numeric"
        )
    )

    if (drop_irrelevant == TRUE) {
        nowcasting_dataset <- nowcasting_dataset[, -c(2, 3, 5)]
    }

    if (dataset_end_date != FALSE) {
        nowcasting_dataset <-
            subset(
                nowcasting_dataset,
                subset = nowcasting_dataset$Date <= dataset_end_date
            )
    }

    if (interpolate == TRUE) {
        nowcasting_dataset$GDP_QNA_RG <-
            na.approx(nowcasting_dataset$GDP_QNA_RG)
    }

    # Identify the column with missing values
    # colSums(is.na(nowcasting_dataset))

    # Calculate the mean of the column
    column_mean <- mean(nowcasting_dataset$LIBOR_3mth, na.rm = TRUE)

    # Replace the missing values with the mean
    nowcasting_dataset$LIBOR_3mth <- ifelse(
        is.na(nowcasting_dataset$LIBOR_3mth),
        column_mean,
        nowcasting_dataset$LIBOR_3mth
    )

    # Underscore in GDP_QNA_RG causes latex errors
    names(nowcasting_dataset)[2] <- "GDP"

    return(nowcasting_dataset)
}
