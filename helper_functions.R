source("packages_manager.R")

get_seeds <- function() {
  seeds <- vector(mode = "list", length = 88)
  for (i in 1:527) {
    seeds[[i]] <- sample.int(1000, 15)
  }
  return(seeds)
}

get_intervals <- function() {
  INTERVALS <- hashmap()
  params_2010 = c(
    dataset_end_date = "2010-12-01",
    train_end_date = "2005-12-01",
    test_start_date = "2006-01-01",
    initialWindow = 200
  )
  params_2019 = c(
    dataset_end_date = "2019-12-01",
    train_end_date = "2015-12-01",
    test_start_date = "2016-01-01",
    initialWindow = 310
  )
  params_2022 = c(
    dataset_end_date = FALSE,
    train_end_date = "2015-12-01",
    test_start_date = "2016-01-01",
    initialWindow = 310
  )
  INTERVALS[[2010]] <- params_2010
  INTERVALS[[2019]] <- params_2019
  INTERVALS[[2022]] <- params_2022
  INTERVALS[["2010.break_points_quarterly"]] <- c(73)
  INTERVALS[["2019.break_points_quarterly"]] <- c(73)
  INTERVALS[["2022.break_points_quarterly"]] <- c(73, 122)
  
  return(INTERVALS)
}

export_latex <- function(type, name, year = "", object, width = 7, height = 6, TEX=FALSE) {
  # type should be "plot" or "table"
  # name should be a string, normally a prefix (i.e. ar, var, ml)
  # object should be a ggplot object for plots or a dataframe for tables
  # year = "" For cases when plot/table (the object) is not year-specific
  if (TEX == TRUE) {
    if (type == "plot") {
      tikz(
        paste0("./output/", name, "_plot_", year, ".tex"),
        width = width,
        height = height
      )
      plot(object)
      dev.off()
    } else if (type == "table") {
      print(
        xtable(
          object,
          caption = paste0(name, year, "renameme"),
          label = paste0("tab:", name, year, "renameme")
        ),
        include.rownames = FALSE,
        file = paste0("./output/", name, "_table_", year, ".tex")
      )
    }
  }
}

calculate_msfe <- function(predictions,  oos) {
  squared_diff <- (as.numeric(predictions) - oos)^2
  sum_squared_diff <- sum(squared_diff)
  msfe <- mean(sum_squared_diff)
  return(msfe)
}
