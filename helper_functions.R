get_seeds <- function() {
  seeds <- vector(mode = "list", length = 88)
  for (i in 1:527) {
    seeds[[i]] <- sample.int(1000, 15)
  }
  return(seeds)
}

get_intervals <- function() {
  INTERVALS <- c(
    "2010" = c(
      dataset_end_date = "2010-12-01",
      train_end_date = "2005-12-01",
      test_start_date = "2006-01-01",
      initialWindow = 200
    ),
    "2019" = c(
      dataset_end_date = "2019-12-01",
      train_end_date = "2015-12-01",
      test_start_date = "2016-01-01",
      initialWindow = 310
    ),
    "2022" = c(
      dataset_end_date = FALSE,
      train_end_date = "2015-12-01",
      test_start_date = "2016-01-01",
      initialWindow = 310
    )
  )
  return(INTERVALS)
}

export_latex <- function(type, name, year, object, width = 7, height = 6) {
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
        caption = paste0(name, year, "_rename_me"),
        label = paste0("tab:", name, year, "_rename_me")
      ),
      include.rownames = FALSE,
      file = paste0("./output/", name, "_table_", year, ".tex")
    )
  }
}
