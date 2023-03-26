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

save_plot <- function(name, plot) {
  tikz(paste0('./output/',name),width=7,height=6)
  plot
  # ggsave(name, plot = plot, path = "./output", width = 7, height = 7, dpi = 1200)
  dev.off()
}
