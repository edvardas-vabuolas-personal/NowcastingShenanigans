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