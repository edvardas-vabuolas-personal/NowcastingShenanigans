get_seeds <- function() {
    seeds <- vector(mode = "list", length = 88)
    for (i in 1:527) {
        seeds[[i]] <- sample.int(1000, 15)
    }
    return(seeds)
}
