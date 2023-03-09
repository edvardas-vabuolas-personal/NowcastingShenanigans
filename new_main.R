library(readxl)
boe_data <- read_excel("uncertain-kingdom-nowcasting-gdp-and-its-revisions-dataset.xlsx", 
                       sheet = "staticVintage", col_types = c("date", 
                                                              "numeric", "numeric", "numeric", "numeric", "numeric", 
                                                              "numeric", "numeric", "numeric", "numeric", "numeric", 
                                                              "numeric", "numeric", "numeric", "numeric", "numeric", 
                                                              "numeric", "numeric", "numeric", "numeric", "numeric", 
                                                              "numeric", "numeric", "numeric", "numeric", "numeric", 
                                                              "numeric", "numeric", "numeric", "numeric", "numeric", 
                                                              "numeric", "numeric", "numeric", "numeric", "numeric"))


boe_data_test <- diff(log(boe_data$LFSE))


View(boe_data)
