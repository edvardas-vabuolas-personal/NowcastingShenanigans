library(readxl)
boe_data <- read_excel("uncertain-kingdom-nowcasting-gdp-and-its-revisions-dataset.xlsx", 
                       sheet = "staticVintage", skip = 1)
View(boe_data)