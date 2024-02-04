## code to prepare `FFDATA` dataset goes here
FFDATA <- FFData()
usethis::use_data(FFDATA, overwrite = TRUE)
devtools::document()
devtools::build()
