## code to prepare `PRESENT_VALUES` dataset goes here
PRESENT_VALUES <- ffoptim::get_player_present_values(clean = T)
usethis::use_data(PRESENT_VALUES, overwrite = TRUE)
devtools::document()
devtools::build()
