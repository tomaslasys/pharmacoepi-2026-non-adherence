
run_folder <- function(folder) {
  scripts <- list.files(folder, pattern = "\\.R$", full.names = TRUE)
  for (path in scripts) {
    message("\n#######################################################\n####    Running:\n####    ", path)
    source(path)
  }
}


run_folder("scripts/downloading_data")

run_folder("scripts/processing")

run_folder("scripts/manuscript")

source("scripts/helpers/combine_tables.R")
