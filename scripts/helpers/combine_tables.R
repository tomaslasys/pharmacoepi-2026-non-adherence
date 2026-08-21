
library(writexl)

tab_dir <- "./output/tables"
out     <- file.path(tab_dir, "all_tables.xlsx")

files <- list.files(tab_dir, pattern = "\\.rds$", full.names = TRUE)
nm    <- tools::file_path_sans_ext(basename(files))

# order: main tables, then supplements, then anything else (e.g. checks)
rank  <- ifelse(grepl("^table", nm), 0, ifelse(grepl("^supp", nm), 1, 2))
ord   <- order(rank, nm)
files <- files[ord]; nm <- nm[ord]

# Excel sheet names: <= 31 chars and unique
sheet  <- make.unique(substr(nm, 1, 31), sep = "_")
tables <- setNames(lapply(files, readRDS), sheet)

write_xlsx(tables, out)
message("wrote ", length(tables), " sheets -> ", out)
print(sheet)
