library(data.table)
library(arrow)

process_zips <- function(zip_dir, out_dir, prefix) {
  dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)
  
  zips <- list.files(zip_dir, pattern = "\\.zip$", full.names = TRUE)
  cat(prefix, "— Zips found:", length(zips), "\n\n")
  
  for (z in zips) {
    bn  <- basename(z)
    yr  <- sub(".*(\\d{4})_\\d_ketv.*", "\\1", bn)
    qtr <- sub(".*\\d{4}_(\\d)_ketv.*", "\\1", bn)
    
    out_file <- file.path(out_dir, paste0(prefix, "_", yr, "_", qtr, ".parquet"))
    
    if (file.exists(out_file)) {
      cat(sprintf("── %s: already exists, skipping\n", basename(out_file)))
      next
    }
    
    cat(sprintf("── %s ... ", bn))
    
    tryCatch({
      tmp    <- tempdir()
      inner  <- unzip(z, list = TRUE)$Name
      target <- inner[grepl("\\.(csv|txt)$", inner, ignore.case = TRUE)][1]
      unzip(z, files = target, exdir = tmp, overwrite = TRUE)
      
      dt <- fread(
        file.path(tmp, target),
        encoding   = "UTF-8",
        fill       = TRUE,
        na.strings = c("NULL", "", "NA"),
        showProgress  = FALSE
      )
      
      setnames(dt, trimws(sub("^\xef\xbb\xbf", "", names(dt))))
      dt[, source_file := basename(z)]
      
      write_parquet(dt, out_file)
      cat(sprintf("%s rows — ✓ %s\n", 
                  formatC(nrow(dt), 
                          format = "d", 
                          big.mark = ",",
                          width = 12), 
                  basename(out_file)))
      
      rm(dt)
      gc()
      
    }, error = function(e) {
      cat("✗ ERROR:", conditionMessage(e), "\n")
    })
  }
}

# ── run ───────────────────────────────────────────────────────────────────────
process_zips(
  zip_dir = "data/raw/receptai/erec_zip",
  out_dir = "data/raw/receptai/erec_parquet",
  prefix  = "erec"
)
process_zips(
  zip_dir = "data/raw/receptai/evai_zip",
  out_dir = "data/raw/receptai/evai_parquet",
  prefix  = "evai"
)
cat("✓ All done\n")
