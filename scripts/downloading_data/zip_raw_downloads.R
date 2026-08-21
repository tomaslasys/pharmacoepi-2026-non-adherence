library(data.table)
library(httr)

erec <- "data/raw/receptai/erec_zip"
evai <- "data/raw/receptai/evai_zip"

dir.create(erec,
           recursive = TRUE, 
           showWarnings = FALSE)

dir.create(evai, 
           recursive = TRUE, 
           showWarnings = FALSE)

downloaded <- c()
skipped    <- list()

url_patterns <- list(
  list(
    pattern = "https://www.registrucentras.lt/aduomenys/?byla={year}_{quarter}_ketv_receptai.zip",
    folder  = erec
  ),
  list(
    pattern = "https://www.registrucentras.lt/aduomenys/?byla=vaistu_isdavimai_pagal_{year}_{quarter}_ketv_receptus.zip",
    folder  = evai
  )
)

for (item in url_patterns) {
  for (year in 2017:2017) {
    for (quarter in 1:2) {
      url      <- gsub("\\{year\\}",    year,    item$pattern)
      url      <- gsub("\\{quarter\\}", quarter, url)
      filename <- basename(gsub(".*byla=", "", url))
      filepath <- file.path(item$folder, filename)
      
      if (file.exists(filepath)) {
        cat("⟳ Already exists, skipping:", filename, "\n")
        downloaded <- c(downloaded, filename)
        next
      }
      
      tryCatch({
        response <- GET(url, timeout(200))
        
        if (status_code(response) == 200 && length(content(response, "raw")) > 0) {
          writeBin(content(response, "raw"), filepath)
          cat("✓ Downloaded:", filename, "→", item$folder, "\n")
          downloaded <- c(downloaded, filename)
        } else {
          cat("✗ Skipped (status", status_code(response), "):", filename, "\n")
          skipped <- append(skipped, list(list(file = filename, reason = paste("status", status_code(response)))))
        }
      }, error = function(e) {
        cat("✗ Skipped (error):", filename, "-", conditionMessage(e), "\n")
        skipped <<- append(skipped, list(list(file = filename, reason = conditionMessage(e))))
      })
    }
  }
}

cat("\n", strrep("=", 50), "\n")
cat("REPORT:", length(downloaded), "downloaded,", length(skipped), "skipped\n")
cat(strrep("=", 50), "\n")
cat("\nDownloaded:\n")
for (f in downloaded) cat("  ✓", f, "\n")
cat("\nSkipped:\n")
for (item in skipped) cat("  ✗", item$file, "(", item$reason, ")\n")





# ── remove bad files  ─────────────────────────────────────────────

check_zips <- function(folder) {
  zips <- list.files(folder, pattern = "\\.zip$", full.names = TRUE)
  bad  <- c()
  
  for (z in zips) {
    size <- file.size(z)
    ok   <- tryCatch({ unzip(z, list = TRUE); TRUE }, error = function(e) FALSE)
    
    if (!ok || size < 1024) {
      cat("✗ BAD:", basename(z), sprintf("(%.1f KB)\n", size / 1024))
      bad <- c(bad, z)
    } else {
      cat("✓", basename(z), sprintf("(%.1f KB)\n", size / 1024))
    }
  }
  
  cat("\n", length(bad), "bad files found\n")
  invisible(bad)
}

bad_erec <- check_zips(erec)
bad_evai <- check_zips(evai)


to_remove <- c(bad_erec, bad_evai)

if (length(to_remove) > 0) {
  file.remove(to_remove)
} else {
  message("No corrupted files found — nothing to remove.")
}
