library(data.table)
library(arrow)
library(tidyverse)

process_zips <- function(zip_dir, out_dir, prefix) {
  dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)

  zips <- list.files(zip_dir, pattern = "\\.zip$", full.names = TRUE)
  cat(prefix, "— Zips found:", length(zips), "\n\n")

  z <- zips[1]

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
        showProgress  = FALSE,
        select        = c("recepto_metai", "recepto_ketv", "vaisto_tipas")
      )

      dt1 <- dt %>%
        group_by(recepto_metai, recepto_ketv, vaisto_tipas) %>%
        summarise(n = n())

      write_parquet(dt1, out_file)
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
  out_dir = "data/raw/receptai/check",
  prefix  = "erec"
)
cat("✓ All done\n")



parquet_dir <- "data/raw/receptai/check"
pq_files <- list.files(parquet_dir, pattern = "\\.parquet$", full.names = TRUE)

cat("Found", length(pq_files), "parquet files\n\n")

# Read and bind all files
dt_all <- rbindlist(lapply(pq_files, function(f) {
  cat("Reading:", basename(f), "\n")
  read_parquet(f)
}))

# View summary
cat("\n--- Summary ---\n")
print(dim(dt_all))
head(dt_all)


parquet_dir <- "raw_data/receptai/check"
pq_files <- list.files(parquet_dir, pattern = "\\.parquet$", full.names = TRUE)
cat("Found", length(pq_files), "parquet files\n\n")

dt_all <- rbindlist(lapply(pq_files, function(f) {
  cat("Reading:", basename(f), "\n")
  read_parquet(f)
}))


dt_all1 <- dt_all %>%
  group_by(recepto_metai, vaisto_tipas) %>%
  filter(recepto_metai < 2025,
         recepto_metai > 2017) %>%
  summarise(n = sum(n), .groups = "drop")

dt_all2 <- dt_all %>%
  group_by(vaisto_tipas) %>%
  filter(recepto_metai < 2025,
         recepto_metai > 2017) %>%
  summarise(n = sum(n), .groups = "drop") %>%
  ungroup() %>%
  mutate(perc = round(n / sum(n) * 100, 2))


table_1 <- dt_all2 %>%
  mutate(category_en = case_when(
    vaisto_tipas %in% c("Neatveriama", "Kitos priemonės") ~ "Other",
    vaisto_tipas == "Vaistas"                              ~ "Medicine",
    vaisto_tipas == "Vardinis vaistas"                      ~ "Named-patient medicine",
    vaisto_tipas == "Ekstemporalus vaistas"                 ~ "Extemporaneous medicine",
    vaisto_tipas == "Medicininos pagalbos priemonės"        ~ "Medical devices",
    TRUE ~ vaisto_tipas
  )) %>%
  group_by(category_en) %>%
  summarise(n = sum(n), .groups = "drop") %>%
  mutate(pct = round(100 * n / sum(n), 2)) %>%
  arrange(desc(n))

table_1

