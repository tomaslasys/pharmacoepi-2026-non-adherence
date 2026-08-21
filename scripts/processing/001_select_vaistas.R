
suppressPackageStartupMessages({
  library(arrow)
  library(dplyr)
  library(stringr)
  library(purrr)
})

# ---- config ----------------------------------------------------------------
erec_in  <- "data/raw/receptai/erec_parquet"
evai_in  <- "data/raw/receptai/evai_parquet"
erec_out <- "data/processed/vaistas/erec"
evai_out <- "data/processed/vaistas/evai"
years    <- 2017:2026

# Columns to retain. any_of() => silently skips any not present in the source.
erec_keep <- c(
  "dirbt_recepto_id",                       # join key
  "recepto_metai", "recepto_ketv",          # prescription cohort
  # "ligos_kodas",                            # ICD-10 diagnosis
  "atc_kodas", 
  # "atc_pav",                   # drug class
  # "vaisto_bendr_pav",
  # "vaisto_prek_pav",
  # "vaisto_stiprumas",   # generic name + strength
  "gydymui_testi_periodas_d",                 # "continue until" period (days)
  "dozuociu_sk",                            # prescribed quantity (doses)
  # "vart_daznumas",                          # frequency text -> doses_per_day
  # "vien_doze",                              # single dose
  "vart_trukme_d",                          # prescribed duration (days)
  "recepto_galiojimas_d",                   # validity (days)
  "recepto_statusas",                       # completed / stopped
  "kompens_poz",                            # reimbursed flag
  "pac_lytis", "pac_amziaus_gr",            # patient sex / age group
  "pac_savivaldybe", "pac_sav_kodas",       # patient municipality
  "org_savivaldybe", "org_sav_kodas",        # prescriber municipality
  "gyd_kvalifikacijos_galioja"                 # prescriber specialty (valid)
)

evai_keep <- c(
  "dirbt_recepto_id",                       # join key
  "dirbt_isdavimo_id",                      # dispensing event id
  "isdavimo_metai", 
  "isdavimo_ketv",        # dispensing quarter (may differ from cohort)
  "isduotas_vaisto_kiekis",                 # dispensed quantity
  "kompensavimo_kodas", "kompensavimo_procentas",
  "vaisto_bendrinis_pavadinimas",
  # "npakid", 
  "npakid7"
)

# ---- helpers ---------------------------------------------------------------

# Extract a "YYYY_Q" cohort key from a file path.
# Matches 2022q1, 2022_1, 2022-1, 2022_1_ketv, etc. Adjust the regex if your
# filenames use a different convention.
parse_cohort <- function(path) {
  m <- str_match(basename(path), "(20\\d{2})[_\\-qQ]+([1-4])")
  if (is.na(m[1, 1])) return(NA_character_)
  paste(m[1, 2], m[1, 3], sep = "_")
}

# "2 k./1 d." -> 2 ;  "1 k./2 d." -> 0.5 ;  unparseable -> NA
parse_doses_per_day <- function(x) {
  m     <- str_match(x, "(\\d+)\\s*k\\.\\s*/?\\s*(\\d+)?\\s*d")
  times <- suppressWarnings(as.numeric(m[, 2]))
  days  <- suppressWarnings(as.numeric(m[, 3]))
  days[is.na(days)] <- 1
  times / days
}

# ---- discover & pair files by cohort ---------------------------------------
dir.create(erec_out, recursive = TRUE, showWarnings = FALSE)
dir.create(evai_out, recursive = TRUE, showWarnings = FALSE)

erec_files <- list.files(erec_in, pattern = "\\.parquet$", full.names = TRUE, recursive = TRUE)
evai_files <- list.files(evai_in, pattern = "\\.parquet$", full.names = TRUE, recursive = TRUE)

stopifnot(length(erec_files) > 0, length(evai_files) > 0)

erec_by_cohort <- split(erec_files, map_chr(erec_files, parse_cohort))
evai_by_cohort <- split(evai_files, map_chr(evai_files, parse_cohort))

cohorts <- names(erec_by_cohort)
cohorts <- cohorts[!is.na(cohorts) &
                     as.integer(substr(cohorts, 1, 4)) %in% years]
cohorts <- sort(cohorts)

message(sprintf("Found %d erec cohort(s) to process (%d evai cohort(s) available).",
                length(cohorts), length(evai_by_cohort)))

# ---- main loop: one prescription-quarter cohort at a time ------------------
ch <- cohorts[17]

for (ch in cohorts) {
  
  ef <- erec_by_cohort[[ch]]
  vf <- evai_by_cohort[[ch]]
  
  # --- erec: filter to Vaistas, slim columns, derive doses_per_day ---
  erec_ds   <- open_dataset(ef)
  n_erec_raw <- nrow(erec_ds)
  
  erec_slim <- erec_ds |>
    filter(vaisto_tipas == "Vaistas") |>
    select(any_of(erec_keep)) |>
    mutate(specialty1 = gsub("Medicinos gydytojas, ", "", gyd_kvalifikacijos_galioja),
           specialty1 = gsub(", Medicinos gydytojas", "", specialty1),
           specialty1 = ifelse(specialty1 == "Neatveriama", NA_character_, specialty1)) %>%
    mutate(specialty =
             case_when(
               is.na(specialty1) ~
                 "Unknown",
               grepl(",", specialty1) ~
                 "Physician with mutiple specialties",
               grepl("Šeimos gydytojas", specialty1, ignore.case = TRUE) ~
                 "General practitioner",
               grepl("Psichiatras", specialty1, ignore.case = TRUE) ~
                 "Psychiatrist",
               grepl("Vidaus ligų gydytojas", specialty1, ignore.case = TRUE) ~
                 "Internal medicine physician",
               grepl("gydytojas", specialty1, ignore.case = TRUE) ~
                 "Other physician",
               TRUE ~ "Non-physician prescriber"
             )) %>%
    select(-gyd_kvalifikacijos_galioja, -specialty1) |>
    collect() |>
    mutate(
      prescription_quarter = lubridate::make_date(
        recepto_metai, (recepto_ketv - 1L) * 3L + 1L, 1L),
      tx_continuation = ifelse(
        is.na(gydymui_testi_periodas_d), "No", "Yes")
    ) %>% 
    select(-recepto_metai, 
           -recepto_ketv)
  
  # summary(erec_slim$gydymui_testi_periodas_d)
  
  write_parquet(erec_slim, file.path(erec_out, paste0("erec_", ch, ".parquet")))
  
  # --- evai: semi_join on the kept prescription keys ---
  if (is.null(vf)) {
    message(sprintf("[%s] erec_raw=%d  erec_vaistas=%d  | WARNING: no matching evai cohort",
                    ch, n_erec_raw, nrow(erec_slim)))
    log_rows[[ch]] <- tibble(cohort = ch, erec_raw = n_erec_raw,
                             erec_vaistas = nrow(erec_slim),
                             evai_raw = NA_integer_, evai_kept = NA_integer_)
    next
  }
  
  keys      <- erec_slim |> distinct(dirbt_recepto_id)
  evai_ds   <- open_dataset(vf)
  n_evai_raw <- nrow(evai_ds)
  
  evai_slim <- evai_ds |>
    select(any_of(evai_keep)) |>
    semi_join(keys, by = "dirbt_recepto_id") |>   # keep only dispensings of kept prescriptions
    collect()|>
    mutate(
      prescription_quarter = lubridate::make_date(
        isdavimo_metai, (isdavimo_ketv - 1L) * 3L + 1L, 1L)
    ) %>% 
    select(-isdavimo_metai, 
           -isdavimo_ketv)
  
  write_parquet(evai_slim, file.path(evai_out, paste0("evai_", ch, ".parquet")))
  
  erec_pct <- if (n_erec_raw > 0) 100 * nrow(erec_slim) / n_erec_raw else NA_real_
  evai_pct <- if (n_evai_raw > 0) 100 * nrow(evai_slim) / n_evai_raw else NA_real_
  
  fmt    <- function(x) formatC(x, format = "d", big.mark = ",")
  indent <- strrep(" ", nchar(ch) + 3)
  
  line1 <- sprintf("erec_raw =%10s  %-13s%10s (%5.1f%%)",
                   fmt(n_erec_raw), "  |  erec_kept =", fmt(nrow(erec_slim)), erec_pct)
  line2 <- sprintf("evai_raw =%10s  %-13s%10s (%5.1f%%)",
                   fmt(n_evai_raw), "  |  evai_kept =",    fmt(nrow(evai_slim)), evai_pct)
  
  message(sprintf("[%s] %s\n%s%s", ch, line1, indent, line2))
  
  
}

rm(list = ls())
