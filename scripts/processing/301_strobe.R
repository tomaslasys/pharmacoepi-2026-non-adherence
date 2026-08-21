
erec_raw  <- "data/raw/receptai/erec_parquet"
evai_raw  <- "data/raw/receptai/evai_parquet"
erec_kept <- "data/processed/vaistas/erec"
evai_kept <- "data/processed/vaistas/evai"

# read all files, count n_raw_erec, n_kept_erec, n_raw_evai, n_kept_evai, n_erec_in_evai 
log_rows <- list()

for (f in list.files(erec_kept, pattern = "^erec_.*\\.parquet$", full.names = TRUE)) {
  
  ch <- str_extract(basename(f), "(?<=erec_)20\\d{2}_[1-4]")   # -> "2018_1"
  
  ef_raw  <- file.path(erec_raw,  paste0("erec_", ch, ".parquet"))
  vf_raw  <- file.path(evai_raw,  paste0("evai_", ch, ".parquet"))
  ef_kept <- file.path(erec_kept, paste0("erec_", ch, ".parquet"))
  vf_kept <- file.path(evai_kept, paste0("evai_", ch, ".parquet"))
  
  n_raw_erec  <- nrow(open_dataset(ef_raw))
  n_kept_erec <- nrow(open_dataset(ef_kept))
  n_raw_evai  <- nrow(open_dataset(vf_raw))
  n_kept_evai <- nrow(open_dataset(vf_kept))
  
  # unique kept prescriptions that have a dispensing = distinct ids in kept evai
  n_erec_in_evai <- open_dataset(vf_kept) %>%
    distinct(dirbt_recepto_id) %>%
    collect() %>%
    nrow()
  
  log_rows[[ch]] <- tibble(
    cohort = ch,
    n_raw_erec = n_raw_erec, 
    n_kept_erec = n_kept_erec,
    n_raw_evai = n_raw_evai, 
    n_kept_evai = n_kept_evai,
    n_erec_in_evai = n_erec_in_evai)
}

agg_strobe <- bind_rows(log_rows) %>%
  mutate(year = str_sub(cohort, 1, 4)) %>%
  group_by(year) %>%
  summarise(across(where(is.numeric), sum), .groups = "drop") %>%
  arrange(year)

output_dir <- "data/processed/aggregated"

saveRDS(agg_strobe, file.path(output_dir, "strobe.RDS"))
