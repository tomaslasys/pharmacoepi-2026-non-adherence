
library(readr)
library(dplyr)
library(stringr)
library(arrow)

erec_dir <- "./data/processed/vaistas/erec"
erec_files <- list.files(erec_dir, pattern = "^erec_.*\\.parquet$", full.names = TRUE)

evai_dir <- "./data/processed/vaistas/evai"
evai_files <- list.files(evai_dir, pattern = "^evai_.*\\.parquet$", full.names = TRUE)

output_dir <- "./data/processed/vaistas/merged"

if (!dir.exists(output_dir)) {
  dir.create(output_dir, recursive = TRUE)
}

quarters <- str_extract(basename(erec_files), "(?<=erec_)[0-9]{4}_[1-4]")

quarter <- quarters[18]

checks <- list()

for(quarter in quarters){
  
  erec_file <- erec_files[str_detect(erec_files, quarter)]
  evai_file <- evai_files[str_detect(evai_files, quarter)]
  
  if(length(erec_file) == 0 | length(evai_file) == 0){
    warning(paste("Missing files for quarter:", quarter))
    next
  }
  
  erec_data0 <- open_dataset(erec_file) %>% 
    select(dirbt_recepto_id, 
           # ligos_kodas,
           atc_kodas,
           dozuociu_sk,
           vart_trukme_d,
           tx_continuation,
           recepto_statusas,
           kompens_poz, 
           pac_lytis,
           pac_amziaus_gr,
           pac_sav_kodas,
           org_sav_kodas,
           specialty,
           prescription_quarter) %>% 
    mutate(rx_duration = case_when(
      vart_trukme_d <= 30 ~ "short-term",
      vart_trukme_d > 30 & vart_trukme_d <= 90 ~ "medium-term",
      vart_trukme_d > 90 ~ "long-term",
      TRUE ~ NA_character_)
    ) %>% 
    collect()
  
  evai_data0 <- open_dataset(evai_file) %>%
    mutate(
      is_censored = isduotas_vaisto_kiekis < 0,
      qty_known   = if_else(isduotas_vaisto_kiekis < 0, 1, isduotas_vaisto_kiekis)
    ) %>%
    group_by(dirbt_recepto_id, kompensavimo_kodas, kompensavimo_procentas) %>%
    summarise(
      n_dispensings       = n(),
      qty_censored          = sum(is_censored, na.rm = TRUE) > 0,
      qty_dispensed_total = sum(qty_known, na.rm = TRUE),   # excludes -1 now
      n_products          = n_distinct(npakid7)
    ) %>%
    mutate(
      kompensavimo_kodas     = if_else(is.na(kompensavimo_kodas), 0, kompensavimo_kodas),
      kompensavimo_procentas = if_else(is.na(kompensavimo_procentas), 0, kompensavimo_procentas)
    ) %>%
    ungroup() %>%
    collect()
  
  check_evai0 <- evai_data0 %>% 
    group_by(dirbt_recepto_id) %>% 
    filter(n() > 1) %>% 
    mutate(quarter = quarter)
  
  # test0 <- 
  #   evai_data0 %>% 
  #   filter(qty_dispensed_total == 0)
  
  erec_data1 <- erec_data0  %>%
    mutate(
      prescriber_municipality = case_when(
        pac_sav_kodas == org_sav_kodas              ~ "Patient's municipality",
        pac_sav_kodas != org_sav_kodas              ~ "Other municipality",
        TRUE ~ NA_character_
      ),
      pt_sex = factor(
        case_when(
          pac_lytis == "M" ~ "female",
          pac_lytis == "V" ~ "male",
          TRUE             ~ "unknown")),
      
      pt_age_group = factor(
        case_when(
          pac_amziaus_gr == "Iki 17 m." ~ "0-17",
          pac_amziaus_gr == "18-44 m."  ~ "18-44",
          pac_amziaus_gr == "45-64 m."  ~ "45-64",
          pac_amziaus_gr == "Nuo 65 m." ~ "65+",
          TRUE                          ~ "Unknown"),
        ordered = TRUE))
  
  merged_data <- left_join(erec_data1, 
                           evai_data0, 
                           by = "dirbt_recepto_id") %>% 
    mutate(
      
      rx_status = case_when(
        as.numeric(qty_dispensed_total) / as.numeric(dozuociu_sk) >= 0.9 ~ "completed",
        qty_dispensed_total > 0 ~ "partially completed",
        TRUE ~ "not completed"),
      
      status_match = case_when(
        recepto_statusas == "completed" |
          rx_status == "completed" ~ recepto_statusas == rx_status,
        TRUE ~ TRUE)
    ) %>% 
    select(-pac_lytis,
           -pac_amziaus_gr)
  
  output_file <- file.path(output_dir, paste0("merged_", quarter, ".parquet"))
  write_parquet(merged_data, output_file)
  
  print(paste0("quarter ", quarter, " - merged"))
  
  checks[[quarter]] <- check_evai0
  
  # test1 <- merged_data %>%
  #   group_by(pt_sex) %>%
  #   summarise(n = n())
  
}


checks_df <- bind_rows(checks)

rm(list = ls())
