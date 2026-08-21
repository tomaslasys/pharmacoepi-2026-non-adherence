
library(arrow)
library(dplyr)
library(tidyr)
library(stringr)
library(purrr)
library(lubridate)
library(data.table)

merged_dir <- "./data/processed/vaistas/merged"
output_dir <- "./data/processed/aggregated"
if (!dir.exists(output_dir)) dir.create(output_dir, recursive = TRUE)

# ---- dictionaries ---------------------------------------------------
dict_municipalities <- fread("data/dictionaries/dict_municipalities.csv") %>%
  select(sav_kodas = lau_code, municipality_name_en = name_en, degurba_class) %>%
  mutate(degurba_class = str_to_sentence(degurba_class),
         municipality_name_en = str_replace_all(
           municipality_name_en,
           c("City" = "city", "Municipality" = "municipality", "District" = "district")))

dict_atc <- fread("data/dictionaries/dict_atc.csv") %>%
  select(atc_code, atc_name) %>% mutate(atc_name = str_to_sentence(atc_name))

dict_atc_alterations <- fread("data/dictionaries/dict_atc_alterations.csv") %>%
  select(atc_kodas = old_atc_code, new_atc_code)

dict_atc1 <- dict_atc %>% select(atc_kodas = atc_code, new_atc_code = atc_code) %>%
  anti_join(dict_atc_alterations, by = "atc_kodas")

dict_atc_groups <- bind_rows(dict_atc_alterations, dict_atc1) %>%
  mutate(lvl1 = str_sub(new_atc_code, 1, 1),
         lvl3 = str_sub(new_atc_code, 1, 3)) %>%
  left_join(dict_atc, by = c("lvl1" = "atc_code")) %>% rename(atc_class = atc_name) %>%
  left_join(dict_atc, by = c("lvl3" = "atc_code")) %>% rename(atc_group = atc_name) %>%
  mutate(atc_class = paste0(lvl1, " - ", atc_class),
         atc_group = paste0(lvl3, " - ", atc_group)) %>%
  select(atc_kodas, atc_class, atc_group) %>% distinct()

# variables for the marginal tables (one block each)
cat_vars <- c("pt_sex", 
              "pt_age_group", 
              "kompens_poz", 
              "rx_duration",
              "tx_continuation",
              "municipality_name_en", 
              "degurba_class", 
              "prescriber_municipality",
              "specialty",
              "atc_class", 
              "atc_group")

# coarse predictors that define a regression covariate pattern
pattern_vars <- c("pt_sex", 
                  "pt_age_group", 
                  "kompens_poz", 
                  "rx_duration",
                  "tx_continuation",
                  "degurba_class", 
                  "prescriber_municipality",
                  "specialty",
                  "atc_class", 
                  "year",
                  "quarter")

merged_files <- list.files(merged_dir, pattern = "^merged_.*\\.parquet$",
                           full.names = TRUE)


f <- merged_files[1]

# ---- one quarter at a time -> marginal long counts + pattern counts -
res <- lapply(merged_files, function(f) {
  
  d <- open_dataset(f) %>%
    select(atc_kodas, 
           kompens_poz, 
           prescription_quarter, 
           rx_duration,
           tx_continuation,
           pac_sav_kodas, 
           prescriber_municipality,
           specialty,
           rx_status, 
           pt_sex, 
           pt_age_group) %>%
    mutate(atc_kodas = case_when(
      is.na(atc_kodas) ~ NA_character_,
      atc_kodas == "-" ~ NA_character_,
      TRUE             ~ atc_kodas)) %>%
    collect() %>%
    left_join(dict_atc_groups,     by = "atc_kodas") %>%
    left_join(dict_municipalities, by = c("pac_sav_kodas" = "sav_kodas")) %>%
    mutate(quarter = case_when(
      month(prescription_quarter) %in% c(12, 1, 2) ~ "Q1",
      month(prescription_quarter) %in% c(3, 4, 5)  ~ "Q2",
      month(prescription_quarter) %in% c(6, 7, 8)  ~ "Q3",
      month(prescription_quarter) %in% c(9, 10, 11) ~ "Q4")) %>%
    select(-pac_sav_kodas) %>%
    mutate(across(c(pt_sex, rx_duration), str_to_sentence),
           year = year(prescription_quarter))
  
  # marginal: keep both quarter and year so both aggregates derive from one pass
  marg <- map_dfr(cat_vars, function(v) {
    d %>%
      transmute(prescription_quarter, year, rx_status,
                variable = v, level = as.character(.data[[v]])) %>%
      count(variable, 
            prescription_quarter, 
            year, 
            rx_status, 
            level, 
            name = "n")
  })
  
  # joint covariate patterns with PNA event count
  pat <- d %>%
    mutate(pna = as.integer(rx_status == "not completed")) %>%
    group_by(across(all_of(pattern_vars))) %>%
    summarise(n = n(), pna = sum(pna), .groups = "drop")
  
  print(sprintf("%s: %s rows", basename(f), formatC(nrow(d), big.mark = ",")))
  
  list(marg = marg, pat = pat)
})

# ---- combine --------------------------------------------------------
marg_all <- bind_rows(map(res, "marg"))
pat_all  <- bind_rows(map(res, "pat"))

agg_quarter <- marg_all %>%
  group_by(variable, prescription_quarter, rx_status, level) %>%
  summarise(n = sum(n), .groups = "drop")

agg_year <- marg_all %>%
  group_by(variable, year, rx_status, level) %>%
  summarise(n = sum(n), .groups = "drop")

pna_patterns <- pat_all %>%
  group_by(across(all_of(pattern_vars))) %>%
  summarise(n = sum(n), pna = sum(pna), .groups = "drop")

# ---- save -----------------------------------------------------------
saveRDS(agg_year,     file.path(output_dir, "aggregated_vaistas_yearly.RDS"))
saveRDS(agg_quarter,  file.path(output_dir, "aggregated_vaistas_quarterly.RDS"))
saveRDS(pna_patterns, file.path(output_dir, "aggregated_pna_patterns.rds"))

message(sprintf("patterns: %s rows | prescriptions: %s | PNA: %s",
                formatC(nrow(pna_patterns), big.mark = ","),
                formatC(sum(pna_patterns$n),   big.mark = ","),
                formatC(sum(pna_patterns$pna), big.mark = ",")))
message("Done.")


rm(list = ls())
