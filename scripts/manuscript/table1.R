
source("./scripts/helpers/helpers.R")
agg <- load_agg() %>% recode_levels()

block_vars <- c("pt_sex", 
                "pt_age_group", 
                "kompens_poz", 
                "rx_duration",
                # "tx_continuation",
                "degurba_class", 
                "specialty",
                "prescriber_municipality", 
                "atc_class",
                "municipality_name_en")

var_labels <- c(
  pt_sex = "By sex", pt_age_group = "By age group", kompens_poz = "By reimbursement",
  rx_duration = "By treatment duration", degurba_class = "By degree of urbanisation",
  # tx_continuation = "Prescription is marked as for treatment continuation",
  specialty = "By prescriber specialty",
  prescriber_municipality = "By prescriber location",
  atc_class = "By therapeutic class (ATC level 1)",
  municipality_name_en = "By municipality"
)
lev_order <- list(
  pt_sex                  = c("Male", "Female", "Unknown"),
  pt_age_group            = c("0-17", "18-44", "45-64", "65+", "Unknown"),
  kompens_poz             = c("Yes", "No", "Unknown"),
  rx_duration             = c("Short-term", "Medium-term", "Long-term", "Unknown"),
  # tx_continuation         = c("Yes", "No"),
  degurba_class           = c("City", "Town or suburb", "Rural area", "Unknown"),
  specialty = c(
    "General practitioner",
    "Internal medicine physician",
    "Psychiatrist",
    "Other physician",
    "Physician with mutiple specialties",
    "Non-physician prescriber",
    "Unknown"
  ),
  prescriber_municipality = c("Patient's municipality", "Other municipality", "Unknown")
)

ref_var <- intersect(block_vars, unique(agg$variable))[1]   # partitions all rx
indent  <- "   "

year_counts <- agg %>% filter(variable == ref_var) %>%
  group_by(year) %>% summarise(n = sum(n), .groups = "drop") %>% arrange(year)

N_total <- sum(year_counts$n)

# category header row + indented level rows, single label + single n (%) column
block <- function(v, label, ord = NULL) {
  w <- agg %>% filter(variable == v) %>%
    group_by(level) %>% summarise(n = sum(n), .groups = "drop")
  w <- if (is.null(ord)) arrange(w, level == "Unknown", level) else arrange(w, match(level, ord))
  bind_rows(
    tibble(Characteristic = label, `n (%)` = ""),
    tibble(Characteristic = paste0(indent, w$level), `n (%)` = fmt_np(w$n, N_total))
  )
}

all_row <- tibble(Characteristic = "All prescriptions", `n (%)` = fmt_np(N_total, N_total))
year_block <- bind_rows(
  tibble(Characteristic = "By calendar year", `n (%)` = ""),
  tibble(Characteristic = paste0(indent, year_counts$year),
         `n (%)` = fmt_np(year_counts$n, N_total))
)

table1 <- bind_rows(c(
  list(all_row, year_block),
  lapply(block_vars, function(v) block(v, var_labels[[v]], ord = lev_order[[v]]))
))

# Split block_vars into main and supplementary
main_vars <- c("pt_sex", 
               "pt_age_group", 
               "kompens_poz", 
               "rx_duration",
               "degurba_class", 
               "specialty",
               "prescriber_municipality")

supp_vars <- c("atc_class", 
               "municipality_name_en")

# table1: all blocks
table1a <- bind_rows(c(
  list(all_row, year_block),
  lapply(main_vars, function(v) block(v, var_labels[[v]], ord = lev_order[[v]]))
))

# table_s1: all row + year + only ATC class and municipality
table_s1 <- bind_rows(c(
  list(all_row),
  lapply(supp_vars, function(v) block(v, var_labels[[v]], ord = lev_order[[v]]))
))

save_table(table1a, "table1")
save_table(table_s1, "supp_table1")
