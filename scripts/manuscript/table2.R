
source("./scripts/helpers/helpers.R")
library(purrr)

CI_Z <- qnorm(0.995)   # 99% CI

patterns <- readRDS("./data/processed/aggregated/aggregated_pna_patterns.rds") %>%
  filter(year %in% YEARS)

u <- function(x) replace_na(as.character(x), "Unknown")
atc_lv <- {
  a <- u(patterns$atc_class)
  c(sort(setdiff(unique(a), "Unknown")), intersect("Unknown", a))   # Unknown last
}

mk <- patterns %>%
  mutate(
    year       = factor(year),
    quarter     = factor(u(quarter), levels = c("Q1", "Q2", "Q3", "Q4")),
    sex        = factor(u(pt_sex), levels = c("Male", "Female", "Unknown")),
    age        = factor(u(pt_age_group), levels = c("0-17", "18-44", "45-64", "65+", "Unknown")),
    reimbursed = factor(case_when(kompens_poz == 1 ~ "Yes", kompens_poz == 0 ~ "No",
                                  TRUE ~ "Unknown"), levels = c("Yes", "No", "Unknown")),
    duration   = factor(u(rx_duration),
                        levels = c("Short-term", 
                                   "Medium-term", 
                                   "Long-term", 
                                   "Unknown")),
    # tx_continuation = factor(u(tx_continuation),
    #                          levels = c("Yes", 
    #                                     "No")),
    degurba    = factor(u(degurba_class),
                        levels = c("City", 
                                   "Town or suburb", 
                                   "Rural area", 
                                   "Unknown")),
    specialty = factor(u(specialty),
                       levels = c("General practitioner", 
                                  "Internal medicine physician",
                                  "Psychiatrist", 
                                  "Other physician", 
                                  "Physician with mutiple specialties",
                                  "Non-physician prescriber", 
                                  "Unknown")),
    prescriber = factor(u(prescriber_municipality),
                        levels = c("Patient's municipality", "Other municipality", "Unknown")),
    atc_class  = factor(u(atc_class), levels = atc_lv),
    fail       = n - pna) %>%
  droplevels()   # drop Unknown levels that have no observations

preds <- c("year", 
           "quarter",
           "sex", 
           "age", 
           "reimbursed", 
           "duration", 
           # "tx_continuation",
           "degurba", 
           "specialty", 
           "prescriber", 
           "atc_class")

var_labels <- c(
  year = "By calendar year", 
  quarter = "By quarter of year",
  sex = "By sex", 
  age = "By age group",
  reimbursed = "By reimbursement", 
  duration = "By treatment duration",
  tx_continuation = "Prescription is marked as for treatment continuation",
  degurba = "By degree of urbanisation", 
  specialty = "By prescriber specialty",
  prescriber = "By prescriber location",
  atc_class = "By therapeutic class (ATC level 1)"
)

or_tbl <- function(m) {
  s <- summary(m)$coefficients
  tibble(term = rownames(s), est = s[, "Estimate"], se = s[, "Std. Error"]) %>%
    filter(term != "(Intercept)") %>%
    mutate(ci = sprintf("%.2f (%.2f-%.2f)",
                        exp(est), exp(est - CI_Z * se), exp(est + CI_Z * se)))
}

crude <- map_dfr(preds, function(p) {
  mk %>% group_by(across(all_of(p))) %>%
    summarise(pna = sum(pna), fail = sum(fail), .groups = "drop") %>%
    glm(reformulate(p, response = "cbind(pna, fail)"), data = ., family = binomial) %>%
    or_tbl() %>% transmute(term, crude = ci)
})

adj <- glm(reformulate(preds, response = "cbind(pna, fail)"),
           data = mk, family = binomial) %>%
  or_tbl() %>% transmute(term, adjusted = ci)

indent     <- "   "
blank_or   <- tibble(`Crude OR (99% CI)` = "", `Adjusted OR (99% CI)` = "")

make_block <- function(p) {
  lv <- levels(mk[[p]])
  d <- tibble(level = lv, term = paste0(p, lv), is_ref = lv == lv[1]) %>%
    left_join(crude, by = "term") %>% left_join(adj, by = "term") %>%
    mutate(crude    = ifelse(is_ref, "1.00 (reference)", crude),
           adjusted = ifelse(is_ref, "1.00 (reference)", adjusted))
  bind_rows(
    bind_cols(tibble(Characteristic = var_labels[[p]]), blank_or),
    tibble(Characteristic         = paste0(indent, d$level),
           `Crude OR (99% CI)`    = d$crude,
           `Adjusted OR (99% CI)` = d$adjusted)
  )
}

table2 <- bind_rows(c(lapply(preds, make_block)))

save_table(table2, "table2")


# --- Municipality (crude only; Vilnius city as reference) ----
muni_marg <- load_agg() %>% recode_levels() %>%
  filter(variable == "municipality_name_en") %>%
  group_by(level, rx_status) %>% summarise(n = sum(n), .groups = "drop") %>%
  tidyr::pivot_wider(names_from = rx_status, values_from = n, values_fill = 0) %>%
  mutate(pna  = `not completed`,
         fail = `completed` + `partially completed`)

# Define levels manually: Vilnius city first, then others sorted, Unknown last
all_muni_levels <- sort(setdiff(unique(muni_marg$level), c("Unknown", "Vilnius city municipality")))
muni_lv <- c("Vilnius city municipality", all_muni_levels, intersect("Unknown", muni_marg$level))


# Apply the new order to the factor
muni_marg <- muni_marg %>% 
  mutate(municipality = factor(level, levels = muni_lv))

# Fit the model (now "Vilnius city" is the default reference)
m_crude <- glm(cbind(pna, fail) ~ municipality, data = muni_marg, family = binomial) %>%
  or_tbl() %>% transmute(term, crude = ci)

# Create the result block
muni_block <- tibble(level = muni_lv, term = paste0("municipality", muni_lv),
                     is_ref = level == "Vilnius city municipality") %>%  # Explicitly mark Vilnius as ref
  left_join(m_crude, by = "term") %>%
  mutate(crude = ifelse(is_ref, "1.00 (reference)", crude))

table_s5 <- bind_rows(
  bind_cols(tibble(Characteristic = "By municipality"), blank_or),
  tibble(Characteristic         = paste0(indent, muni_block$level),
         `Crude OR (99% CI)`    = muni_block$crude,
         `Adjusted OR (99% CI)` = "\u2014")
) %>% 
  select(Municipality = Characteristic , `Crude OR (99% CI)`) %>% 
  filter(Municipality != "By municipality")

save_table(table_s5, "supp_table5")
