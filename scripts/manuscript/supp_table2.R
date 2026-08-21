
source("./scripts/helpers/helpers.R")
agg <- load_agg() %>% recode_levels()

block_vars <- c("pt_sex", 
                "pt_age_group", 
                "kompens_poz", 
                "rx_duration",
                "degurba_class", 
                "specialty",
                "prescriber_municipality", 
                "municipality_name_en",
                "atc_class")

var_labels <- c(
  pt_sex                  = "By sex",
  pt_age_group            = "By age group",
  kompens_poz             = "By reimbursement",
  rx_duration             = "By treatment duration",
  degurba_class           = "By degree of urbanisation",
  specialty               = "By prescriber specialty",
  prescriber_municipality = "By prescriber location",
  municipality_name_en    = "By patient's municipality",
  atc_class               = "By therapeutic class (ATC level 1)"
)
lev_order <- list(
  pt_sex                  = c("Male", "Female", "Unknown"),
  pt_age_group            = c("0-17", "18-44", "45-64", "65+", "Unknown"),
  kompens_poz             = c("Yes", "No", "Unknown"),
  rx_duration             = c("Short-term", "Medium-term", "Long-term"),
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

ref_var <- intersect(block_vars, unique(agg$variable))[1]

# long (level, rx_status, n) -> wide with row total, status columns guaranteed
to_wide <- function(df) {
  w <- df %>% pivot_wider(names_from = rx_status, values_from = n, values_fill = 0)
  for (s in status_raw) if (!s %in% names(w)) w[[s]] <- 0L
  w %>% mutate(Total = rowSums(across(all_of(status_raw))))
}

# format a wide frame (with `level` + status cols + Total) into row-% block
row_cols <- function(w) tibble(
  Characteristic        = paste0("\u2003", w$level),
  Completed             = fmt_np(w$completed,             w$Total),
  `Partially completed` = fmt_np(w$`partially completed`, w$Total),
  `Not completed (PNA)` = fmt_np(w$`not completed`,       w$Total),
  `Total, N`            = formatC(w$Total, big.mark = ",", format = "d")
)

make_block <- function(v) {
  w <- agg %>% filter(variable == v) %>%
    group_by(level, rx_status) %>% summarise(n = sum(n), .groups = "drop") %>%
    to_wide()
  ord <- lev_order[[v]]
  w <- if (is.null(ord)) arrange(w, level == "Unknown", level) else arrange(w, match(level, ord))
  row_cols(w)
}

header <- function(lab) tibble(Characteristic = lab, Completed = "",
                               `Partially completed` = "", `Not completed (PNA)` = "",
                               `Total, N` = "")

# calendar-year block from the year column
year_w <- agg %>% filter(variable == ref_var) %>%
  group_by(level = as.character(year), rx_status) %>% summarise(n = sum(n), .groups = "drop") %>%
  to_wide() %>% arrange(level)

# overall (All prescriptions) row
ov <- agg %>% filter(variable == ref_var) %>%
  group_by(level = "All", rx_status) %>% summarise(n = sum(n), .groups = "drop") %>%
  to_wide()
n_row <- row_cols(ov) %>% mutate(Characteristic = "All prescriptions")

table_s1 <- bind_rows(c(
  list(n_row),
  list(header("By calendar year"), row_cols(year_w)),
  unlist(lapply(block_vars, function(v) list(header(var_labels[[v]]), make_block(v))),
         recursive = FALSE)
))

save_table(table_s1, "supp_table2")
