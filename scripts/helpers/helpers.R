
suppressPackageStartupMessages({
  library(dplyr); library(tidyr); library(stringr); library(lubridate)
  library(ggplot2); library(writexl)
})

agg_year_path    <- "./data/processed/aggregated/aggregated_vaistas_yearly.RDS"
agg_quarter_path <- "./data/processed/aggregated/aggregated_vaistas_quarterly.RDS"  # <- confirm exact name
log_path    <- "./data/processed/aggregated/log_select_vaistas.RDS"
strobe_path <- "./data/processed/aggregated/strobe.RDS"
merged_dir <- "./data/processed/vaistas/merged"
tab_dir    <- "./output/tables"
fig_dir    <- "./output/figures"
for (d in c(tab_dir, fig_dir)) if (!dir.exists(d)) dir.create(d, recursive = TRUE)

# ---- study period ---------------------------------------------------
# FULL_DATA <- TRUE  -> include ALL available years (e.g. 2025 / full data)
# FULL_DATA <- FALSE -> study window 2018-2024
FULL_DATA  <- FALSE
YEARS      <- if (FULL_DATA) 2000:2100 else 2018:2024
SUFFIX     <- if (FULL_DATA) "_full"    else ""
PERIOD_LBL <- if (FULL_DATA) "all years" else "2018-2024"

status_raw <- c("completed", "partially completed", "not completed")

# aggregated counts; pass years = NULL to include all quarters/years
load_agg_year <- function(years = YEARS) {
  d <- readRDS(agg_year_path) %>% mutate(year = as.integer(year))
  if (!is.null(years)) d <- filter(d, year %in% years)
  d
}
load_agg_quarter <- function(years = YEARS) {
  d <- readRDS(agg_quarter_path) %>%
    mutate(year = year(prescription_quarter), quarter = quarter(prescription_quarter))
  if (!is.null(years)) d <- filter(d, year %in% years)
  d
}
# default loader = year (used by most tables/figures)
load_agg <- load_agg_year

fmt_np <- function(n, N) {
  pct <- 100 * n / N
  pct_str <- ifelse(pct > 0 & pct < 0.05, "<0.1", sprintf("%.1f", pct))
  sprintf("%s (%s)", formatC(n, big.mark = ",", format = "d"), pct_str)
}

save_table <- function(tbl, name) {
  name <- paste0(name, SUFFIX)
  saveRDS(tbl, file.path(tab_dir, paste0(name, ".rds")))
  # write_xlsx(tbl, file.path(tab_dir, paste0(name, ".xlsx")))
  message("table -> ", name, " (.rds/.xlsx)"); invisible(tbl)
}

save_fig <- function(p, name, width = 9, height = 5) {
  name <- paste0(name, SUFFIX)
  ggsave(file.path(fig_dir, paste0(name, ".jpeg")), p, width = width, height = height, dpi = 300)
  ggsave(file.path(fig_dir, paste0(name, ".svg")),  p, width = width, height = height)
  message("figure -> ", name, " (.jpeg/.svg)"); invisible(p)
}

# Wilson 95% CI for a proportion, returned as percentages
wilson <- function(x, n, conf = 0.95) {
  z <- qnorm(1 - (1 - conf) / 2); p <- x / n; den <- 1 + z^2 / n
  centre <- (p + z^2 / (2 * n)) / den
  half   <- z * sqrt(p * (1 - p) / n + z^2 / (4 * n^2)) / den
  tibble(est = 100 * p, lower = 100 * (centre - half), upper = 100 * (centre + half))
}

# readable levels shared across tables
recode_levels <- function(df) {
  df %>% mutate(level = case_when(
    is.na(level)                                       ~ "Unknown",
    variable == "kompens_poz"       & level == "1"     ~ "Yes",
    variable == "kompens_poz"       & level == "0"     ~ "No",
    variable == "prescriber_municipality" & level == "TRUE"  ~ "Patient's municipality",
    variable == "prescriber_municipality" & level == "FALSE" ~ "Other municipality",
    TRUE                                               ~ level))
}
