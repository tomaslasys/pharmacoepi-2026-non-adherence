
source("./scripts/helpers/helpers.R")

w <- load_agg() %>%
  filter(variable == "atc_group") %>%
  mutate(level = ifelse(is.na(level), "Unknown", level)) %>%
  group_by(level, rx_status) %>% summarise(n = sum(n), .groups = "drop") %>%
  pivot_wider(names_from = rx_status, values_from = n, values_fill = 0)

for (s in status_raw) if (!s %in% names(w)) w[[s]] <- 0L
w <- w %>%
  mutate(Total = rowSums(across(all_of(status_raw))),
         level = case_when(level == "Unknown" ~ "Unknown",
                           Total < 1000       ~ "Other",
                           TRUE               ~ level)) %>%
  group_by(level) %>%
  summarise(across(all_of(c(status_raw, "Total")), sum), .groups = "drop") %>%
  mutate(rank = case_when(level == "Other" ~ 2, level == "Unknown" ~ 3, TRUE ~ 1)) %>%
  arrange(rank, level) %>% select(-rank)

table_s2 <- w %>% transmute(
  `ATC group`           = level,
  Completed             = fmt_np(completed,             Total),
  `Partially completed` = fmt_np(`partially completed`, Total),
  `Not completed (PNA)` = fmt_np(`not completed`,       Total),
  `Total, N`            = formatC(Total, big.mark = ",", format = "d"))

save_table(table_s2, "supp_table3")
