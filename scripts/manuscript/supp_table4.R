
source("./scripts/helpers/helpers.R")
library(data.table)

# municipality -> DEGURBA crosswalk
xwalk <- fread("data/dictionaries/dict_municipalities.csv") %>%
  transmute(municipality_name_en =
              str_replace_all(name_en, c("City" = "city", "Municipality" = "municipality",
                                         "District" = "district")),
            degurba_class = str_to_sentence(degurba_class))

w <- load_agg() %>%
  filter(variable == "municipality_name_en") %>%
  mutate(level = ifelse(is.na(level), "Unknown", level)) %>%
  group_by(level, rx_status) %>% summarise(n = sum(n), .groups = "drop") %>%
  pivot_wider(names_from = rx_status, values_from = n, values_fill = 0)

for (s in status_raw) if (!s %in% names(w)) w[[s]] <- 0L
w <- w %>%
  mutate(Total = rowSums(across(all_of(status_raw)))) %>%
  left_join(xwalk, by = c("level" = "municipality_name_en")) %>%
  arrange(level == "Unknown", level)

table_s3 <- w %>% transmute(
  Municipality          = level,
  `Degree of urbanisation` = ifelse(is.na(degurba_class), "Unknown", degurba_class),
  Completed             = fmt_np(completed,             Total),
  `Partially completed` = fmt_np(`partially completed`, Total),
  `Not completed (PNA)` = fmt_np(`not completed`,       Total),
  `Total, N`            = formatC(Total, big.mark = ",", format = "d"))

save_table(table_s3, "supp_table4")

