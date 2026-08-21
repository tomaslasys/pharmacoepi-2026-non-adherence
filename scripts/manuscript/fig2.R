
source("./scripts/helpers/helpers.R")

library(showtext) 

font_add_google("Montserrat", 
                "Montserrat")
showtext_auto()
showtext_opts(dpi = 300)

byq <- load_agg_quarter() %>%          
  filter(variable == "pt_age_group") %>%           
  group_by(prescription_quarter, rx_status) %>%
  summarise(n = sum(n), .groups = "drop") %>%
  pivot_wider(names_from = rx_status, values_from = n, values_fill = 0)

for (s in status_raw) if (!s %in% names(byq)) byq[[s]] <- 0L
byq <- byq %>%
  mutate(prescription_quarter = as.Date(prescription_quarter),
         Total = rowSums(across(all_of(status_raw)))) %>%
  arrange(prescription_quarter)
byq <- bind_cols(byq, wilson(byq$`not completed`, byq$Total))

yr_rng <- range(year(byq$prescription_quarter))

p <- ggplot(byq, aes(prescription_quarter, est)) +
  geom_ribbon(aes(ymin = lower, ymax = upper), fill = "#08306b", alpha = 0.15) +
  geom_line(colour = "#08306b", linewidth = 0.7) +
  geom_point(colour = "#08306b", size = 1.8) +
  scale_x_date(breaks = byq$prescription_quarter[c(TRUE, FALSE)],
               labels = function(d) paste0(year(d), " Q", quarter(d))) +
  scale_y_continuous(limits = c(0, NA)) +
  labs(x = NULL, y = "Primary non-adherence (%)") +
  theme_minimal(base_size = 12) +
  theme_minimal(base_size = 12, 
                base_family = "Montserrat") +
  theme(plot.title       = element_text(hjust = 0.5, 
                                        face = "bold", 
                                        size = 13),
        axis.title       = element_text(size = 13),
        axis.text        = element_text(size = 11),
        axis.text.x      = element_text(angle = 45, 
                                        hjust = 1),
        axis.text.y      = element_text(angle = 0, 
                                        hjust = 1),
        panel.grid.minor = element_blank())

save_fig(p, "fig2", width = 9, height = 4.5)
print(p)

