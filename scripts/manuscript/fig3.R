
source("./scripts/helpers/helpers.R")

library(sf)
library(data.table)
library(giscoR)
library(showtext) 

font_add_google("Montserrat", "Montserrat")
showtext_auto()
showtext_opts(dpi = 300)  


SHP_PATH <- "data/dictionaries/lt_municipalities.gpkg"   # <- TODO: your shapefile/gpkg
SHP_KEY  <- "lau_code"                            # <- TODO: code column in shapefile
MAP_YEARS <- c(2018, 2020, 2022, 2024)

# municipality english name -> lau_code (to join names back to geometry)
xwalk <- fread("data/dictionaries/dict_municipalities.csv") %>%
  transmute(lau_code,
            municipality_name_en =
              str_replace_all(name_en, c("City" = "city", "Municipality" = "municipality",
                                         "District" = "district")))

# PNA rate per municipality per selected year
pna <- load_agg() %>%
  filter(variable == "municipality_name_en", year %in% MAP_YEARS) %>%
  group_by(year, level, rx_status) %>% summarise(n = sum(n), .groups = "drop") %>%
  pivot_wider(names_from = rx_status, values_from = n, values_fill = 0)
for (s in status_raw) if (!s %in% names(pna)) pna[[s]] <- 0L
pna <- pna %>%
  mutate(Total = rowSums(across(all_of(status_raw))),
         pna_pct = 100 * `not completed` / Total) %>%
  rename(municipality_name_en = level) %>%
  left_join(xwalk, by = "municipality_name_en")

# geometry x years


shp <- gisco_get_lau(year = 2021, country = "LT")   # downloads once, then cached
SHP_KEY <- "LAU_ID" 

shp <- shp %>% mutate(LAU_ID = as.integer(LAU_ID))


map_df <- tidyr::crossing(!!sym(SHP_KEY) := shp[[SHP_KEY]], year = MAP_YEARS) %>%
  left_join(shp, by = SHP_KEY) %>%
  left_join(pna, by = setNames(c("lau_code", "year"), c(SHP_KEY, "year"))) %>%
  st_as_sf()

p <- ggplot(map_df) +
  geom_sf(aes(fill = pna_pct), 
          colour = "white", 
          linewidth = 0.1) +
  scale_fill_gradient(low = "#e1eef6", 
                      high = "#08306b", 
                      name = "PNA\n(%)",
                      na.value = "grey90", 
                      n.breaks = 8,
                      guide = guide_colourbar(barwidth  = unit(4, "mm"),
                                              barheight = unit(0.4 * 6, "in"))) +
  facet_wrap(~ year, nrow = 2) +
  # labs(title = "Primary non-adherence by municipality\n") +
  theme_void(base_size = 12, 
             base_family = "Montserrat") +
  theme(legend.position = "right",
        strip.text   = element_text(size = 13),
        legend.title = element_text(size = 11),
        legend.text  = element_text(size = 9))

save_fig(p, "fig3", width = 12, height = 4)
print(p)
