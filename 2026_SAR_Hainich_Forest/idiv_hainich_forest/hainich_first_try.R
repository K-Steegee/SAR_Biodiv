library(dplyr)
test <- read.csv(r"(C:\Users\uwe\OneDrive\Desktop\KaiUniaktuell\Physik\Semester9\SAR\project\idiv_hainich_forest\hainich_inventory.csv)")

library(tidyverse)

test_sp %>%
  filter(census_year == 2023) %>%
  ggplot(aes(easting_m, northing_m, color = sp)) +
  geom_point(size = 1, alpha = 0.8) +
  coord_equal() +
  theme_minimal() +
  labs(title = "Tree distribution – 2023")

test_sp %>%
  filter(status == "A") %>%
  ggplot(aes(x = census_year, y = dbh, group = stem_id)) +
  geom_line(alpha = 0.08, color = "grey40") +
  geom_smooth(aes(group = 1), method = "loess",
              color = "red", linewidth = 1.2) +
  theme_minimal() +
  labs(
    title = "DBH development across inventories",
    x = "Census year",
    y = "DBH (cm)"
  )

test_sp %>%
  filter(census_year == 2023) %>%
  ggplot(aes(easting_m, northing_m, color = status)) +
  geom_point(size = 1.2) +
  coord_equal() +
  scale_color_manual(values = c(
    "A" = "darkgreen",
    "D" = "red",
    "M" = "grey",
    "P" = "blue"
  )) +
  theme_minimal() +
  labs(title = "Tree status – 2023")

dbh_change <- test %>%
  filter(census_year %in% c(2013, 2023),
         status == "A") %>%
  select(stem_id, census_year, dbh) %>%
  pivot_wider(
    names_from = census_year,
    values_from = dbh,
    names_prefix = "dbh_"
  ) %>%
  mutate(
    dbh_change = dbh_2023 - dbh_2013
  )

dbh_change_5000 <- dbh_change %>% filter(
  stem_id < 50000,
  dbh_change > -20 & dbh_change < 20
                                         )


plot(dbh_change_5000$stem_id, dbh_change_5000$dbh_change)


trees_2023 <- test %>%
  filter(census_year == 2023) %>%
  select(
    plot_id, stem_id, sp,
    easting_m, northing_m,
    dbh, status, vitality, coverage
  )

write_csv(trees_2023, r"(C:\Users\uwe\OneDrive\Desktop\KaiUniaktuell\Physik\Semester9\SAR\project\idiv_hainich_forest\results\hainich_trees_2023.csv)")

