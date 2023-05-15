
# Packages --------------------------------------------------------
library(here)
library(janitor)
library(tidyverse)




# read and preproc ------------------------------------------------
db_raw <- read_csv(here("data-raw/penguins.csv"))
penguins <- db_raw |>
  clean_names() |>
  select(species, sex, body_mass_g, culmen_depth_mm)



# store data analyzed ---------------------------------------------
write_rds(penguins, here("data/penguins.rds"))




# Analysis --------------------------------------------------------
gg <- penguins |>
  ggplot(aes(culmen_depth_mm, body_mass_g)) +
  geom_smooth(method = lm, colour = "black", linetype = "dashed") +
  geom_smooth(aes(colour = species), method = lm) +
  geom_point(aes(shape = sex, colour = species)) +
  theme(legend.position = "top")
ggsave(
  "gg.png", gg, path = here("output"),
  width = 16, height = 9, units = "cm", dpi = "retina", scale = 2
)


mod_depth <- lm(body_mass_g ~ culmen_depth_mm, data = penguins)
write_rds(mod_depth, here("output/mod_depth.rds"))

mod_depth_spec <- lm(
  body_mass_g ~ species + culmen_depth_mm,
  data = penguins
)
write_rds(mod_depth_spec, here("output/mod_depth_spec.rds"))
