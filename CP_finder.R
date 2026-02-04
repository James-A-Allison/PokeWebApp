library(tidyverse)
library(googlesheets4)

base_stats <- read_sheet("https://docs.google.com/spreadsheets/d/1cjTin49W2AkW9Z2ndJ59IDZ3o64FGtj2LhYrx-QoxHg/",
                          sheet = "Base Stats")

levels <- read_sheet("https://docs.google.com/spreadsheets/d/1cjTin49W2AkW9Z2ndJ59IDZ3o64FGtj2LhYrx-QoxHg/",
                    sheet = "Levels")



CP_Finder <- function(pokemon, CP, status, dust, HP, attack, defence, base_stats, levels) {
  base_stats <- base_stats %>%
    filter(name == pokemon) %>%
    select(name, Nerf, `HP Go`, `Attack Go`, `Defence Go`)


}

levels %>%
  select(`Level`, `CP Multiplier`) %>%
  mutate(join = 1) %>%
  left_join(tibble(join = 1, Attack_IV = seq(from = 0, to = 15, by = 1))) %>%
  left_join(tibble(join = 1, Def_IV = seq(from = 0, to = 15, by = 1))) %>%
  left_join(tibble(join = 1, HP_IV = seq(from = 0, to = 15, by = 1)))

