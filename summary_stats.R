library(tidyverse)
library(googlesheets4)
library(shiny)
library(pokemonGoSim)
library(DT)
library(uuid)


base_stats <- readRDS("data/base_stats.rds")
levels <- readRDS("data/levels.rds")
user_move_combinations <- readRDS("data/user_move_combinations.RDS")
user_pokemon <- readRDS("data/user_pokemon.RDS")

moves_formatted <- readRDS("data/moves_formatted.RDS")


attacker_type_summaries <- tibble()
poke_types <- base_stats %>% select(`Type 1`) %>% distinct() %>% pull()
input_table <- user_pokemon %>%
  inner_join(levels %>% select(Level, `CP Multiplier`)) %>%
  left_join(moves_formatted %>% 
    filter(category  == "fast_move") %>%
    select(`Fast Move` = name, fast_type = type)) %>%
  left_join(moves_formatted %>% 
    filter(category  == "charge_move") %>%
    select(Charge1 = name, charged_type1 = type)) %>%
  left_join(moves_formatted %>% 
    filter(category  == "charge_move") %>%
    select(Charge2 = name, charged_type2 = type)) %>%
  rowwise() %>%
  mutate(CP = CP_Formula(pokemon = Pokemon, 
        base_stats = base_stats,
        levels = levels,
        level = Level,
        CP_Multiplier = `CP Multiplier`,
        Attack_IV = `Attack IV`,
        Defence_IV = `Defence IV`,
        HP_IV = `HP IV`)) %>%
  ungroup()

for (i in seq(poke_types)) {
  poke_type <- poke_types[i]

  attacker_type_summaries <- input_table %>%
    filter(fast_type == poke_type |
          charged_type1 == poke_type |
          charged_type2 == poke_type ) %>%
    summarise(n = n_distinct(uuid),
            level = mean(Level),
            CP = mean(CP)) %>%
              mutate(attacker_type = poke_type) %>%
              bind_rows(attacker_type_summaries)
}

pokemon_summaries <- input_table %>%
  group_by(Pokemon) %>% 
    summarise(n = n_distinct(uuid),
            level = mean(Level),
            CP = mean(CP)) 
