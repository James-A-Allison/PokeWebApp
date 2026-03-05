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

type_colours <- c(
  Normal   = "#A8A77A",
  Fire     = "#EE8130",
  Water    = "#6390F0",
  Electric = "#F7D02C",
  Grass    = "#7AC74C",
  Ice      = "#96D9D6",
  Fighting = "#C22E28",
  Poison   = "#A33EA1",
  Ground   = "#E2BF65",
  Flying   = "#A98FF3",
  Psychic  = "#F95587",
  Bug      = "#A6B91A",
  Rock     = "#B6A136",
  Ghost    = "#735797",
  Dragon   = "#6F35FC",
  Dark     = "#705746",
  Steel    = "#B7B7CE",
  Fairy    = "#D685AD"
)

user_pokemon %>%
  left_join(base_stats %>% select(Pokemon = name, type_1 = `Type 1`, type_2 = `Type 2`)) %>%
  # mutate(Type = if_else(type_1 == type_2, type_1, paste(type_1, type_2, sep = " & "))) %>%
  select(Pokemon, Level, type_1, type_2, uuid) %>%
  distinct() %>%
  ggplot(aes(x = Level, fill = type_1, color = type_2)) +
  geom_dotplot(stackgroups = TRUE, stackdir = "up") +
  scale_fill_manual(values = type_colours) +
  scale_color_manual(values = type_colours)





