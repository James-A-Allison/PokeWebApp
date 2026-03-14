library(tidyverse)
library(shinydashboard)
library(shiny)
library(pokemonGoSim)

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

files <- list.files("R", pattern = "\\.R$", full.names = TRUE)

lapply(files, source)

moves_formatted <- get_moves_formatted()
levels <- get_levels()
base_stats <- get_base_stats()

user_table <- get_user_pokemon_enriched("0a08ee46-2663-4155-a535-22a93cd5a821") %>%
    select(
      pokemon_instance_id,
      Pokemon,
      `Dust Status` = dust_status,
      `Can Mega Evolve` = can_mega_evolve,
      `Can Dynamax` = can_dynamax,
      `Fast Move` = fast_move,
      Charge1,
      Charge2,
      Level, 
      `Attack IV` = attack_iv, 
      `Defence IV` = defence_iv, 
      `HP IV` = hp_iv)

user_table %>%
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
    group_by(Pokemon) %>% 
      summarise(n = n_distinct(pokemon_instance_id),
              level = mean(Level),
              CP = mean(CP)) %>%
  arrange(desc(n))

user_table %>%
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
  left_join(base_stats %>% select(Pokemon = name, `Type 1`, `Type 2`)) %>%
  select(pokemon_instance_id, CP, Level, `Type 1`, `Type 2`) %>%
  distinct() %>%
  pivot_longer(cols = c(`Type 1`, `Type 2`), values_to = "Type") %>%
  select(-name) %>%
  distinct() %>%
  mutate(Type = factor(Type, levels = rev(names(type_colours)))) %>%
  group_by(Type) %>%
  summarise(`Number of Pokemon` = n_distinct(pokemon_instance_id),
            `Average Level of Pokemon` = mean(Level),
            `Average CP of Pokemon` = mean(CP)) %>%
  pivot_longer(cols = -Type) %>%
  ggplot(aes(x = Type, y = value, fill = Type, color = Type)) +
  facet_wrap(facets = vars(name), scales = "free_x") +
  geom_col(stat = "identity") +
  coord_flip() +
  scale_x_discrete(drop = FALSE) +
  scale_fill_manual(values = type_colours) +
  scale_color_manual(values = type_colours) +
  theme(legend.position = "none") +
  labs(x = NULL, y = NULL)

