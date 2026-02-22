library(tidyverse)
library(doFuture)
library(future)
library(progressr)
library(furrr)
# devtools::install("C:/Users/james/pokemonGoSim")
library(pokemonGoSim)

options(progressr.enable = TRUE)
options(future.globals.maxSize= 891289600)

base_stats <- readRDS("data/base_stats.rds") %>%
  select(
    pokemon_id = name,
    name,
    form = name,
    base_atk = `Attack Go`,
    base_def = `Defence Go`,
    base_sta = `HP Go`,
    type1 = `Type 1`,
    type2 = `Type 2`,
    class = Class
  )

useful_base_attackers <- readRDS("data/useful_base_attackers.rds")

pokemon_moves <- readRDS("data/pokemon_moves.rds")
moves <- readRDS("data/moves.rds")

pokemon_ids <- readRDS("data/pokemon_ids.rds")
move_ids <- readRDS("data/move_ids.rds")

mega_table <- readRDS("data/mega_table.RDS")



## Bosses

raid_bosses <- readRDS("data/base_stats.rds") %>%
  # filter(`Raid Boss Tier` > 3) %>%
  filter(`Raid Boss Tier` > 3) %>%
  select(Pokemon = name, tier = `Raid Boss Tier`) %>%
  distinct() %>%
  left_join(raid_boss_tiers)

boss_move_combinations <- inner_join(
  pokemon_moves %>%
    left_join(move_ids %>% rename(`Move Name` = name)) %>%
    left_join(pokemon_ids %>% rename(`Pokemon` = name)) %>%
    select(Pokemon, `Move Name`, legacy) %>%
    filter(legacy == "No") %>%
    inner_join(moves %>% filter(`Move Type` == "Fast"), relationship = "many-to-many") %>%
    select(Pokemon, fast_move = `Move Name`),

  pokemon_moves %>%
    left_join(move_ids %>% rename(`Move Name` = name)) %>%
    left_join(pokemon_ids %>% rename(`Pokemon` = name)) %>%
    select(Pokemon, `Move Name`, legacy) %>%
    filter(legacy == "No") %>%
    inner_join(moves %>% filter(`Move Type` == "Charge"), relationship = "many-to-many") %>%
    select(Pokemon, charge_move = `Move Name`), relationship = "many-to-many" 
)

available_move_combinations <- inner_join(
  pokemon_moves %>%
    left_join(move_ids %>% rename(`Move Name` = name)) %>%
    left_join(pokemon_ids %>% rename(`Pokemon` = name)) %>%
    select(Pokemon, `Move Name`, legacy) %>%
    inner_join(moves %>% filter(`Move Type` == "Fast"), relationship = "many-to-many") %>%
    select(Pokemon, fast_move = `Move Name`),

  pokemon_moves %>%
    left_join(move_ids %>% rename(`Move Name` = name)) %>%
    left_join(pokemon_ids %>% rename(`Pokemon` = name)) %>%
    select(Pokemon, `Move Name`, legacy) %>%
    inner_join(moves %>% filter(`Move Type` == "Charge"), relationship = "many-to-many") %>%
    select(Pokemon, charge_move = `Move Name`), relationship = "many-to-many" 
)

moves <- moves %>%
      mutate(
        energy_delta = (`Energy Gain` * 1) + (`Energy Cost` * -1),
        duration_ms = Duration * 1000,
        category = case_when(
          `Move Type` == "Fast" ~ "fast_move",
          `Move Type` == "Charge" ~ "charge_move"
        )
      ) %>%
  select(
        move_id = `Move Name`,
        name = `Move Name`,
        category,
        power = Power,
        energy_delta,
        duration_ms,
        type = Category)

bosses <- boss_move_combinations %>%
  inner_join(raid_bosses) %>%
  rowwise() %>%
  mutate(boss = list(
    build_boss(
      pokemon_id = Pokemon,
      tier = tier,
      fast_move_id = fast_move,
      charged_move_id = charge_move
  ))) %>%
  ungroup

available_pokemon <- bind_rows(
  readRDS("data/base_stats.rds") %>%
    filter(Released == "Yes") %>%
    mutate(shadow = FALSE) %>%
    select(
      pokemon_id = name,
      name,
      form = name,
      base_atk = `Attack Go`,
      base_def = `Defence Go`,
      base_sta = `HP Go`,
      type1 = `Type 1`,
      type2 = `Type 2`,
      shadow
    ),

  readRDS("data/base_stats.rds") %>%
    filter(`Shadow Released` == "Yes") %>%
    mutate(shadow = TRUE) %>%
    select(
      pokemon_id = name,
      name,
      form = name,
      base_atk = `Attack Go`,
      base_def = `Defence Go`,
      base_sta = `HP Go`,
      type1 = `Type 1`,
      type2 = `Type 2`,
      shadow
    )
) %>%
  mutate(join = 1) %>%
  left_join(tibble(join = 1, level = c(20, 25, 30, 40, 50))) %>%
  left_join(tibble(
    join = 1,
    iv_atk = c(0, 15),
    iv_def = c(0, 15),
    iv_sta = c(0, 15)
  )) %>%
  select(-join)

long_attacker_list <- available_pokemon %>%
  inner_join(available_move_combinations %>% rename(pokemon_id = Pokemon)) %>%
  # filter(type1 %in% c("Electric", "Grass", "Water") |
  #       type2 %in% c("Electric", "Grass", "Water")) %>%
  rename(attacker_fast_move = fast_move,
         attacker_charge_move = charge_move) %>%
  # slice(1:1000) %>%
  rowwise() %>%
  mutate(
    attacker = list(
      build_attacker(
        pokemon_id      = pokemon_id,
        level           = level,
        fast_move_id    = attacker_fast_move,
        charged_move_id = attacker_charge_move,
        shadow = shadow,
        base_stats = base_stats
      )
    )
  ) %>%
  ungroup()



sim_grid <- tidyr::crossing(
  long_attacker_list,
  bosses) %>%
  mutate(weather = "Extreme")
