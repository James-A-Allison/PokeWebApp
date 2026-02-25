library(tidyverse)

pokemon_moves <- readRDS("data/pokemon_moves.rds")
moves <- readRDS("data/moves.rds")
# weather_boosts <- readRDS("data/weather.rds")

pokemon_ids <- readRDS("data/pokemon_ids.rds")
move_ids <- readRDS("data/move_ids.rds")

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

moves_formatted <- moves %>%
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

saveRDS(boss_move_combinations, "data/boss_move_combinations.RDS")
saveRDS(available_move_combinations, "data/user_move_combinations.RDS")
saveRDS(moves_formatted, "data/moves_formatted.RDS")