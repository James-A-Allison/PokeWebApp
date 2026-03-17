library(tidyverse)
library(DBI)

con <- dbConnect(duckdb::duckdb(), "data/pokemon.db")

pokemon_moves <- DBI::dbReadTable(con, "pokemon_moves")
moves <- DBI::dbReadTable(con, "move_stats")
# weather_boosts <- readRDS("data/weather.rds")

pokemon_ids <-  DBI::dbReadTable(con, "pokemon")
move_ids <- DBI::dbReadTable(con, "moves")

boss_move_combinations <- inner_join(
  pokemon_moves %>%
    left_join(move_ids %>% rename(movename = name)) %>%
    left_join(pokemon_ids %>% rename(`Pokemon` = name)) %>%
    select(Pokemon, movename, legacy) %>%
    filter(legacy == "No") %>%
    inner_join(moves %>% filter(movetype == "Fast"), relationship = "many-to-many") %>%
    select(Pokemon, fast_move = movename),

  pokemon_moves %>%
    left_join(move_ids %>% rename(movename = name)) %>%
    left_join(pokemon_ids %>% rename(`Pokemon` = name)) %>%
    select(Pokemon, movename, legacy) %>%
    filter(legacy == "No") %>%
    inner_join(moves %>% filter(movetype == "Charge"), relationship = "many-to-many") %>%
    select(Pokemon, charge_move = movename), relationship = "many-to-many" 
)

available_move_combinations <- inner_join(
  pokemon_moves %>%
    left_join(move_ids %>% rename(movename = name)) %>%
    left_join(pokemon_ids %>% rename(`Pokemon` = name)) %>%
    select(Pokemon, movename, legacy) %>%
    inner_join(moves %>% filter(movetype == "Fast"), relationship = "many-to-many") %>%
    select(Pokemon, fast_move = movename),

  pokemon_moves %>%
    left_join(move_ids %>% rename(movename = name)) %>%
    left_join(pokemon_ids %>% rename(`Pokemon` = name)) %>%
    select(Pokemon, movename, legacy) %>%
    inner_join(moves %>% filter(movetype == "Charge"), relationship = "many-to-many") %>%
    select(Pokemon, charge_move = movename), relationship = "many-to-many" 
)

moves_formatted <- moves %>%
    rename(type = category) %>%
      mutate(
        energy_delta = (energygain * 1) + (energycost * -1),
        duration_ms = duration * 1000,
        category = case_when(
          movetype == "Fast" ~ "fast_move",
          movetype == "Charge" ~ "charge_move"
        )
      ) %>%
  select(
        move_id = movename,
        name = movename,
        category,
        power,
        energy_delta,
        duration_ms,
        type)

# saveRDS(boss_move_combinations, "data/boss_move_combinations.RDS")
# saveRDS(available_move_combinations, "data/user_move_combinations.RDS")
# saveRDS(moves_formatted, "data/moves_formatted.RDS")

dbWriteTable(con, "user_move_combinations", available_move_combinations, overwrite = TRUE)
dbWriteTable(con, "boss_move_combinations", boss_move_combinations, overwrite = TRUE)
dbWriteTable(con, "moves_formatted", moves_formatted, overwrite = TRUE)
dbDisconnect(con, shutdown = FALSE)
