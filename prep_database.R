library(tidyverse)
library(duckdb)
library(duckplyr)
library(uuid)

con <- dbConnect(duckdb::duckdb(), "data/pokemon.db")

user_pokemon <- readRDS("data/user_pokemon.RDS")

pokemon <- readRDS("data/pokemon_ids.rds")
moves <- readRDS("data/move_ids.rds")
pokemon_moves <- readRDS("data/pokemon_moves.rds")

base_data <- readRDS("data/base_stats.RDS")

users <- tibble(
  user_id = uuid::UUIDgenerate(),
  username = "Jellys0n",
  password_hash = NULL,
  created_at = Sys.time(),
  plan = "admin"
)

dbWriteTable(
  con,
  "users",
  users,
  overwrite = TRUE
)

dust_status <- tibble(
  dust_status = c("Normal", "Lucky", "Shadow", "Purified","Eternatus"),
  dust_id = 1:5
)

dbWriteTable(
  con,
  "dust_status",
  dust_status,
  overwrite = TRUE
)

user_pokemon <- user_pokemon %>%
  mutate(user_id = users$user_id[1],
  created_at = Sys.time(),
  nickname = NA_character_) %>%
  left_join(pokemon %>% select(Pokemon = name, pokemon_id)) %>%
  left_join(moves %>% select(fast_move_id = move_id, `Fast Move` = name)) %>%
  left_join(moves %>% select(charge_move_1_id = move_id, `Charge1` = name)) %>%
  left_join(moves %>% select(charge_move_2_id = move_id, `Charge2` = name)) %>%
  select(user_id,
        pokemon_instance_id = uuid,
        pokemon_id,
        nickname,
        dust_status = `Dust Status`,
        can_mega_evolve = `Can Mega Evolve`,
        can_dynamax = `Can Dynamax`,
      fast_move_id, charge_move_1_id, charge_move_2_id,
      level = Level,
      attack_iv = `Attack IV`,
      defence_iv = `Defence IV`,
      hp_iv = `HP IV`) %>%
  left_join(dust_status) %>%
  select(-dust_status) %>%
  mutate(can_mega_evolve = if_else(can_mega_evolve == "Yes", TRUE, FALSE),
  can_dynamax = if_else(can_dynamax == "Yes", TRUE, FALSE))

dbWriteTable(
  con,
  "user_pokemon",
  user_pokemon,
  overwrite = TRUE
)

dbWriteTable(
  con,
  "pokemon",
  pokemon,
  overwrite = TRUE
)
dbWriteTable(
  con,
  "moves",
  moves,
  overwrite = TRUE
)

dbWriteTable(
  con,
  "pokemon_moves",
  pokemon_moves,
  overwrite = TRUE
)

user_row <- users <- tibble(
  user_id = uuid::UUIDgenerate(),
  username = "Jellyroo000m",
  # password_hash = NA_character_,
  created_at = Sys.time(),
  plan = "admin"
)

dbWriteTable(
  con,
  "users",
  user_row,
  append = TRUE
)

dbReadTable(con, "users")

base_stats <- readRDS("data/base_stats.RDS")

dbWriteTable(
  con,
  "base_stats",
  base_stats,
  overwrite = TRUE
)

boss_move_combinations <- readRDS("data/boss_move_combinations.RDS")

dbWriteTable(
  con,
  "boss_move_combinations",
  boss_move_combinations,
  overwrite = TRUE
)

calendar <- readRDS("data/calendar.RDS")

dbWriteTable(
  con,
  "calendar",
  calendar,
  overwrite = TRUE
)

levels <- readRDS("data/levels.RDS")

dbWriteTable(
  con,
  "levels",
  levels,
  overwrite = TRUE
)

mega_table <- readRDS("data/mega_table.RDS")

dbWriteTable(
  con,
  "mega_table",
  mega_table,
  overwrite = TRUE
)

moves_formatted <- readRDS("data/moves_formatted.RDS")

dbWriteTable(
  con,
  "moves_formatted",
  moves_formatted,
  overwrite = TRUE
)

type_effectiveness <- readRDS("data/type_effectiveness.RDS") 

dbWriteTable(
  con,
  "type_effectiveness",
  type_effectiveness,
  overwrite = TRUE
)

user_move_combinations <- readRDS("data/user_move_combinations.RDS") 

dbWriteTable(
  con,
  "user_move_combinations",
  user_move_combinations,
  overwrite = TRUE
)

weather <- readRDS("data/weather.RDS")

dbWriteTable(
  con,
  "weather",
  weather,
  overwrite = TRUE
)

dbDisconnect(con, shutdown = FALSE)

