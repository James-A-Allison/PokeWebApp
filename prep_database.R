library(tidyverse)
library(duckdb)
library(duckplyr)
library(uuid)

con <- dbConnect(duckdb::duckdb(), "pokemon.db")

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

dust_status <- user_pokemon %>%
  select(dust_status = `Dust Status`) %>%
  distinct() %>%
  mutate(dust_id = case_when(
    dust_status == "Normal" ~ 1,
    dust_status == "Shadow" ~ 3,
    dust_status == "Lucky" ~ 2,
    dust_status == "Eternatus" ~ 5,
    dust_status == "Purified" ~ 4
              ))

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
