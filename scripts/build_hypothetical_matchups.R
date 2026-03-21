library(tidyverse)
library(doFuture)
library(future)
library(progressr)
library(furrr)
# devtools::install("C:/Users/james/pokemonGoSim")
library(pokemonGoSim)
library(DBI)

options(progressr.enable = TRUE)
options(future.globals.maxSize= 891289600)

# friendship = c("best", "good", "great", "none", "ultra")
# party_power = c(0,2,3,4)

friendship = c("best")
party_power = c(2)

dm_bonus_tibble <- crossing(friendship, party_power) %>%
  mutate(party_power = if_else(friendship == "none",0, party_power)) %>%
  distinct()


files <- list.files("R", pattern = "\\.R$", full.names = TRUE)

lapply(files, source)

base_stats <- get_base_stats() %>%
  select(
    pokemon_id = name,
    name,
    form = name,
    base_atk = `Attack Go`,
    base_def = `Defence Go`,
    base_sta = `HP Go`,
    type1 = `Type 1`,
    type2 = `Type 2`
  )

upcoming_bosses <- get_calendar() %>%
  filter(To >= Sys.Date(),
    !grepl("Shadow", `Raid Boss`))

raid_bosses <- get_base_stats() %>%
  # filter(`Raid Boss Tier` > 3) %>%
  filter(`Raid Boss Tier` > 4) %>%
  select(Pokemon = name, tier = `Raid Boss Tier`) %>%
  distinct() %>%
  left_join(raid_boss_tiers) # %>%
  # inner_join(upcoming_bosses %>% select(Pokemon = `Raid Boss`))

boss_move_combinations <- get_boss_move_combinations()

moves <- get_moves_formatted()

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

user_move_combinations <- get_user_move_combinations()

user_pokemon_to_run <- get_base_stats() %>%
  filter(`Lv 40 CP` > 3500) %>%
  filter(`Type 1` == "Ground" | `Type 2` == "Ground") %>%
#   filter(Class %in% c("Starter", "Mega",  "Legendary", "Pseudo-legendary",
#  "Mega Legendary", "Mythical", "Ultra Beast", "Paradox Pokemon")) %>%
  select(Pokemon = name) %>%
  distinct()

# useful_base_attackers <- readRDS("data/useful_base_attackers.RDS")

hypo_pokemon <- user_move_combinations %>%
  rename(fast_move_id = fast_move,
        charged_move_id = charge_move) %>%
  inner_join(user_pokemon_to_run) %>%
  # mutate(shadow = if_else(dust_status == "Shadow", TRUE, FALSE)) %>% ADD SHADOW
  mutate(iv_atk = 0,
     iv_def  = 0, 
     iv_sta  = 0,
     shadow = FALSE,
    ) %>%
  rename(pokemon_id = Pokemon) %>%
  crossing(tibble(level = c(30,40,50))) %>%
  rowwise() %>%
    mutate(
    attacker = list(
      build_attacker(
        pokemon_id      = pokemon_id,
        level           = level,
        fast_move_id    = fast_move_id,
        charged_move_id = charged_move_id,
        shadow = shadow,
        base_stats = base_stats
      )
    )
  ) %>%
  ungroup() %>%
  mutate(weather = "Extreme")

sim_grid <- tidyr::crossing(
  hypo_pokemon,
  bosses,dm_bonus_tibble)

con <- dbConnect(duckdb::duckdb(), "data/pokemon.db")
existing_sims <-dbReadTable(con, "hypothetical_matchups") %>%
  rename(fast_move = boss_fast_move_id,
        charge_move =boss_charged_move_id,
        pokemon_id = pokemon_name,
        Pokemon =  raid_boss)
dbDisconnect(con, shutdown = FALSE)

sim_grid <- anti_join(sim_grid, existing_sims) %>%
  slice(1:400000)

rm(existing_sims)

registerDoFuture()
plan(multisession, workers = 10)  # or however many

print(Sys.time())

with_progress({

  p <- progressor(along = seq_len(nrow(sim_grid)))

  sim_list <- future_map(
    seq_len(nrow(sim_grid)),
    function(i) {

      p()

      user_result <- sim_grid %>%
        slice(i) %>%
        mutate(sim = map2(
          attacker,
          boss,
          ~ simulate_battle_timeline(
              attacker   = .x,
              boss       = .y,
              weather    = weather,
              friendship = friendship,
              party_size = party_power 
            ))) %>%
        mutate(
          dps    = map_dbl(sim, "dps"),
          damage = map_dbl(sim, "damage_done"),
          time   = map_dbl(sim, "time"),
          weather = "Extreme") %>%
        select(
              friendship,
              pokemon_name = pokemon_id,
              raid_boss = Pokemon,
              level,
              fast_move_id,
              charged_move_id,
              boss_fast_move_id = fast_move,
              boss_charged_move_id = charge_move,
              dps,
              damage,
              time,
              weather,
              raid_tier = tier,shadow, party_power)
      # add_user_battle_results(user_result) 
    }
    , .options = furrr_options(packages = "pokemonGoSim")) %>%
    list_rbind()

})

con <- dbConnect(duckdb::duckdb(), "data/pokemon.db")

dbWriteTable(con, "hypothetical_matchups", sim_list, append = TRUE)
dbDisconnect(con, shutdown = FALSE)
