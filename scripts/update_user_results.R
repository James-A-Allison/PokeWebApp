library(tidyverse)
library(doFuture)
library(future)
library(progressr)
library(furrr)
# devtools::install("C:/Users/james/pokemonGoSim")
library(pokemonGoSim)

options(progressr.enable = TRUE)
options(future.globals.maxSize= 891289600)

# PARTY_POWER_MULT <- 2
# BOSS_CHARGE_COOLDOWN <- 2.5

# raid_boss_tiers <- tibble::tibble(
#   tier = c(6, 5, 4),
#   hp = c(22500, 15000, 9000),
#   cpm = c(0.7903, 0.7903, 0.7903)
# )

user_id <- "0a08ee46-2663-4155-a535-22a93cd5a821"

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

# pokemon_moves <- readRDS("data/pokemon_moves.rds")
# moves <- readRDS("data/moves.rds")
# # weather_boosts <- readRDS("data/weather.rds")

# pokemon_ids <- readRDS("data/pokemon_ids.rds")
# move_ids <- readRDS("data/move_ids.rds")

# mega_table <- readRDS("data/mega_table.RDS")
# type_effectiveness <- readRDS("data/type_effectiveness.RDS") 
# level_multipliers <- readRDS("data/levels.rds") %>%
#   select(level = `Level`, cpm = `CP Multiplier`)


## Bosses

raid_bosses <- get_base_stats() %>%
  # filter(`Raid Boss Tier` > 3) %>%
  filter(`Raid Boss Tier` > 3) %>%
  select(Pokemon = name, tier = `Raid Boss Tier`) %>%
  distinct() %>%
  left_join(raid_boss_tiers)

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


## Users

# existing_sims <- readRDS("data/results_summary.RDS")

user_pokemon <- get_user_pokemon_to_sim(user_id) %>%
  mutate(shadow = if_else(dust_status == "Shadow", TRUE, FALSE)) %>%
  mutate(attack_iv = if_else(is.na(attack_iv), 0, attack_iv)) %>%
  mutate(defence_iv  = if_else(is.na(defence_iv ), 0, defence_iv )) %>%
  mutate(hp_iv  = if_else(is.na(hp_iv ), 0, hp_iv )) %>%
  select(pokemon_instance_id,
        pokemon_id = Pokemon,
        level = `Level`,
        iv_atk = attack_iv,
        iv_def = defence_iv ,
        iv_sta = hp_iv ,
        shadow,
        fast_move_id = fast_move,
        charged_move_id = `Charge1`,
        Charge2)

user_pokemon <- get_user_pokemon_enriched(user_id) %>%
  filter(can_mega_evolve == "Yes") %>%
  rename(base_name = Pokemon) %>%
  inner_join(mega_table) %>%
  select(-c(`pokedex number`, base_name)) %>%
  rename(Pokemon = Mega_name) %>%
  mutate(shadow = if_else(dust_status == "Shadow", TRUE, FALSE)) %>%
  mutate(attack_iv = if_else(is.na(attack_iv), 0, attack_iv)) %>%
  mutate(defence_iv  = if_else(is.na(defence_iv ), 0, defence_iv )) %>%
  mutate(hp_iv  = if_else(is.na(hp_iv ), 0, hp_iv )) %>%
  select(pokemon_instance_id,
        pokemon_id = Pokemon,
        level = `Level`,
        iv_atk = attack_iv,
        iv_def = defence_iv ,
        iv_sta = hp_iv ,
        shadow,
        fast_move_id = fast_move,
        charged_move_id = `Charge1`,
        Charge2) %>%
  bind_rows(user_pokemon)

user_pokemon <- bind_rows(user_pokemon %>%
  filter(!is.na(Charge2), !is.null(Charge2)) %>%
  select(-charged_move_id) %>%
  rename(charged_move_id = Charge2),

  user_pokemon %>%
    # filter(is.na(Charge2)) %>%
    select(-Charge2))

# existing_sims <- setdiff(existing_sims %>%
#   select(pokemon_id, level, fast_move_id, charged_move_id, shadow) %>%
#   distinct(),

# user_pokemon %>%
#   select(pokemon_id, level, fast_move_id, charged_move_id, shadow) %>%
#   distinct()) %>%
#   anti_join(existing_sims, .)

# saveRDS(existing_sims, "data/results_summary.RDS")


new_pokemon <- user_pokemon %>%
  # anti_join(
  #   existing_sims %>% 
  #     select(pokemon_id, level, fast_move_id, charged_move_id, shadow) %>%
  #     distinct()) %>%
  # rename(attacker_fast_move = fast_move,
        #  attacker_charge_move = charge_move) %>%
  # slice(1:1000) %>%
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

# rm(existing_sims)

sim_grid <- tidyr::crossing(
  new_pokemon,
  bosses)  %>%
  mutate(weather = "Extreme")

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
              friendship = "best"
            ))) %>%
        mutate(
          dps    = map_dbl(sim, "dps"),
          damage = map_dbl(sim, "damage_done"),
          time   = map_dbl(sim, "time"),
          weather = "Extreme",
          friendship = "best",
          user_id = user_id) %>%
        select(user_id,
              friendship,
              pokemon_instance_id,
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
              raid_tier = tier,shadow)
      # add_user_battle_results(user_result) 
    }
    , .options = furrr_options(packages = "pokemonGoSim")) %>%
    list_rbind()

})

# existing_sims <- readRDS("data/results_summary.RDS")

# results_summary <- sim_list %>%
#   mutate(
#     dps    = map_dbl(sim, "dps"),
#     damage = map_dbl(sim, "damage_done"),
#     time   = map_dbl(sim, "time"),
#     weather = "Extreme",
#     friendship = "best",
#     user_id = user_id
#   ) %>%
#   select(
#     user_id,
#     friendship,
#     pokemon_instance_id,
#     pokemon_name = pokemon_id,
#     raid_boss = Pokemon,
#     level,
#     fast_move_id,
#     charged_move_id,
#     boss_fast_move_id = fast_move,
#     boss_charged_move_id = charge_move,
#     dps,
#     damage,
#     time,
#     weather,
#     raid_tier = tier,
#     shadow
#    ) # %>%
#   #   bind_rows(existing_sims)

# # saveRDS(results_summary, "data/results_summary.RDS")

# con <- DBI::dbConnect(duckdb::duckdb(), "data/pokemon.db")

add_user_battle_results(sim_list)
# DBI::dbDisconnect(con, shutdown = TRUE)
