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

base_stats <- readRDS("data/base_stats.rds") %>%
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

pokemon_moves <- readRDS("data/pokemon_moves.rds")
moves <- readRDS("data/moves.rds")
# weather_boosts <- readRDS("data/weather.rds")

pokemon_ids <- readRDS("data/pokemon_ids.rds")
move_ids <- readRDS("data/move_ids.rds")

mega_table <- readRDS("data/mega_table.RDS")
# type_effectiveness <- readRDS("data/type_effectiveness.RDS") 
# level_multipliers <- readRDS("data/levels.rds") %>%
#   select(level = `Level`, cpm = `CP Multiplier`)


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


## Users

existing_sims <- readRDS("data/results_summary.RDS")

user_pokemon <- readRDS("data/user_pokemon.rds") %>%
  mutate(shadow = if_else(`Dust Status` == "Shadow", TRUE, FALSE)) %>%
  mutate(`Attack IV` = if_else(is.na(`Attack IV`), 0, `Attack IV`)) %>%
  mutate(`Defence IV` = if_else(is.na(`Defence IV`), 0, `Defence IV`)) %>%
  mutate(`HP IV` = if_else(is.na(`HP IV`), 0, `HP IV`)) %>%
  select(uuid = ID,
        pokemon_id = Pokemon,
        level = `Level`,
        iv_atk = `Attack IV`,
        iv_def = `Defence IV`,
        iv_sta = `HP IV`,
        shadow,
        fast_move_id = `Fast Move`,
        charged_move_id = `Charge1`,
        Charge2)

user_pokemon <- readRDS("data/user_pokemon.rds") %>%
  filter(`Can Mega Evolve` == "Yes") %>%
  rename(base_name = Pokemon) %>%
  inner_join(mega_table) %>%
  select(-c(`pokedex number`, base_name)) %>%
  rename(Pokemon = Mega_name) %>%
  mutate(shadow = if_else(`Dust Status` == "Shadow", TRUE, FALSE)) %>%
  mutate(`Attack IV` = if_else(is.na(`Attack IV`), 0, `Attack IV`)) %>%
  mutate(`Defence IV` = if_else(is.na(`Defence IV`), 0, `Defence IV`)) %>%
  mutate(`HP IV` = if_else(is.na(`HP IV`), 0, `HP IV`)) %>%
  select(uuid = ID,
        pokemon_id = Pokemon,
        level = `Level`,
        iv_atk = `Attack IV`,
        iv_def = `Defence IV`,
        iv_sta = `HP IV`,
        shadow,
        fast_move_id = `Fast Move`,
        charged_move_id = `Charge1`,
      Charge2) %>%
  bind_rows(user_pokemon)

user_pokemon <- bind_rows(user_pokemon %>%
  filter(!is.na(Charge2)) %>%
  select(-charged_move_id) %>%
  rename(charged_move_id = Charge2),

  user_pokemon %>%
    # filter(is.na(Charge2)) %>%
    select(-Charge2))

existing_sims <- setdiff(existing_sims %>%
  select(pokemon_id, level, fast_move_id, charged_move_id, shadow) %>%
  distinct(),

user_pokemon %>%
  select(pokemon_id, level, fast_move_id, charged_move_id, shadow) %>%
  distinct()) %>%
  anti_join(existing_sims, .)

saveRDS(existing_sims, "data/results_summary.RDS")


new_pokemon <- user_pokemon %>%
  anti_join(
    existing_sims %>% 
      select(pokemon_id, level, fast_move_id, charged_move_id, shadow) %>%
      distinct()) %>%
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

rm(existing_sims)

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

      sim_grid %>%
        slice(i) %>%
        mutate(sim = map2(
          attacker,
          boss,
          ~ simulate_battle_timeline(
              attacker   = .x,
              boss       = .y,
              weather    = weather,
              friendship = "best"
            )
        ))
    }
    , .options = furrr_options(packages = "pokemonGoSim")) %>%
    list_rbind()

})

existing_sims <- readRDS("data/results_summary.RDS")

results_summary <- sim_list %>%
  mutate(
    dps    = map_dbl(sim, "dps"),
    damage = map_dbl(sim, "damage_done"),
    time   = map_dbl(sim, "time"),
    weather = "Extreme"
  ) %>%
  select(
    uuid,
    pokemon_id,
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
    tier,
    shadow
  ) %>%
    bind_rows(existing_sims)

saveRDS(results_summary, "data/results_summary.RDS")
