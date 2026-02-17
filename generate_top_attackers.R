library(tidyverse)
library(doFuture)
library(future)
library(progressr)
library(furrr)

options(progressr.enable = TRUE)
options(future.globals.maxSize= 891289600)

## Variables

PARTY_POWER_MULT <- 2
BOSS_CHARGE_COOLDOWN <- 2.5

raid_boss_tiers <- tibble::tibble(
  tier = c(6, 5, 4),
  hp = c(22500, 15000, 9000),
  cpm = c(0.7903, 0.7903, 0.7903)
)

## Data

type_effectiveness <- readRDS("data/type_effectiveness.RDS") 

get_type_effectiveness <- function(atk, def1, def2 = NA) {
  if (is.na(def2)) {
  type_effectiveness %>%
    filter(Attack == !!atk,
          Defence_1 == !!def1,
          is.na(Defence_2)
        ) %>%
    pull(`Damage Multiplier`)
  } else if (!is.na(def2)) {
      type_effectiveness %>%
    filter(Attack == !!atk,
          Defence_1 == !!def1,
          Defence_2 == !!def2
        ) %>%
    pull(`Damage Multiplier`)
  } else {1}


}

mega_table <- readRDS("data/mega_table.RDS")

raid_bosses <- readRDS("data/base_stats.rds") %>%
  # filter(`Raid Boss Tier` > 3) %>%
  filter(`Raid Boss Tier` > 5) %>%
  select(Pokemon = name, tier = `Raid Boss Tier`) %>%
  distinct() %>%
  left_join(raid_boss_tiers)

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



#         level = `Level`,
#         iv_atk = `Attack IV`,
#         iv_def = `Defence IV`,
#         iv_sta = `HP IV`,


# user_pokemon <- readRDS("data/user_pokemon.rds") %>%
#   mutate(shadow = if_else(`Dust Status` == "Shadow", TRUE, FALSE)) %>%
#   mutate(`Attack IV` = if_else(is.na(`Attack IV`), 0, `Attack IV`)) %>%
#   mutate(`Defence IV` = if_else(is.na(`Defence IV`), 0, `Defence IV`)) %>%
#   mutate(`HP IV` = if_else(is.na(`HP IV`), 0, `HP IV`)) %>%
#   select(uuid = ID,
#         pokemon_id = Pokemon,
#         level = `Level`,
#         iv_atk = `Attack IV`,
#         iv_def = `Defence IV`,
#         iv_sta = `HP IV`,
#         shadow,
#         fast_move_id = `Fast Move`,
#         charged_move_id = `Charge1`,
#         Charge2)

# user_pokemon <- readRDS("data/user_pokemon.rds") %>%
#   filter(`Can Mega Evolve` == "Yes") %>%
#   rename(base_name = Pokemon) %>%
#   inner_join(mega_table) %>%
#   select(-c(`pokedex number`, base_name)) %>%
#   rename(Pokemon = Mega_name) %>%
#   mutate(shadow = if_else(`Dust Status` == "Shadow", TRUE, FALSE)) %>%
#   mutate(`Attack IV` = if_else(is.na(`Attack IV`), 0, `Attack IV`)) %>%
#   mutate(`Defence IV` = if_else(is.na(`Defence IV`), 0, `Defence IV`)) %>%
#   mutate(`HP IV` = if_else(is.na(`HP IV`), 0, `HP IV`)) %>%
#   select(uuid = ID,
#         pokemon_id = Pokemon,
#         level = `Level`,
#         iv_atk = `Attack IV`,
#         iv_def = `Defence IV`,
#         iv_sta = `HP IV`,
#         shadow,
#         fast_move_id = `Fast Move`,
#         charged_move_id = `Charge1`,
#       Charge2) %>%
#   bind_rows(user_pokemon)

# user_pokemon <- bind_rows(user_pokemon %>%
#   filter(!is.na(Charge2)) %>%
#   select(-charged_move_id) %>%
#   rename(charged_move_id = Charge2),

#   user_pokemon %>%
#     # filter(is.na(Charge2)) %>%
#     select(-Charge2))

pokemon_moves <- readRDS("data/pokemon_moves.rds")
moves <- readRDS("data/moves.rds")
weather_boosts <- readRDS("data/weather.rds")

pokemon_ids <- readRDS("data/pokemon_ids.rds")
move_ids <- readRDS("data/move_ids.rds")




level_multipliers <- readRDS("data/levels.rds") %>%
  select(level = `Level`, cpm = `CP Multiplier`)

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

## Functions

calc_pokemon_stats <- function(
  pokemon_id,
  level,
  iv_atk = 15,
  iv_def = 15,
  iv_sta = 15,
  shadow = FALSE
) {

  base <- base_stats |> dplyr::filter(pokemon_id == !!pokemon_id)
  cpm  <- level_multipliers |> dplyr::filter(level == !!level) |> dplyr::pull(cpm)

  atk <- (base$base_atk + iv_atk) * cpm
  def <- (base$base_def + iv_def) * cpm
  sta <- floor((base$base_sta + iv_sta) * cpm)

  if (shadow) {
    atk <- atk * 1.2
    def <- def * 0.8333333
  }

  list(
    atk = atk,
    def = def,
    hp  = sta,
    type1 = base$type1,
    type2 = base$type2
  )
}

calc_boss_stats <- function(pokemon_id, tier) {

  base <- base_stats |> dplyr::filter(pokemon_id == !!pokemon_id)
  tier_data <- raid_boss_tiers |> dplyr::filter(tier == !!tier)

  atk <- base$base_atk * tier_data$cpm
  def <- base$base_def * tier_data$cpm

  list(
    atk = atk,
    def = def,
    hp  = tier_data$hp,
    type1 = base$type1,
    type2 = base$type2
  )
}

build_attacker <- function(
  pokemon_id,
  level,
  fast_move_id,
  charged_move_id
) {

  stats <- calc_pokemon_stats(
    pokemon_id = pokemon_id,
    level = level,
    shadow = grepl("shadow", pokemon_id)
  )

  fast <- moves |> dplyr::filter(move_id == fast_move_id)
  charged <- moves |> dplyr::filter(move_id == charged_move_id)

  list(
    hp = stats$hp,
    atk = stats$atk,
    def = stats$def,
    energy = 0,
    party_power = 0,
    next_action_time = 0,

    type1 = stats$type1,
    type2 = stats$type2,

    fast = list(
      power = fast$power,
      duration = fast$duration_ms / 1000,
      energy = fast$energy_delta,
      type = fast$type
    ),

    charged = list(
      power = charged$power,
      duration = charged$duration_ms / 1000,
      energy = abs(charged$energy_delta),
      type = charged$type
    )
  )
}

build_boss <- function(
  pokemon_id,
  tier,
  fast_move_id,
  charged_move_id
) {

  stats <- calc_boss_stats(pokemon_id, tier)

  fast <- moves |> dplyr::filter(move_id == fast_move_id)
  charged <- moves |> dplyr::filter(move_id == charged_move_id)

  list(
    hp = stats$hp,
    atk = stats$atk,
    def = stats$def,
    energy = 0,
    next_action_time = 1.5,
    last_charged_time = -Inf,

    type1 = stats$type1,
    type2 = stats$type2,

    fast = list(
      power = fast$power,
      duration = fast$duration_ms / 1000,
      type = fast$type
    ),

    charged = list(
      power = charged$power,
      duration = charged$duration_ms / 1000,
      energy = abs(charged$energy_delta),
      type = charged$type
    )
  )
}



calc_damage <- function(
  power,
  atk,
  def,
  move_type,
  attacker_types,
  weather,
  friendship = "none", 
  stab,
  type_effectiveness
) {
  # stab <- if (move_type %in% attacker_types) 1.2 else 1.0

  weather_mult <- get_weather_multiplier(move_type, weather)
  friendship_mult <- get_friendship_multiplier(friendship)

  floor(
    0.5 *
    power *
    (atk / def) *
    stab *
    weather_mult *
    friendship_mult *
    type_effectiveness
  ) + 1
}

party_power_gain <- function(fast_duration) {
  # Niantic-style approximation
  fast_duration * 10
}

do_fast_move <- function(attacker, boss, time, weather, friendship) {
  stab <- get_stab(
    move_type = attacker$fast$type,
    type1 = attacker$type1,
    type2 = attacker$type2
  )

  type_effectiveness <- get_type_effectiveness(attacker$fast$type, boss$type1, boss$type2)

  dmg <- calc_damage(
    power = attacker$fast$power,
    atk = attacker$atk,
    def = boss$def,
    stab = stab,
    weather = weather,
    friendship = friendship,
    move_type = attacker$fast$type,
    type_effectiveness = type_effectiveness
  )

  boss$hp <- boss$hp - dmg
  attacker$energy <- min(100, attacker$energy + attacker$fast$energy)

  attacker$party_power <- min(
    100,
    attacker$party_power + party_power_gain(attacker$fast$duration)
  )

  attacker$next_action_time <- time + attacker$fast$duration

  list(attacker = attacker, boss = boss)
}

do_charged_move <- function(attacker, boss, time, weather, friendship) {
  is_party <- attacker$party_power >= 100

  mult <- if (is_party) PARTY_POWER_MULT else 1
  duration <- if (is_party) 0 else attacker$charged$duration

  stab <- get_stab(
    move_type = attacker$charged$type,
    type1 = attacker$type1,
    type2 = attacker$type2
  )
  
  type_effectiveness <- get_type_effectiveness(attacker$charged$type, boss$type1, boss$type2)

  dmg <- calc_damage(
    power = attacker$charged$power * mult,
    atk = attacker$atk,
    def = boss$def,
    stab = stab,
    weather = weather,
    friendship = friendship,
    move_type = attacker$charged$type,
    type_effectiveness = type_effectiveness

  )

  boss$hp <- boss$hp - dmg
  attacker$energy <- attacker$energy - attacker$charged$energy

  if (is_party) {
    attacker$party_power <- 0
  }

  attacker$next_action_time <- time + duration

  list(attacker = attacker, boss = boss)
}

get_stab <- function(move_type, type1, type2) {
  if (move_type == type1 || move_type == type2) {
    1.2
  } else {
    1
  }
}

get_weather_multiplier <- function(move_type, weather) {
  boosted <- weather_boosts %>%
    dplyr::filter(
      weather == !!weather,
      type == !!move_type
    ) %>%
    nrow() > 0

  if (boosted) 1.2 else 1.0
}

get_friendship_multiplier <- function(friendship) {
  dplyr::case_when(
    friendship == "good"  ~ 1.03,
    friendship == "great" ~ 1.05,
    friendship == "ultra" ~ 1.07,
    friendship == "best"  ~ 1.10,
    TRUE                  ~ 1.00
  )
}

do_boss_fast <- function(attacker, boss, time, weather, friendship = "none") {

  stab <- get_stab(
    boss$fast$type,
    boss$type1,
    boss$type2
  )

  type_effectiveness <- get_type_effectiveness(boss$charged$type, attacker$type1, attacker$type2)

  dmg <- calc_damage(
    power = boss$fast$power,
    atk   = boss$atk,
    def   = attacker$def,
    stab  = stab,
    weather = weather,
    move_type = boss$fast$type,
    type_effectiveness = type_effectiveness
  )

  attacker$hp <- attacker$hp - dmg

  # Boss gains energy from fast moves
  boss$energy <- min(100, boss$energy + boss$fast$power * 0.5)

  boss$next_action_time <- time + boss$fast$duration

  list(attacker = attacker, boss = boss)
}

do_boss_charged <- function(attacker, boss, time, weather, friendship = "none") {

  stab <- get_stab(
    boss$charged$type,
    boss$type1,
    boss$type2
  )

  type_effectiveness <- get_type_effectiveness(boss$charged$type, attacker$type1, attacker$type2)

  dmg <- calc_damage(
    power = boss$charged$power,
    atk   = boss$atk,
    def   = attacker$def,
    stab  = stab,
    weather = weather,
    move_type = boss$charged$type,
    type_effectiveness = type_effectiveness
  )

  attacker$hp <- attacker$hp - dmg
  boss$energy <- boss$energy - abs(boss$charged$energy)

  boss$last_charged_time <- time
  boss$next_action_time <- time + boss$charged$duration

  list(attacker = attacker, boss = boss)
}


boss_can_charge <- function(boss, time) {
  boss$energy >= abs(boss$charged$energy) &&
    (time - boss$last_charged_time) >= BOSS_CHARGE_COOLDOWN
}

simulate_battle_timeline <- function(attacker, boss, max_time = 300, weather, friendship) {
  time <- 0
  damage_done <- 0
  starting_hp <- boss$hp
  # browser()
  while (
    attacker$hp > 0 &&
      boss$hp > 0 &&
      time < max_time
  ) {
    next_event_time <- min(
      attacker$next_action_time,
      boss$next_action_time
    )

    time <- next_event_time

if (attacker$next_action_time <= boss$next_action_time) {

  if (attacker$energy >= attacker$charged$energy) {
    res <- do_charged_move(attacker, boss, time, weather, friendship)
  } else {
    res <- do_fast_move(attacker, boss, time, weather, friendship)
  }

} else {

  if (boss_can_charge(boss, time)) {
    res <- do_boss_charged(attacker, boss, time, weather)
  } else {
    res <- do_boss_fast(attacker, boss, time, weather)
  }

}

    attacker <- res$attacker
    boss <- res$boss
  }

  list(
    time = time,
    boss_hp = boss$hp,
    attacker_hp = attacker$hp,
    damage_done = max(0, starting_hp - boss$hp),
    dps = max(0, starting_hp - boss$hp) / time
  )
}

long_attacker_list <- available_pokemon %>%
  inner_join(available_move_combinations %>% rename(pokemon_id = Pokemon)) %>%
  filter(type1 %in% c("Electric", "Grass", "Water") |
        type2 %in% c("Electric", "Grass", "Water")) %>%
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
        charged_move_id = attacker_charge_move
      )
    )
  ) %>%
  ungroup()




bosses <- boss_move_combinations %>%
  inner_join(raid_bosses) %>%
  filter(Pokemon %in% c("Primal Kyogre", "Primal Groudon")) %>%
    rename(boss_fast_move = fast_move,
         boss_charge_move = charge_move) %>%
  rowwise() %>%
  mutate(boss = list(
    build_boss(
      pokemon_id = Pokemon,
      tier = tier,
      fast_move_id = boss_fast_move,
      charged_move_id = boss_charge_move
  ))) %>%
  ungroup


# weathers <- weather_boosts %>% select(weather) %>% distinct()

sim_grid <- tidyr::crossing(
  long_attacker_list,
  bosses)

rm(available_move_combinations, available_pokemon,
   boss_move_combinations, long_attacker_list)






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
              weather    = "Extreme",
              friendship = "best"
            )
        ))
    }
  ) %>%
    list_rbind()

})


results_summary <- sim_list %>%
  mutate(
    dps    = map_dbl(sim, "dps"),
    damage = map_dbl(sim, "damage_done"),
    time   = map_dbl(sim, "time"),
    weather = "Extreme",
    uuid = row_number(),
    pokemon_id = if_else(shadow == TRUE, paste0("Shadow ", form), form),
  ) %>%
  select(
    uuid,
    pokemon_id,
    raid_boss = Pokemon,
    level,
    fast_move_id = attacker_fast_move,
    charged_move_id = attacker_charge_move,
    boss_fast_move_id = boss_fast_move,
    boss_charged_move_id = boss_charge_move,
    dps,
    damage,
    time,
    weather,
    tier
  )

saveRDS(results_summary, "data/hypotectical_matchups.RDS")
