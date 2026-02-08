library(tidyverse)
library(shiny)

## Variables

PARTY_POWER_MULT <- 2
BOSS_CHARGE_COOLDOWN <- 2.5

raid_boss_tiers <- tibble::tibble(
  tier = c(5),
  hp = c(15000),
  cpm = c(0.7903)
)

## Data

user_pokemon <- readRDS("data/user_pokemon.rds") %>%
  mutate(shadow = if_else(`Dust Status` == "Shadow", TRUE, FALSE)) %>%
  mutate(`Attack IV` = if_else(is.na(`Attack IV`), 0, `Attack IV`)) %>%
  mutate(`Defence IV` = if_else(is.na(`Defence IV`), 0, `Defence IV`)) %>%
  mutate(`HP IV` = if_else(is.na(`HP IV`), 0, `HP IV`)) %>%
  select(pokemon_id = Pokemon,
        level = `Level`,
        iv_atk = `Attack IV`,
        iv_def = `Defence IV`,
        iv_sta = `HP IV`,
          shadow,
        fast_move_id = `Fast Move`,
      charged_move_id = `Charge1`)

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

level_multipliers <- readRDS("data/levels.rds") %>%
  select(level = `Level`, cpm = `CP Multiplier`)

move_combinations <- inner_join(
  pokemon_moves %>%
    inner_join(moves %>% filter(`Move Type` == "Fast"), relationship = "many-to-many") %>%
    select(Pokemon, fast_move = `Move Name`),

  pokemon_moves %>%
    inner_join(moves %>% filter(`Move Type` == "Charge"), relationship = "many-to-many") %>%
    select(Pokemon, charge_move = `Move Name`), relationship = "many-to-many" 
) %>%
  pivot_longer(
    cols = c("fast_move", "charge_move"),
    names_to = "category",
    values_to = "name"
  ) %>%
  inner_join(
    moves %>%
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
        type = Category
      ), relationship = "many-to-many"
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



calc_damage <- function(power, atk, def, stab = 1, effectiveness = 1, bonus = 1) {
  floor(0.5 * power * (atk / def) * stab * effectiveness * bonus) + 1
}

party_power_gain <- function(fast_duration) {
  # Niantic-style approximation
  fast_duration * 10
}

do_fast_move <- function(attacker, boss, time) {
  stab <- get_stab(
    move_type = attacker$fast$type,
    type1 = attacker$type1,
    type2 = attacker$type2
  )

  dmg <- calc_damage(
    power = attacker$fast$power,
    atk = attacker$atk,
    def = boss$def,
    stab = stab
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

do_charged_move <- function(attacker, boss, time) {
  is_party <- attacker$party_power >= 100

  mult <- if (is_party) PARTY_POWER_MULT else 1
  duration <- if (is_party) 0 else attacker$charged$duration

  stab <- get_stab(
    move_type = attacker$charged$type,
    type1 = attacker$type1,
    type2 = attacker$type2
  )

  dmg <- calc_damage(
    power = attacker$charged$power * mult,
    atk = attacker$atk,
    def = boss$def,
    stab = stab
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

do_boss_fast <- function(attacker, boss, time) {

  stab <- get_stab(
    boss$fast$type,
    boss$type1,
    boss$type2
  )

  dmg <- calc_damage(
    power = boss$fast$power,
    atk   = boss$atk,
    def   = attacker$def,
    stab  = stab
  )

  attacker$hp <- attacker$hp - dmg

  # Boss gains energy from fast moves
  boss$energy <- min(100, boss$energy + boss$fast$power * 0.5)

  boss$next_action_time <- time + boss$fast$duration

  list(attacker = attacker, boss = boss)
}

do_boss_charged <- function(attacker, boss, time) {

  stab <- get_stab(
    boss$charged$type,
    boss$type1,
    boss$type2
  )

  dmg <- calc_damage(
    power = boss$charged$power,
    atk   = boss$atk,
    def   = attacker$def,
    stab  = stab
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

simulate_battle_timeline <- function(attacker, boss, max_time = 300) {
  time <- 0
  damage_done <- 0

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
    res <- do_charged_move(attacker, boss, time)
  } else {
    res <- do_fast_move(attacker, boss, time)
  }

} else {

  if (boss_can_charge(boss, time)) {
    res <- do_boss_charged(attacker, boss, time)
  } else {
    res <- do_boss_fast(attacker, boss, time)
  }

}

    attacker <- res$attacker
    boss <- res$boss
  }

  list(
    time = time,
    boss_hp = boss$hp,
    attacker_hp = attacker$hp,
    damage_done = max(0, 15000 - boss$hp),
    dps = max(0, 15000 - boss$hp) / time
  )
}

attacker <- build_attacker(
  pokemon_id = "Mewtwo",
  level = 50,
  fast_move_id = "Psycho Cut",
  charged_move_id = "Hyper Beam"
)

boss <- build_boss(
  pokemon_id = "Mewtwo",
  tier = 5,
  fast_move_id = "Psycho Cut",
  charged_move_id = "Hyper Beam"
)

result <- simulate_battle_timeline(attacker, boss)

result$dps
result$damage_done
result

user_pokemon %>%
  slice(1) %>%
  build_attacker(
    pokemon_id = pokemon_id,
    level = level,
    fast_move_id = fast_move_id,
    charged_move_id = charged_move_id
   # shadow = shadow,
   # iv_atk = iv_atk,
   # iv_def = iv_def,
   # iv_sta = iv_sta
  )

attackers <- user_pokemon %>%
  rowwise() %>%
  mutate(
    attacker = list(
      build_attacker(
        pokemon_id      = pokemon_id,
        level           = level,
        fast_move_id    = fast_move_id,
        charged_move_id = charged_move_id
      )
    )
  ) %>%
  ungroup()
