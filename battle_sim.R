library(tidyverse)
library(shiny)

base_stats <- readRDS("data/base_stats.rds") %>%
  select(pokemon_id = name,
        name,
        form = name,
        base_atk = `Attack Go`,
        base_def = `Defence Go`,
        base_sta = `HP Go`,
        type1 = `Type 1`,
        type2 = `Type 2`)

level_multipliers <- readRDS("data/levels.rds") %>%
  select(level = `Level`, cpm = `CP Multiplier`)

move_combinations <- inner_join(
  pokemon_moves %>%
    inner_join(moves %>% filter(`Move Type` == "Fast")) %>%
    select(Pokemon, fast_move = `Move Name`),

  pokemon_moves %>%
    inner_join(moves %>% filter(`Move Type` == "Charge")) %>%
    select(Pokemon, charge_move = `Move Name`)
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
        category = case_when(`Move Type` == "Fast" ~ "fast_move",
        `Move Type` == "Charge" ~ "charge_move")
      ) %>%
      select(
        move_id = `Move Name`,
        name = `Move Name`,
        category,
        power = Power,
        energy_delta,
        duration_ms,
        type = Category
      )
  )




raid_boss_tiers <- tibble::tibble(
  tier = c(5),
  hp = c(15000),
  cpm = c(0.7903)
)

attacker <- list(
  hp = 150,
  atk = 250,
  def = 180,
  energy = 0,
  party_power = 0,
  next_action_time = 0,
  fast = list(
    power = 8,
    duration = 0.6,   # seconds
    energy = 8,
    type = "Psychic"
  ),
  charged = list(
    power = 90,
    duration = 2.7,
    energy = 50,
    type = "Psychic"
  )
)

boss <- list(
  hp = 15000,
  atk = 300,
  def = 200,
  next_action_time = 1.5,   # bosses don’t act immediately
  fast = list(
    power = 12,
    duration = 1.0,
    type = "Dark"
  )
)
calc_damage <- function(power, atk, def, stab = 1, effectiveness = 1, bonus = 1) {
  floor(0.5 * power * (atk / def) * stab * effectiveness * bonus) + 1
}
party_power_gain <- function(fast_duration) {
  # Niantic-style approximation
  fast_duration * 10
}

PARTY_POWER_MULT <- 2
do_fast_move <- function(attacker, boss, time) {

  dmg <- calc_damage(
    power = attacker$fast$power,
    atk   = attacker$atk,
    def   = boss$def,
    stab  = 1.2
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

  dmg <- calc_damage(
    power = attacker$charged$power * mult,
    atk   = attacker$atk,
    def   = boss$def,
    stab  = 1.2
  )

  boss$hp <- boss$hp - dmg
  attacker$energy <- attacker$energy - attacker$charged$energy

  if (is_party) {
    attacker$party_power <- 0
  }

  attacker$next_action_time <- time + duration

  list(attacker = attacker, boss = boss)
}
do_boss_fast <- function(attacker, boss, time) {

  dmg <- calc_damage(
    power = boss$fast$power,
    atk   = boss$atk,
    def   = attacker$def
  )

  attacker$hp <- attacker$hp - dmg
  boss$next_action_time <- time + boss$fast$duration

  list(attacker = attacker, boss = boss)
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
      res <- do_boss_fast(attacker, boss, time)
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

result <- simulate_battle_timeline(attacker, boss)

result$dps
result$damage_done
