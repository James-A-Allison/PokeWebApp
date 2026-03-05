library(tidyverse)
library(pokemonGoSim)

base_stats <- readRDS("data/base_stats.rds") %>%
  select(
    pokemon_id = name,
    name,
    form = name,
    base_atk = `Attack Go`,
    base_def = `Defence Go`,
    base_sta = `HP Go`,
    type1 = `Type 1`,
    type2 = `Type 2`,
    class = Class
  )

pokemon_moves <- readRDS("data/pokemon_moves.rds")
moves <- readRDS("data/moves.rds")

pokemon_ids <- readRDS("data/pokemon_ids.rds")
move_ids <- readRDS("data/move_ids.rds")

available_move_combinations <- inner_join(
  pokemon_moves %>%
    left_join(move_ids %>% rename(`Move Name` = name)) %>%
    left_join(pokemon_ids %>% rename(`Pokemon` = name)) %>%
    select(Pokemon, `Move Name`, legacy) %>%
    inner_join(moves %>% filter(`Move Type` == "Fast"), relationship = "many-to-many") %>%
    select(Pokemon, fast_move = `Move Name`, elite_fast_tm = legacy),

  pokemon_moves %>%
    left_join(move_ids %>% rename(`Move Name` = name)) %>%
    left_join(pokemon_ids %>% rename(`Pokemon` = name)) %>%
    select(Pokemon, `Move Name`, legacy) %>%
    inner_join(moves %>% filter(`Move Type` == "Charge"), relationship = "many-to-many") %>%
    select(Pokemon, charge_move = `Move Name`, elite_charged_tm = legacy), relationship = "many-to-many" 
)

base_table <- available_move_combinations %>%
  left_join(moves %>%
  rename_with(tolower) %>%
  rename_with(~ paste0("fast_", .x)) %>%
  rename_with(~ gsub(" ", "_", .x)) %>%
  filter(fast_move_type == "Fast") %>%
  select(fast_move = fast_move_name, fast_category, fast_power, fast_duration, fast_energy_gain)) %>%
  left_join(moves %>%
  rename_with(tolower) %>%
  rename_with(~ paste0("charge_", .x)) %>%
  rename_with(~ gsub(" ", "_", .x)) %>%
  filter(charge_move_type == "Charge") %>%
  select(charge_move = charge_move_name, charge_category, charge_power, charge_duration, charge_energy_cost)) %>%
  inner_join(base_stats %>% rename(Pokemon = pokemon_id)) %>%
  mutate(fast_move_required = floor(charge_energy_cost / fast_energy_gain), 
        cycle_time = (fast_duration * fast_move_required) + charge_duration, 
        stab_fast = if_else(fast_category == type1 | fast_category == type2, 1.2, 1),
        stab_charge = if_else(charge_category == type1 | charge_category == type2, 1.2, 1), 
        fast_damage = (floor(base_atk * fast_power * stab_fast) + 1) * fast_move_required,
        charge_damage = base_atk * charge_power * stab_charge * 1,
        damage_output = fast_damage + charge_damage,
        dps = damage_output / cycle_time)


unique_types <- base_stats %>% select(type1) %>% distinct() %>% pull()

base_attacker_candidates <- tibble()

for (i in 1:length(unique_types)) {
  
  loop_type <- unique_types[i]

  base_attacker_candidates <- base_table %>%
    filter(fast_category == loop_type | charge_category == loop_type) %>%
    filter(class == "Base") %>%
    group_by(Pokemon, attack_type = loop_type) %>%
    summarise(max_dps = max(dps)) %>%
    ungroup %>%
    top_n(n = 10, wt = max_dps) %>%
    bind_rows(base_attacker_candidates)
}

useful_base_attackers <- base_attacker_candidates %>% select(Pokemon) %>% distinct()

saveRDS(useful_base_attackers, "data/useful_base_attackers.RDS")

base_dps <- base_table %>%
  filter(elite_charged_tm == "No", elite_fast_tm == "No") %>%
  group_by(Pokemon) %>%
  top_n(n = 1, wt = dps) %>%
  select(Pokemon, max_base_dps = dps)

elite_fast_tm_dps <- base_table %>%
  filter(elite_fast_tm == "Yes") %>%
  group_by(Pokemon) %>%
  top_n(n = 1, wt = dps) %>%
  select(Pokemon, fast_move, charge_move, elite_fast_tm_dps = dps) %>%
  left_join(base_dps) %>%
  mutate(dps_percent_increase = elite_fast_tm_dps / max_base_dps,
          dps_abs_increase = elite_fast_tm_dps / max_base_dps) %>%
  arrange(desc(dps_percent_increase))

elite_charged_tm_dps <- base_table %>%
  filter(elite_charged_tm == "Yes") %>%
  group_by(Pokemon) %>%
  top_n(n = 1, wt = dps) %>%
  select(Pokemon, fast_move, charge_move, elite_charged_tm_dps = dps) %>%
  left_join(base_dps) %>%
  mutate(dps_percent_increase = elite_charged_tm_dps / max_base_dps,
        dps_abs_increase = elite_charged_tm_dps - max_base_dps) %>%
  arrange(desc(dps_percent_increase))

saveRDS(elite_fast_tm_dps, "data/elite_fast_tm_dps.RDS")
saveRDS(elite_charged_tm_dps, "data/elite_charged_tm_dps.RDS")


two_attack_candidates <- tibble()

for (i in 1:length(unique_types)) {
  
  loop_type <- unique_types[i]

  two_attack_candidates <- base_table %>%
    filter(charge_category == loop_type) %>%
    # filter(class == "Base") %>%
    group_by(Pokemon, attack_type = loop_type) %>%
    summarise(max_dps = max(dps)) %>%
    ungroup %>%
    top_n(n = 6, wt = max_dps) %>%
    bind_rows(two_attack_candidates)
}

two_attack_candidates %>%
  group_by(Pokemon) %>%
  mutate(Types = n_distinct(attack_type)) %>%
  filter(Types > 1)


useful_base_attackers <- two_attack_candidates %>% select(Pokemon) %>% distinct()