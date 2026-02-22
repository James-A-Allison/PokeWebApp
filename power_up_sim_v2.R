library(tidyverse)
library(doFuture)
library(future)
library(progressr)
library(furrr)
# devtools::install("C:/Users/james/pokemonGoSim")
library(pokemonGoSim)

dust_table <-readRDS("data/levels.RDS") %>%
  select(level = Level,
        normal = `Marginal Dust`,
        eternatus = `Marginal Dust`,
        lucky = `Marginal Dust Lucky`,
        shadow = `Marginal Dust Shadow`,
        purified_dust = `Marginal Dust Purified`
      )

user_pokemon <- readRDS("data/user_pokemon.rds") %>%
  mutate(shadow = if_else(`Dust Status` == "Shadow", TRUE, FALSE)) %>%
  mutate(`Attack IV` = if_else(is.na(`Attack IV`), 0, `Attack IV`)) %>%
  mutate(`Defence IV` = if_else(is.na(`Defence IV`), 0, `Defence IV`)) %>%
  mutate(`HP IV` = if_else(is.na(`HP IV`), 0, `HP IV`)) %>%
  select(uuid = ID,
        pokemon_id = Pokemon,
        dust_status = `Dust Status`,
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
        dust_status = `Dust Status`,
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



max_level_with_dust_fast <- function(current_level,
                                     dust_limit,
                                     levels_tbl,
                                     dust_col = "normal_dust"
                                    ,max_level = 50) {
  dust_col = tolower(dust_col)
  # browser()
  # Pull chosen dust column once (fast)
  dusts_all <- levels_tbl[[dust_col]]
  levels_all <- levels_tbl$level
  
  available <- levels_all > current_level & levels_all <= max_level
  
  if (!any(available)) {
    return(list(
      final_level     = current_level,
      total_dust_used = 0,
      levels_gained   = 0
    ))
  }

  dusts <- dusts_all[available]
  levels <- levels_all[available]
  
  cumulative <- cumsum(dusts)
  
  affordable_idx <- which(cumulative <= dust_limit)
  
  if (length(affordable_idx) == 0) {
    return(list(
      final_level = current_level,
      total_dust_used = 0,
      levels_gained = 0
    ))
  }
  
  last_idx <- max(affordable_idx)
  
  return(tibble::tibble(
    final_level = levels[last_idx],
    total_dust_used = cumulative[last_idx],
    levels_gained = levels[last_idx] - current_level
  ))
}

user_pokemon %>%
  rowwise() %>%
  slice(1) %>%
  mutate(sim_level_up = list(max_level_with_dust_fast(
    current_level = level,
    dust_limit = 100000,
    levels_tbl = dust_table,
    max_level = 30,
    dust_col = dust_status
  ))) %>%  tidyr::unnest_wider(sim_level_up) %>%
  select(pokemon_id, level, final_level, total_dust_used, levels_gained) %>%
  arrange(desc(levels_gained))
