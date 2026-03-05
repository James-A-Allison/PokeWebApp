library(tidyverse)

pokemon <- readRDS("data/pokemon_ids.rds")
moves <- readRDS("data/move_ids.rds")
pokemon_moves <- readRDS("data/pokemon_moves.rds")
mega_table <- readRDS("data/mega_table.RDS")

move_dex_completion <- pokemon %>%
  rename(Pokemon = name) %>%
  left_join(pokemon_moves) %>%
  left_join(moves %>% rename(Move_Name = name)) %>%
  filter(legacy == "No" | is.na(legacy)) %>%
  group_by(Pokemon, pokemon_id) %>%
  summarise(n_fast = n_distinct(move_id[category == "Fast"], na.rm = TRUE),
            n_charge = n_distinct(move_id[category == "Charge"], na.rm = TRUE)) %>%
  ungroup() %>%
  mutate(valid = if_else(n_fast > 0 & n_charge > 0, "Yes", "No")) %>%
  group_by(valid) %>%
  summarise(Count = n()) %>%
  mutate(Total = sum(Count),
        Share = Count/Total) %>%
  filter(valid == "Yes") %>%
  pull


completed_pokmon <- pokemon %>%
  rename(Pokemon = name) %>%
  inner_join(pokemon_moves) %>%
  select(base_name = Pokemon, move_id, legacy)

megas_to_add <- pokemon %>%
  rename(Pokemon = name) %>%
  left_join(pokemon_moves) %>%
  left_join(moves %>% rename(Move_Name = name)) %>%
  filter(legacy == "No" | is.na(legacy)) %>%
  group_by(Pokemon, pokemon_id) %>%
  summarise(n_fast = n_distinct(move_id[category == "Fast"], na.rm = TRUE),
            n_charge = n_distinct(move_id[category == "Charge"], na.rm = TRUE)) %>%
  ungroup() %>%
  mutate(valid = if_else(n_fast > 0 & n_charge > 0, "Yes", "No")) %>%
  filter(valid == "No",
          grepl("Mega ", Pokemon)) %>%
  rename(Mega_name = Pokemon)

megas_to_add %>%
  inner_join(mega_table) %>%
  select(Mega_name, pokemon_id, base_name) %>%
  inner_join(completed_pokmon) %>%
  select(pokemon_id, move_id, legacy) %>%
  bind_rows(pokemon_moves) %>%
  distinct() %>%
  saveRDS("data/pokemon_moves.RDS")

