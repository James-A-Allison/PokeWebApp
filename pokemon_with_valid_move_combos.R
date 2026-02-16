pokemon <- readRDS("data/pokemon_ids.rds")
moves <- readRDS("data/move_ids.rds")
pokemon_moves <- readRDS("data/pokemon_moves.rds")

pokemon %>%
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
  summarise(Count = n())
