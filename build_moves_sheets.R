library(googlesheets4)
library(tidyverse)

base_stats <- read_sheet("https://docs.google.com/spreadsheets/d/1cjTin49W2AkW9Z2ndJ59IDZ3o64FGtj2LhYrx-QoxHg/",
                          sheet = "Base Stats")

moves <- read_sheet("https://docs.google.com/spreadsheets/d/1cjTin49W2AkW9Z2ndJ59IDZ3o64FGtj2LhYrx-QoxHg/",
                    sheet = "Moves")

pokemon_moves <- read_sheet("https://docs.google.com/spreadsheets/d/1cjTin49W2AkW9Z2ndJ59IDZ3o64FGtj2LhYrx-QoxHg/",
                    sheet = "Pokemon_Moves")
           
pokemon <- base_stats %>%
  select(name, pokedex_number =  `pokedex number`) %>%
  distinct() %>%
  mutate(pokemon_id = row_number())


moves <- moves %>%
  select(name = `Move Name`, category = `Move Type`, type = Category) %>%
  distinct() %>%
  mutate(move_id = row_number())

to_update <- pokemon_moves %>%
  left_join(pokemon %>% rename(Pokemon = name)) %>%
  left_join(moves %>% rename(`Move Name` = name)) %>%
  select(pokemon_id, move_id, legacy = `Is Special`) 

  write_sheet(to_update, "https://docs.google.com/spreadsheets/d/1cjTin49W2AkW9Z2ndJ59IDZ3o64FGtj2LhYrx-QoxHg/", "pokemon_moves_2")
  write_sheet(pokemon, "https://docs.google.com/spreadsheets/d/1cjTin49W2AkW9Z2ndJ59IDZ3o64FGtj2LhYrx-QoxHg/", "pokemon_ids")
  write_sheet(moves, "https://docs.google.com/spreadsheets/d/1cjTin49W2AkW9Z2ndJ59IDZ3o64FGtj2LhYrx-QoxHg/", "move_ids")
