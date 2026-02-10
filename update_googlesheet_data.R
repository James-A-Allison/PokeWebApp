library(googlesheets4)
library(tidyverse)

base_stats <- read_sheet("https://docs.google.com/spreadsheets/d/1cjTin49W2AkW9Z2ndJ59IDZ3o64FGtj2LhYrx-QoxHg/",
                          sheet = "Base Stats")

levels <- read_sheet("https://docs.google.com/spreadsheets/d/1cjTin49W2AkW9Z2ndJ59IDZ3o64FGtj2LhYrx-QoxHg/",
                    sheet = "Levels")

moves <- read_sheet("https://docs.google.com/spreadsheets/d/1cjTin49W2AkW9Z2ndJ59IDZ3o64FGtj2LhYrx-QoxHg/",
                    sheet = "Moves")

pokemon_moves <- read_sheet("https://docs.google.com/spreadsheets/d/1cjTin49W2AkW9Z2ndJ59IDZ3o64FGtj2LhYrx-QoxHg/",
                    sheet = "Pokemon_Moves")

user_pokemon <- read_sheet("https://docs.google.com/spreadsheets/d/1cjTin49W2AkW9Z2ndJ59IDZ3o64FGtj2LhYrx-QoxHg/",
                    sheet = "User Pokemon") %>%
  filter(!is.na(`Pokemon`)) %>%
  select(ID, Pokemon, `Dust Status`, `Fast Move`, Charge1, Charge2, Level, `Attack IV`, `Defence IV`, `HP IV`)

type_effectiveness <- read_sheet("https://docs.google.com/spreadsheets/d/1cjTin49W2AkW9Z2ndJ59IDZ3o64FGtj2LhYrx-QoxHg/",
                          sheet = "Type Effectiveness Output") %>%
  select(`Attack`, `Defence_1`, `Defence_2`, `Damage Multiplier`) %>%
  mutate(`Defence_2` = if_else(`Defence_2` == `Defence_1`, NA_character_, `Defence_2`))

weather <- read_sheet("https://docs.google.com/spreadsheets/d/1cjTin49W2AkW9Z2ndJ59IDZ3o64FGtj2LhYrx-QoxHg/",
                    sheet = "Weather")

saveRDS(base_stats, "data/base_stats.RDS")
saveRDS(levels, "data/levels.RDS")
saveRDS(moves, "data/moves.RDS")                    
saveRDS(pokemon_moves, "data/pokemon_moves.RDS")                    
saveRDS(user_pokemon, "data/user_pokemon.RDS")                    
saveRDS(type_effectiveness, "data/type_effectiveness.RDS")                    
saveRDS(weather, "data/weather.RDS")                    
