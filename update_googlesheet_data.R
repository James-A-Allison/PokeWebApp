library(googlesheets4)
library(tidyverse)
wb <- "https://docs.google.com/spreadsheets/d/1cjTin49W2AkW9Z2ndJ59IDZ3o64FGtj2LhYrx-QoxHg/"

base_stats <- read_sheet(,
                          sheet = "Base Stats")

levels <- read_sheet(wb,
                    sheet = "Levels")

moves <- read_sheet(wb,
                    sheet = "Moves")

pokemon_moves <- read_sheet(wb,
                    sheet = "pokemon_moves")

pokemon_ids <- read_sheet(wb, sheet = "pokemon_ids")
moves_ids <- read_sheet(wb, sheet = "move_ids")

pokemon_moves %>%
  left_join(moves_ids %>% rename(`Move Name` = name)) %>%
  left_join(pokemon_ids %>% rename(`Pokemon` = name)) %>%
  select(Pokemon, `Move Name`, legacy) %>%
  saveRDS("data/pokemon_moves.RDS")  

user_pokemon <- read_sheet(wb,
                    sheet = "User Pokemon") %>%
  filter(!is.na(`Pokemon`)) %>%
  select(ID, Pokemon, `Dust Status`, `Fast Move`, Charge1, Charge2, Level, `Attack IV`, `Defence IV`, `HP IV`)

type_effectiveness <- read_sheet(wb,
                          sheet = "Type Effectiveness Output") %>%
  select(`Attack`, `Defence_1`, `Defence_2`, `Damage Multiplier`)

weather <- read_sheet(wb,
                    sheet = "Weather")



saveRDS(base_stats, "data/base_stats.RDS")
saveRDS(levels, "data/levels.RDS")
saveRDS(moves, "data/moves.RDS")                    
saveRDS(pokemon_moves, "data/pokemon_moves.RDS")                    
saveRDS(type_effectiveness, "data/type_effectiveness.RDS")                    
saveRDS(weather, "data/weather.RDS")                    
