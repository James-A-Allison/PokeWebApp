library(googlesheets4)

base_stats <- read_sheet("https://docs.google.com/spreadsheets/d/1cjTin49W2AkW9Z2ndJ59IDZ3o64FGtj2LhYrx-QoxHg/",
                          sheet = "Base Stats")

levels <- read_sheet("https://docs.google.com/spreadsheets/d/1cjTin49W2AkW9Z2ndJ59IDZ3o64FGtj2LhYrx-QoxHg/",
                    sheet = "Levels")

saveRDS(base_stats, "data/base_stats.RDS")
saveRDS(levels, "data/levels.RDS")                    