library(tidyverse)

base_stats <- readRDS("data/base_stats.RDS")
calendar <- readRDS("data/calendar.RDS")
type_effectiveness <- readRDS(("data/type_effectiveness.RDS"))

test <- base_stats %>%
  filter(`Raid Boss Tier` == 5,
        Released == "Yes") %>%
  select(name, Defence_1 = `Type 1`, Defence_2= `Type 2`) %>%
  inner_join(type_effectiveness, relationship = "many-to-many") %>%
  filter(`Damage Multiplier` > 1) %>%
  group_by(Attack, `Damage Multiplier`) %>%
  summarise(Count = n()) %>%
  pivot_wider(names_from = `Damage Multiplier`, values_from = Count)

type_effectiveness %>% select(Attack) %>% distinct() %>% arrange()

base_stats %>%
  filter(`Raid Boss Tier` == 5,
        Released == "Yes") %>%
  inner_join(calendar %>%
  filter(To >= Sys.Date()) %>%
  select(name = `Raid Boss`)) %>%
  select(name, Defence_1 = `Type 1`, Defence_2= `Type 2`) %>%
  inner_join(type_effectiveness, relationship = "many-to-many") %>%
  filter(`Damage Multiplier` > 1) %>%
  group_by(Attack, `Damage Multiplier`) %>%
  summarise(Count = n()) %>%
  pivot_wider(names_from = `Damage Multiplier`, values_from = Count, values_fill = list(Count = 0))

