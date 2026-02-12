library(tidyverse)

results_summary <- readRDS("data/results_summary.RDS")

boss_summary <- results_summary %>%
  group_by(raid_boss, boss_fast_move_id, boss_charged_move_id, weather) %>%
  mutate(dmg_rank = rank(desc(damage), ties.method = "random")) %>%
  filter(dmg_rank <= 6) %>%
  summarise(damage = sum(damage),
            time = sum(time)) %>%
  mutate(dps = damage/time) %>%
  group_by(raid_boss) %>%
  summarise(min_dps = min(dps),
            max_dps = max(dps),
            mean_dps = mean(dps))

boss_summary %>%
  arrange(mean_dps) %>%                       # order by mean
  mutate(raid_boss = factor(raid_boss, levels = raid_boss)) %>%
  ggplot(aes(y = raid_boss)) +
  
  # Range stick (min to max)
  geom_segment(aes(x = min_dps, xend = max_dps,
                   yend = raid_boss),
               linewidth = 1.2,
               colour = "grey60") +
  
  # Mean point (the lollipop head)
  geom_point(aes(x = mean_dps),
             size = 4,
             colour = "#2C7FB8") +
  
  labs(
    x = "DPS",
    y = NULL,
    title = "Min, Mean, and Max DPS per Raid Boss"
  ) +
  theme_minimal(base_size = 13)


