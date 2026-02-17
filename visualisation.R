library(tidyverse)
library(shinydashboard)
library(shiny)

results_summary <- readRDS("data/results_summary.RDS")
powered_up_summary <- readRDS("data/powered_up_summary.RDS")
upcoming_raids <- readRDS("data/calendar.RDS") %>%
  filter(To >= Sys.Date()) %>%
  select(raid_boss = `Raid Boss`,
        tier = Tier)

ui <- dashboardPage(
  dashboardHeader(title = "Pokemon Go Strategic Team Builder"),
  dashboardSidebar(
    sidebarMenu(
      menuItem("Overview", tabName = "overview"),
      menuItem("Boss Deep Dive", tabName = "boss_deep_dive"),
      menuItem("Power Up Planner", tabName = "power_up_planner")
    )
  ),
  dashboardBody(
    tabItems(
      tabItem(tabName = "overview",
          fluidRow(
            box(plotOutput("summary_voilin")),
            box(tableOutput("useful_mons"))
                 )
             )
            ,
    tabItems(
      tabItem(tabName = "boss_deep_dive",
              h2("WIP")
      )
    ),
    tabItems(
      tabItem(tabName = "power_up_planner",
            h2("WIP 2"))
    )
  )
)
)


server <- function(input, output) { 
  output$summary_voilin <- renderPlot({
      results_summary %>%
        group_by(raid_boss, boss_fast_move_id, boss_charged_move_id, weather) %>%
        mutate(dmg_rank = rank(desc(damage), ties.method = "min")) %>%
        filter(dmg_rank <= 6) %>%
        summarise(damage = sum(damage),
                  time = sum(time)) %>%
        mutate(dps = damage/time) %>%
        group_by(raid_boss) %>%
        mutate(avg_dps = mean(dps)) %>%
        ungroup() %>%
        arrange(desc(avg_dps)) %>%
        ggplot(aes(y = reorder(raid_boss, avg_dps), x = dps)) +
        geom_violin()
  })

  output$useful_mons <- renderTable({
      results_summary %>%
        group_by(raid_boss, boss_fast_move_id, boss_charged_move_id, weather) %>%
        mutate(dmg_rank = rank(desc(damage), ties.method = "min")) %>%
        group_by(uuid, pokemon_id, level, fast_move_id, charged_move_id) %>%
        summarise(scenarios_top_6 = length(dmg_rank[dmg_rank <= 6]),
                  scenarios_top_12 = length(dmg_rank[dmg_rank <= 12])) %>%
        arrange(desc(scenarios_top_12))
  })
}

shinyApp(ui, server)

# boss_summary <- results_summary %>%
#   group_by(raid_boss, boss_fast_move_id, boss_charged_move_id, weather) %>%
#   mutate(dmg_rank = rank(desc(damage), ties.method = "min")) %>%
#   filter(dmg_rank <= 6) %>%
#   summarise(damage = sum(damage),
#             time = sum(time)) %>%
#   mutate(dps = damage/time) %>%
#   group_by(raid_boss) %>%
#   summarise(min_dps = min(dps),
#             max_dps = max(dps),
#             mean_dps = mean(dps))

# powered_up_scenario <- results_summary %>%
#   filter(!(pokemon_id == "Zekrom" & level == 25)) %>%
#   bind_rows(powered_up_summary) %>%
#     group_by(raid_boss, boss_fast_move_id, boss_charged_move_id, weather) %>%
#   mutate(dmg_rank = rank(desc(damage), ties.method = "min")) %>%
#   filter(dmg_rank <= 6) %>%
#   summarise(damage = sum(damage),
#             time = sum(time)) %>%
#   mutate(dps = damage/time) %>%
#   group_by(raid_boss) %>%
#   summarise(min_dps = min(dps),
#             max_dps = max(dps),
#             mean_dps = mean(dps))

# boss_summary %>%
#   arrange(mean_dps) %>%                       # order by mean
#   mutate(raid_boss = factor(raid_boss, levels = raid_boss)) %>%
#   ggplot(aes(y = raid_boss)) +
  
#   # Range stick (min to max)
#   geom_segment(aes(x = min_dps, xend = max_dps,
#                    yend = raid_boss),
#                linewidth = 1.2,
#                colour = "grey60") +
  
#   # Mean point (the lollipop head)
#   geom_point(aes(x = mean_dps),
#              size = 4,
#              colour = "#2C7FB8") +
  
#   labs(
#     x = "DPS",
#     y = NULL,
#     title = "Min, Mean, and Max DPS per Raid Boss"
#   ) +
#   theme_minimal(base_size = 13)

# bind_rows(
#   boss_summary %>% mutate(Version = "Current"),
#   powered_up_scenario %>% mutate(Version = "After Power Up")) %>%
#   group_by(raid_boss) %>%
#   mutate(order_mean = mean_dps[Version == "Current"]) %>%
#   ungroup() %>%
#   arrange(order_mean) %>%
#   # arrange(mean_dps) %>%                       # order by mean
#   mutate(raid_boss = factor(raid_boss, levels = unique(raid_boss))) %>%
#  ggplot(aes(y = raid_boss, colour = Version)) +
  
#   # Min → Max range
#   geom_segment(
#     aes(x = min_dps,
#         xend = max_dps,
#         yend = raid_boss),
#     position = position_dodge(width = 0.6),
#     linewidth = 1.2
#   ) +
  
#   # Mean point
#   geom_point(
#     aes(x = mean_dps),
#     position = position_dodge(width = 0.6),
#     size = 4
#   ) +
  
#   labs(
#     title = "DPS Range by Raid Boss",
#     subtitle = "Current vs After Power Up",
#     x = "DPS",
#     y = NULL,
#     colour = NULL
#   ) +
  
#   theme_minimal(base_size = 13) +
#   theme(
#     legend.position = "top"
#   )

# bind_rows(
#   boss_summary %>% mutate(Version = "Current"),
#   powered_up_scenario %>% mutate(Version = "After Power Up")) %>%
#   select(raid_boss, Version, mean_dps) %>%
#   tidyr::pivot_wider(names_from = Version, values_from = mean_dps) %>%
#   mutate(delta = `After Power Up` - Current) %>%
#   arrange(desc(delta))


# results_summary %>%
#   group_by(raid_boss, boss_fast_move_id, boss_charged_move_id, weather) %>%
#   mutate(dmg_rank = rank(desc(damage), ties.method = "random")) %>%
#   filter(dmg_rank <= 6) %>%
#   filter()


      results_summary %>%
        inner_join(upcoming_raids) %>%
        group_by(raid_boss, boss_fast_move_id, boss_charged_move_id, weather) %>%
        mutate(dmg_rank = rank(desc(damage), ties.method = "min")) %>%
        filter(dmg_rank <= 6) %>%
        summarise(damage = sum(damage),
                  time = sum(time)) %>%
        mutate(dps = damage/time) %>%
        group_by(raid_boss) %>%
        mutate(avg_dps = mean(dps)) %>%
        ungroup() %>%
        arrange(desc(avg_dps)) %>%
        ggplot(aes(y = reorder(raid_boss, avg_dps), x = dps)) +
        geom_violin()

kyorge <-  results_summary %>%
  filter(raid_boss == "Primal Kyogre")
