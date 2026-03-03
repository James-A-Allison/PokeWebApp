library(tidyverse)
library(shinydashboard)
library(shiny)
library(pokemonGoSim)


results_summary <- readRDS("data/results_summary.RDS")
powered_up_summary <- readRDS("data/powered_up_summary.RDS")
upcoming_raids <- readRDS("data/calendar.RDS") %>%
  filter(To >= Sys.Date()) %>%
  select(raid_boss = `Raid Boss`, tier = Tier)
hypotechical_matchups <- readRDS("data/hypotectical_matchups.RDS")

raid_boss_options <- c(
  upcoming_raids %>% select(raid_boss) %>% distinct %>% pull,
  "--",
  results_summary %>%
    select(raid_boss) %>%
    distinct %>%
    arrange(raid_boss) %>%
    pull
)

raid_boss_tier <- c(
  results_summary %>% select(tier) %>% distinct %>% arrange(tier) %>% pull
)

hypo_available_levels <- hypotechical_matchups %>%
  select(level) %>%
  distinct() %>%
  arrange(level) %>%
  pull


ui <- dashboardPage(
  dashboardHeader(),
  dashboardSidebar(),
  dashboardBody(
    selectInput("raid_boss", "Select a raid boss: ", choices = NULL),
    selectInput("raid_tier", "Select a raid boss: ", raid_boss_tier),
    tableOutput("user_pokemon"),
    tableOutput("user_mega_pokemon"),
    tableOutput("best_counters"),
    tableOutput("best_mega_counters"),
    radioButtons(
      "counter_level",
      "What level are the counters? ",
      choices = hypo_available_levels,
      selected = min(hypo_available_levels)
    )
  )
)

server <- function(input, output, session) {
  data <- reactiveValues(
    raid_boss_list = bind_rows(
      upcoming_raids %>%
        arrange(desc(tier), raid_boss) %>%
        distinct() %>%
        mutate(tier = as.character(tier)),
      tibble(tier = as.character(1:6), raid_boss = "--"),
      results_summary %>%
        select(tier, raid_boss) %>%
        distinct() %>%
        arrange(desc(tier), raid_boss) %>%
        mutate(tier = as.character(tier))
    )
  )

  observeEvent(input$raid_tier, {
    req(input$raid_tier, data$raid_boss_list)
    # browser()
    boss_tiers <- data$raid_boss_list %>%
      filter(tier == input$raid_tier)

    updateSelectInput(
      session,
      "raid_boss",
      choices = boss_tiers$raid_boss
    )
  })

  output$best_counters <- renderTable({
    hypotechical_matchups %>%
      filter(raid_boss == input$raid_boss, 
        level == input$counter_level,
      !grepl("Mega |Primal", pokemon_id)) %>%
      group_by(pokemon_id, raid_boss, level, fast_move_id, charged_move_id) %>%
      summarise(damage = sum(damage), time = sum(time)) %>%
      mutate(dps = damage / time) %>%
      ungroup %>%
      arrange(desc(dps)) %>%
      top_n(n = 6, wt = dps)
  })

    output$best_mega_counters <- renderTable({
    hypotechical_matchups %>%
      filter(raid_boss == input$raid_boss,
         level == input$counter_level,
          grepl("Mega |Primal", pokemon_id)) %>%
      group_by(pokemon_id, raid_boss, level, fast_move_id, charged_move_id) %>%
      summarise(damage = sum(damage), time = sum(time)) %>%
      mutate(dps = damage / time) %>%
      ungroup %>%
      arrange(desc(dps)) %>%
      top_n(n = 6, wt = dps)
  })

  output$user_pokemon <- renderTable({
    results_summary %>%
      filter(raid_boss == input$raid_boss,
             !grepl("Mega |Primal", pokemon_id)) %>%
      group_by(pokemon_id, raid_boss, level, fast_move_id, charged_move_id) %>%
      summarise(damage = sum(damage), time = sum(time)) %>%
      mutate(dps = damage / time) %>%
      ungroup %>%
      arrange(desc(dps)) %>%
      top_n(n = 6, wt = dps)
  })

    output$user_mega_pokemon <- renderTable({
    results_summary %>%
      filter(raid_boss == input$raid_boss,
            grepl("Mega |Primal", pokemon_id)) %>%
      group_by(pokemon_id, raid_boss, level, fast_move_id, charged_move_id) %>%
      summarise(damage = sum(damage), time = sum(time)) %>%
      mutate(dps = damage / time) %>%
      ungroup %>%
      arrange(desc(dps)) %>%
      top_n(n = 6, wt = dps)
  })
}

shinyApp(ui, server)

results_summary %>%
  filter(raid_boss %in% upcoming_raids$raid_boss,
          !grepl("Mega |Primal", pokemon_id),
        tier == 5) %>%
  group_by(pokemon_id, raid_boss, level, fast_move_id, charged_move_id) %>%
  summarise(damage = sum(damage), time = sum(time)) %>%
  mutate(dps = damage / time) %>%
  group_by(raid_boss) %>%
  top_n(n = 6, wt = dps) %>%
  summarise(damage = sum(damage),
        time = sum(time)) %>%
  mutate(dps = damage / time) %>%
  arrange(desc(dps))

moves_formatted <- readRDS("data/moves_formatted.RDS")

results_summary %>%
  left_join(moves_formatted %>% 
    filter(category  == "fast_move") %>%
    select(fast_move_id = name, fast_type = type)) %>%
  left_join(moves_formatted %>% 
    filter(category  == "charge_move") %>%
    select(charged_move_id = name, charged_type = type))
