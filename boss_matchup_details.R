library(tidyverse)
library(shinydashboard)
library(shiny)
library(pokemonGoSim)

files <- list.files("R", pattern = "\\.R$", full.names = TRUE)

lapply(files, source)

users <- get_users()

results_summary <- readRDS("data/results_summary.RDS")
powered_up_summary <- readRDS("data/powered_up_summary.RDS")

upcoming_raids <- get_calendar() %>%
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

# raid_boss_tier <- c(
#   results_summary %>% select(tier) %>% distinct %>% arrange(tier) %>% pull
# )

hypo_available_levels <- hypotechical_matchups %>%
  select(level) %>%
  distinct() %>%
  arrange(level) %>%
  pull


ui <- dashboardPage(
      dashboardHeader(title = "Raid Boss Matchup Details",
      tags$li(
        class = "dropdown",
        tags$a(
          href = "#",
          class = "dropdown-toggle",
          `data-toggle` = "dropdown",
          tags$img(src = "avatar.png", class = "user-image", height = "25px"),
          span("James", class = "hidden-xs")
        ),
        tags$ul(
          class = "dropdown-menu",
          tags$li(
          style = "padding:10px;",
            selectInput(
              "active_user",
              NULL,
              choices = setNames(users$user_id, users$username)
            )
          )
        )
      )
  ),
  dashboardSidebar(),
  dashboardBody(
    selectInput("raid_boss", "Select a raid boss: ", choices = upcoming_raids$raid_boss),
    selectInput("raid_tier", "Select a raid tier: ", choices = c(4:6)),
    selectInput("battle_party_size", "Top 6 or 12 Pokemon: ", choices = c(6,12)),
    tableOutput("user_pokemon"),
    tableOutput("user_mega_pokemon"),
    tableOutput("best_counters"),
    tableOutput("best_mega_counters"),
    radioButtons(
      "counter_level",
      "What level are the counters? ",
      choices = hypo_available_levels,
      selected = min(hypo_available_levels)
    ),
    tags$script(HTML("
          $(document).on('click', '.dropdown-menu', function (e) {
          e.stopPropagation();
          });
      "))
  )
)

server <- function(input, output, session) {

  con <- get_con()

  session$onSessionEnded(function() {
    DBI::dbDisconnect(con, shutdown = FALSE)
  })

  users <- get_users()

  active_user <- reactiveVal(NULL)

  updateSelectInput(
    session,
    "active_user",
    choices = setNames(users$user_id, users$username)
  )

  observeEvent(input$active_user, {
    active_user(input$active_user)
  })

  user_battle_results <- reactive({
    # browser()
    # refresh_user_pokemon() 
    user_id <- req(active_user())

  get_user_battle_results(user_id)
})

  raid_boss_list <- reactive({

    req(nrow(user_battle_results()) > 0)

    # browser()
    bind_rows(
      upcoming_raids %>%
        arrange(desc(tier), raid_boss) %>%
        distinct() %>%
        mutate(tier = as.character(tier)),
      tibble(tier = as.character(1:6), raid_boss = "--"),
      user_battle_results() %>%
        select(tier = raid_tier, raid_boss) %>%
        distinct() %>%
        arrange(desc(tier), raid_boss) %>%
        mutate(tier = as.character(tier))
    )
})

  observeEvent(input$raid_tier, {
    req(input$raid_tier, raid_boss_list())
    # browser()
    boss_tiers <- raid_boss_list() %>%
      filter(tier == input$raid_tier)

    updateSelectInput(
      session,
      "raid_boss",
      choices = boss_tiers$raid_boss
    )
  })

  output$best_counters <- renderTable({
    req(input$raid_boss)
    # browser()
    
    hypotechical_matchups %>%
      filter(raid_boss == input$raid_boss, 
        level == input$counter_level,
      !grepl("Mega |Primal", pokemon_id)) %>%
      group_by(pokemon_id, raid_boss, level, fast_move_id, charged_move_id) %>%
      summarise(damage = sum(damage), time = sum(time)) %>%
      mutate(dps = damage / time) %>%
      ungroup %>%
      arrange(desc(dps)) %>%
      top_n(n = as.numeric(input$battle_party_size), wt = dps)
  })

    output$best_mega_counters <- renderTable({
    req(input$raid_boss)
    # browser()

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
    req(input$raid_boss)
    # browser()

    user_battle_results() %>%
      filter(raid_boss == input$raid_boss,
             !grepl("Mega |Primal", pokemon_name)) %>%
      group_by(pokemon_name, pokemon_instance_id, level, fast_move_id, charged_move_id) %>%
      summarise(damage = sum(damage), time = sum(time)) %>%
      mutate(dps = damage / time) %>%
      group_by(pokemon_instance_id) %>%
      top_n(n = 1, wt = dps) %>%
      ungroup() %>%
      select(-pokemon_instance_id) %>%
      arrange(desc(dps)) %>%
      top_n(n = as.numeric(input$battle_party_size), wt = dps)
  })

  output$user_mega_pokemon <- renderTable({
    req(input$raid_boss)
    # browser()

    user_battle_results() %>%
      filter(raid_boss == input$raid_boss,
            grepl("Mega |Primal", pokemon_name)) %>%
      group_by(pokemon_name, raid_boss, level, fast_move_id, charged_move_id) %>%
      summarise(damage = sum(damage), time = sum(time)) %>%
      mutate(dps = damage / time) %>%
      ungroup %>%
      arrange(desc(dps)) %>%
      top_n(n = 6, wt = dps)
  })
}

shinyApp(ui, server)

# results_summary %>%
#   filter(raid_boss %in% upcoming_raids$raid_boss,
#           !grepl("Mega |Primal", pokemon_id),
#         tier == 5) %>%
#   group_by(pokemon_id, raid_boss, level, fast_move_id, charged_move_id) %>%
#   summarise(damage = sum(damage), time = sum(time)) %>%
#   mutate(dps = damage / time) %>%
#   group_by(raid_boss) %>%
#   top_n(n = 6, wt = dps) %>%
#   summarise(damage = sum(damage),
#         time = sum(time)) %>%
#   mutate(dps = damage / time) %>%
#   arrange(desc(dps))

# moves_formatted <- readRDS("data/moves_formatted.RDS")

# results_summary %>%
#   left_join(moves_formatted %>% 
#     filter(category  == "fast_move") %>%
#     select(fast_move_id = name, fast_type = type)) %>%
#   left_join(moves_formatted %>% 
#     filter(category  == "charge_move") %>%
#     select(charged_move_id = name, charged_type = type))
