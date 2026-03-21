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

# hypotechical_matchups <- readRDS("data/hypotectical_matchups.RDS")

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

# hypo_available_levels <- hypotechical_matchups %>%
#   select(level) %>%
#   distinct() %>%
#   arrange(level) %>%
#   pull


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
    fluidRow(
      box(
        width = 12,
        div(
          style = "display:flex; gap:12px; align-items:flex-end; flex-wrap:wrap;",
          
          div(style = "width:220px;",
              selectInput(
                "raid_boss",
                "Raid Boss",
                choices = upcoming_raids$raid_boss,
                selectize = FALSE
              )
          ),
          
          div(style = "width:160px;",
              selectInput(
                "raid_tier",
                "Tier",
                choices = 4:6,
                selectize = FALSE
              )
          ),
          
          div(style = "width:200px;",
              selectInput(
                "battle_party_size",
                "Party Size",
                choices = c(6, 12),
                selectize = FALSE
              )
          )
        )
      )
    ),
    fluidRow(box(width = 6, title = "Your Best Counters", tableOutput("user_pokemon")),
            box(width = 6, title = "Your Best Mega-Evolved Counters",tableOutput("user_mega_pokemon"))),
    fluidRow(box(width = 6, title = "Best Counters", 
              div(
                style = "margin-bottom:10px;",
             radioButtons(
                "counter_level",
                "What level are the counters? ",
                choices = c(30, 40, 50),
                selected = 30,
                inline = TRUE
            )),    
            tableOutput("best_counters")),
            box(witdh = 6, title = "Best Mega-Evolved Counters", tableOutput("best_mega_counters"))),

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
    
    get_hypo_matchups(input$raid_boss, as.numeric(input$raid_tier)) %>%
      filter(level == input$counter_level,
      !grepl("Mega |Primal", pokemon_name)) %>%
      group_by(pokemon_name, raid_boss, level, fast_move_id, charged_move_id) %>%
      summarise(damage = sum(damage), time = sum(time)) %>%
      mutate(dps = damage / time) %>%
      ungroup %>%
      arrange(desc(dps)) %>%
      top_n(n = as.numeric(input$battle_party_size), wt = dps)
  })

    output$best_mega_counters <- renderTable({
    req(input$raid_boss)
    # browser()

    get_hypo_matchups(input$raid_boss, as.numeric(input$raid_tier)) %>%
      filter(level == input$counter_level,
          grepl("Mega |Primal", pokemon_name)) %>%
      group_by(pokemon_name, raid_boss, level, fast_move_id, charged_move_id) %>%
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
