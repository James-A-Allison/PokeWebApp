# devtools::install("C:/Users/james/pokemonGoSim")

library(tidyverse)
library(googlesheets4)
library(shiny)
library(pokemonGoSim)
library(DT)
library(uuid)
library(shinydashboard)

files <- list.files("R", pattern = "\\.R$", full.names = TRUE)

lapply(files, source)

# user_id <- "0a08ee46-2663-4155-a535-22a93cd5a821"

type_colours <- c(
  Normal   = "#A8A77A",
  Fire     = "#EE8130",
  Water    = "#6390F0",
  Electric = "#F7D02C",
  Grass    = "#7AC74C",
  Ice      = "#96D9D6",
  Fighting = "#C22E28",
  Poison   = "#A33EA1",
  Ground   = "#E2BF65",
  Flying   = "#A98FF3",
  Psychic  = "#F95587",
  Bug      = "#A6B91A",
  Rock     = "#B6A136",
  Ghost    = "#735797",
  Dragon   = "#6F35FC",
  Dark     = "#705746",
  Steel    = "#B7B7CE",
  Fairy    = "#D685AD"
)

pokemon_id <- get_pokemon_id()

base_stats <- get_base_stats() 

poke_types <- base_stats %>% select(`Type 1`) %>% distinct() %>% pull()

mega_table <- base_stats %>%
  left_join(get_mega_table() %>%
            mutate(can_mega_evolve = "Yes") %>%
    select(name = base_name, can_mega_evolve)) %>%
  mutate(can_mega_evolve = if_else(is.na(can_mega_evolve), "No", can_mega_evolve))

moves <- get_moves()
levels <- get_levels()
user_move_combinations <- get_user_move_combinations()
# user_pokemon <- readRDS("data/user_pokemon.RDS")

moves_formatted <- get_moves_formatted()
dust_status <- get_dust_status()

# user_pokemon <- user_pokemon %>%
#   # filter(Charge2 == "") %>%
#   mutate(Charge2 = if_else(Charge2 == "", NA_character_, Charge2))

ui <- dashboardPage(
    dashboardHeader(title = "Roster Builder",
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
              choices = c("Account A", "Account B", "Account C")
            )
          )
        )
      )
  ),

  dashboardSidebar(
    # sidebarPanel(
      selectInput(
        "pokemon",
        "Pokémon",
        choices = sort(unique(base_stats$name))
      ),
      selectInput("can_mega", "Can Mega evolve?", choices = NULL),
      selectInput("can_dyanamax", "Can battle in Max Raids?", choices = NULL, selected = "No"),
       numericInput("cp", "Target CP", value = 1500, min = 10, max = 10000),


      selectInput(
        "status",
        "Status",
        choices = c("Normal", "Lucky", "Shadow", "Purified"),
        selected = "Normal"
      ),

      selectInput("fast_move", "Fast Move", choices = NULL),

      selectInput("charge_move_1", "Charge Move 1", choices = NULL),

      selectInput("charge_move_2", "Charge Move 2", choices = NULL),


      uiOutput("dust_ui"),

      numericInput("visible_hp", "Visible HP (optional)", value = NA),
      numericInput("atk_iv", "Attack IV (optional)", value = NA, min = 0, max = 15),
      numericInput("def_iv", "Defence IV (optional)", value = NA, min = 0, max = 15),
      numericInput("hp_iv", "HP IV (optional)", value = NA, min = 0, max = 15),

      actionButton("run", "Find IVs", class = "btn-primary")

      # h4("Possible matches"),


    ),

    dashboardBody(
      dataTableOutput("iv_results"),
      actionButton("add_new", "Add Pokemon", class = "btn-primary"),
      selectInput("filter_pokemon", "Pokemon",
            choices = c("All", sort(unique(user_move_combinations$Pokemon)))),
      dataTableOutput("user_pokemon"),
      actionButton("remove_pokemon", "Remove Pokemon", class = "btn-primary"),
      plotOutput("level_dotplot"),
      dataTableOutput("attacker_type_summaries"),
      dataTableOutput("pokemon_summaries"),
      tags$script(HTML("
          $(document).on('click', '.dropdown-menu', function (e) {
          e.stopPropagation();
          });
        "))
    )
  )


server <- function(input, output, session) {
  refresh_user_pokemon <- reactiveVal(0)
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

  user_table <- reactive({
    # browser()
  refresh_user_pokemon() 
   user_id <- req(active_user())

  get_user_pokemon_enriched(user_id) %>%
    select(
      pokemon_instance_id,
      Pokemon,
      `Dust Status` = dust_status,
      `Can Mega Evolve` = can_mega_evolve,
      `Can Dynamax` = can_dynamax,
      `Fast Move` = fast_move,
      Charge1,
      Charge2,
      Level, 
      `Attack IV` = attack_iv, 
      `Defence IV` = defence_iv, 
      `HP IV` = hp_iv)
})
  
observeEvent(input$pokemon, {
  req(input$pokemon)
  # browser()
  pokemon_moves <- user_move_combinations %>%
    filter(Pokemon == input$pokemon)
  
  fast_move_options   <- unique(pokemon_moves$fast_move)
  charge_move_options <- unique(pokemon_moves$charge_move)
  
  updateSelectInput(session, "fast_move",
                    choices = fast_move_options)
  
  updateSelectInput(session, "charge_move_1",
                    choices = charge_move_options)
  
  updateSelectInput(session, "charge_move_2",
                    choices = c("", charge_move_options),
                  selected = "")
  
  poke_table <- base_stats %>%
    filter(name == input$pokemon) %>% 
    select(`Shadow Released`, `Dynamax Released`)

  shadow_options <- if(poke_table$`Shadow Released` == "Yes") {
     c("Normal", "Lucky", "Shadow", "Purified") } else {
     c("Normal", "Lucky")
  }

  dynamax_options <- if(poke_table$`Dynamax Released` == "Yes") {
     c("No", "Yes") } else {
     c("No")
  }

  mega_table_ui <- mega_table %>% filter(name == input$pokemon)

  mega_options <- if("Yes" %in% mega_table_ui$can_mega_evolve) {
     c("No", "Yes") } else {
     c("No")
  }

  updateSelectInput(session, "status",
                    choices = shadow_options,
                  selected = "Normal")
  
  updateSelectInput(session, "can_mega",
                    choices = mega_options,
                  selected = "No")
  
  updateSelectInput(session, "can_dyanamax",
                    choices = c(dynamax_options),
                  selected = "No")

})
  
  filtered_user_table <- reactive({
  df <- user_table()
  # browser()
  
  # Filter by Pokemon
  if (!is.null(input$filter_pokemon) && input$filter_pokemon != "All") {
    df <- df %>%
      filter(Pokemon == input$filter_pokemon)
  }

  
  df
})

  iv_results_df <- eventReactive(input$run, {

    CP_Finder(
      pokemon = input$pokemon,
      base_stats = base_stats,
      levels = levels,
      target_CP = input$cp,
      status = input$status, #if (input$status == "Normal") NULL else input$status,
      target_dust = if (is.na(input$dust)) NULL else input$dust,
      target_visible_hp = if (is.na(input$visible_hp)) NULL else input$visible_hp,
      target_attack_IV = if (is.na(input$atk_iv)) NULL else input$atk_iv,
      target_defence_IV = if (is.na(input$def_iv)) NULL else input$def_iv,
      target_HP_IV = if (is.na(input$hp_iv)) NULL else input$hp_iv
    )

  })

  output$iv_results <- renderDataTable({
    iv_results_df()}, selection = "single", rownames = FALSE
  )

  output$dust_ui <- renderUI({

  dust_choices <- switch(
    input$status,
    "Lucky"    = sort(unique(levels$`Marginal Dust Lucky`)),
    "Shadow"   = sort(unique(levels$`Marginal Dust Shadow`)),
    "Purified" = sort(unique(levels$`Marginal Dust Purified`)),
    sort(unique(levels$`Marginal Dust`))
  )

  selectInput(
    "dust",
    "Dust Cost",
    choices = dust_choices,
    selected = NULL
  )
})

  output$result_count <- renderText({
    df <- results()
    if (is.null(df) || nrow(df) == 0) {
      "No matching IV combinations found."
    } else {
      paste(nrow(df), "possible IV combinations")
    }
  })

  output$user_pokemon <- renderDataTable({
    # browser()
      filtered_user_table() %>%
      inner_join(levels %>% select(Level, `CP Multiplier`)) %>%
      rowwise() %>%
      mutate(CP = CP_Formula(pokemon = Pokemon, 
        base_stats = base_stats,
        levels = levels,
        level = Level,
        CP_Multiplier = `CP Multiplier`,
        Attack_IV = `Attack IV`,
        Defence_IV = `Defence IV`,
        HP_IV = `HP IV`),
      .after = `Pokemon`) %>%
        select(-c(pokemon_instance_id, `CP Multiplier`))
  },selection = "single", rownames = FALSE)

  observeEvent(input$add_new, {
    req(input$iv_results_rows_selected)
    user_id <- req(active_user())
    # browser()
    selected_index <- input$iv_results_rows_selected

    # new_id <- user_table() %>% select(ID) %>% pull() %>% max() + 1

    new_row <- tibble(
      user_id = user_id,
      pokemon_instance_id = UUIDgenerate(),
      pokemon_id = pokemon_id$pokemon_id[pokemon_id$name == input$pokemon],
      nickname = NA_character_,
      can_mega_evolve = if(input$can_mega == "Yes") {TRUE} else {FALSE},
      can_dynamax = if(input$can_dyanamax == "Yes") {TRUE} else {FALSE},
      fast_move_id = moves$move_id[moves$name == input$fast_move],
      charge_move_1_id = moves$move_id[moves$name == input$charge_move_1],
      charge_move_2_id = if(input$charge_move_2 == "") {NA_character_} else {moves$move_id[moves$name == input$charge_move_2]},
      dust_id = dust_status$dust_id[dust_status$dust_status == input$status],

        # Pokemon = input$pokemon,
        # `Dust Status` = input$status,
        # `Can Mega Evolve` = input$can_mega,
        # `Can Dynamax` = input$can_dyanamax,
        # `Fast Move` = input$fast_move,
        # Charge1 = input$charge_move_1,
        # Charge2 = if(input$charge_move_2 == "") {NA_character_} else {input$charge_move_2},
        level = iv_results_df()$Level[selected_index],
        attack_iv = iv_results_df()$Attack_IV[selected_index],
        defence_iv = iv_results_df()$Def_IV[selected_index],
        hp_iv = iv_results_df()$HP_IV[selected_index]
      )

    # updated <- bind_rows(
    #   user_table(),
    #   new_row)
    
    add_user_pokemon(new_row)
    
    # updated <- get_user_pokemon_enriched(user_id) %>%
    #   select(
    #     pokemon_instance_id,
    #     Pokemon,
    #     `Dust Status` = dust_status,
    #     `Can Mega Evolve` = can_mega_evolve,
    #     `Can Dynamax` = can_dynamax,
    #     `Fast Move` = fast_move,
    #     Charge1,
    #     Charge2,
    #     Level, 
    #     `Attack IV` = attack_iv, 
    #     `Defence IV` = defence_iv, 
    #     `HP IV` = hp_iv)

    # print(new_row)
    # saveRDS(updated, "data/user_pokemon.RDS")
    # user_table(updated)

    refresh_user_pokemon(refresh_user_pokemon() + 1)
  })

    observeEvent(input$remove_pokemon, {
      user_id <- req(active_user())
    req(input$user_pokemon_rows_selected)
    # browser()
    # current <- data$pokemon_moves %>%
    #   filter(pokemon_id == input$pokemon)
    uuid_to_remove <- filtered_user_table() %>%
      slice(input$user_pokemon_rows_selected) %>%
      select(pokemon_instance_id) %>%
      pull()
    # updated <- user_table() %>%
    #   filter(pokemon_instance_id != uuid_to_remove)
      
    remove_user_pokemon(user_id, uuid_to_remove)
    
    # saveRDS(updated, "data/user_pokemon.RDS")
      
    # updated <- get_user_pokemon_enriched(user_id) %>%
    #   select(
    #         pokemon_instance_id,
    #         Pokemon,
    #         `Dust Status` = dust_status,
    #         `Can Mega Evolve` = can_mega_evolve,
    #         `Can Dynamax` = can_dynamax,
    #         `Fast Move` = fast_move,
    #         Charge1,
    #         Charge2,
    #         Level, 
    #         `Attack IV` = attack_iv, 
    #         `Defence IV` = defence_iv, 
    #         `HP IV` = hp_iv)
      
    # user_table(updated)
    refresh_user_pokemon(refresh_user_pokemon() + 1)
    })
  
  # output$level_dotplot <- renderPlot({
  #   user_table() %>%
  #     left_join(base_stats %>% select(Pokemon = name, type_1 = `Type 1`, type_2 = `Type 2`)) %>%
  #     # mutate(Type = if_else(type_1 == type_2, type_1, paste(type_1, type_2, sep = " & "))) %>%
  #     select(Pokemon, Level, type_1, type_2, uuid) %>%
  #     distinct() %>%
  #     ggplot(aes(x = Level, fill = type_1, color = type_2)) +
  #     geom_dotplot(stackgroups = TRUE, stackdir = "up") +
  #     scale_fill_manual(values = type_colours) +
  #     scale_color_manual(values = type_colours)
  # })
  
  # output$pokemon_summaries <- renderDataTable({
  # user_table() %>%
  #     inner_join(levels %>% select(Level, `CP Multiplier`)) %>%
  # left_join(moves_formatted %>% 
  #   filter(category  == "fast_move") %>%
  #   select(`Fast Move` = name, fast_type = type)) %>%
  # left_join(moves_formatted %>% 
  #   filter(category  == "charge_move") %>%
  #   select(Charge1 = name, charged_type1 = type)) %>%
  # left_join(moves_formatted %>% 
  #   filter(category  == "charge_move") %>%
  #   select(Charge2 = name, charged_type2 = type)) %>%
  # rowwise() %>%
  # mutate(CP = CP_Formula(pokemon = Pokemon, 
  #       base_stats = base_stats,
  #       levels = levels,
  #       level = Level,
  #       CP_Multiplier = `CP Multiplier`,
  #       Attack_IV = `Attack IV`,
  #       Defence_IV = `Defence IV`,
  #       HP_IV = `HP IV`)) %>%
  #   group_by(Pokemon) %>% 
  #     summarise(n = n_distinct(uuid),
  #             level = mean(Level),
  #             CP = mean(CP))

  # })


 output$attacker_type_summaries <- renderDataTable({
   attacker_type_summaries <- tibble()

   input_table <- user_table() %>%
     inner_join(levels %>% select(Level, `CP Multiplier`)) %>%
  left_join(moves_formatted %>% 
    filter(category  == "fast_move") %>%
    select(`Fast Move` = name, fast_type = type)) %>%
  left_join(moves_formatted %>% 
    filter(category  == "charge_move") %>%
    select(Charge1 = name, charged_type1 = type)) %>%
  left_join(moves_formatted %>% 
    filter(category  == "charge_move") %>%
    select(Charge2 = name, charged_type2 = type)) %>%
  rowwise() %>%
  mutate(CP = CP_Formula(pokemon = Pokemon, 
        base_stats = base_stats,
        levels = levels,
        level = Level,
        CP_Multiplier = `CP Multiplier`,
        Attack_IV = `Attack IV`,
        Defence_IV = `Defence IV`,
        HP_IV = `HP IV`)) %>%
  ungroup()

for (i in seq(poke_types)) {
  poke_type <- poke_types[i]

  attacker_type_summaries <- input_table %>%
    filter(fast_type == poke_type |
          charged_type1 == poke_type |
          charged_type2 == poke_type ) %>%
    summarise(n = n_distinct(pokemon_instance_id),
            level = mean(Level),
            CP = mean(CP)) %>%
              mutate(attacker_type = poke_type) %>%
              bind_rows(attacker_type_summaries)
}
   attacker_type_summaries %>%
    select(attacker_type, n, level, CP)
   
 }, rownames = FALSE)
}

shinyApp(ui, server)