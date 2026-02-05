library(tidyverse)
library(googlesheets4)
library(shiny)

base_stats <- readRDS("data/base_stats.rds")
levels <- readRDS("data/levels.rds")

Defence_Formula <- function(Base_Defence, Defence_IV, CP_Multiplier) {
  return(floor((Base_Defence+Defence_IV) * CP_Multiplier))
}

Attack_Formula <- function(Base_Attack, Attack_IV, CP_Multiplier) {
  return(floor((Base_Attack+Attack_IV) * CP_Multiplier))
}

# ---- Formulas ----
CP_Formula <- function(
  Attack, Attack_IV,
  Defence, Defence_IV,
  HP, HP_IV,
  CP_Multiplier, Nerf
) {
  floor(
    (((Attack + Attack_IV) *
      sqrt(Defence + Defence_IV) *
      sqrt(HP + HP_IV) *
      CP_Multiplier^2) / 10) * Nerf
  )
}

HP_Formula <- function(Base_HP, HP_IV, CP_Multiplier) {
  floor((Base_HP + HP_IV) * CP_Multiplier)
}

# ---- Core logic ----
CP_Finder <- function(
  pokemon,
  target_CP,
  status = NULL,
  target_dust = NULL,
  target_visible_hp = NULL,
  target_HP_IV = NULL,
  target_attack_IV = NULL,
  target_defence_IV = NULL
) {

  base_stats_internal <- base_stats %>%
    filter(name == pokemon) %>%
    select(
      name,
      Nerf,
      HP = `HP Go`,
      Attack = `Attack Go`,
      Defence = `Defence Go`
    )

  req(nrow(base_stats_internal) == 1)

  levels_internal <- levels %>%
    mutate(
      Dust = case_when(
       status == "Normal" ~ `Marginal Dust`,
        status == "Lucky" ~ `Marginal Dust Lucky`,
        status == "Shadow" ~ `Marginal Dust Shadow`,
        status == "Purified" ~ `Marginal Dust Purified`,
        TRUE ~ `Marginal Dust`
      )
      ) %>%
    select(Level, `CP Multiplier`, Dust) %>%
    mutate(join = 1)

  df_out <- levels_internal %>%
    left_join(tibble(join = 1, Attack_IV = 0:15)) %>%
    left_join(tibble(join = 1, Def_IV = 0:15)) %>%
    left_join(tibble(join = 1, HP_IV = 0:15)) %>%
    left_join(base_stats_internal %>% mutate(join = 1)) %>%
    select(-join) %>%
    mutate(
      CP = CP_Formula(
        Attack,
        Attack_IV,
        Defence,
        Def_IV,
        HP,
        HP_IV,
        `CP Multiplier`,
        Nerf
      ),
      Visible_HP = HP_Formula(
        Base_HP = HP,
        HP_IV = HP_IV,
        CP_Multiplier = `CP Multiplier`
      )
    ) %>%
    filter(CP == target_CP)

  if (!is.null(target_visible_hp)) {
    df_out <- df_out %>% filter(Visible_HP == target_visible_hp)
  }
  if (!is.null(target_HP_IV)) {
    df_out <- df_out %>% filter(HP_IV == target_HP_IV)
  }
  if (!is.null(target_attack_IV)) {
    df_out <- df_out %>% filter(Attack_IV == target_attack_IV)
  }
  if (!is.null(target_defence_IV)) {
    df_out <- df_out %>% filter(Def_IV == target_defence_IV)
  }

  if (!is.null(target_dust)) {
    df_out <- df_out %>% filter(Dust == target_dust)
  }
  df_out %>%
    select(name, CP, Level, Dust, HP = Visible_HP, Attack_IV, Def_IV, HP_IV)
}

# ---- UI ----
ui <- fluidPage(
  titlePanel("Pokémon GO CP / IV Finder"),

  sidebarLayout(
    sidebarPanel(
      selectInput(
        "pokemon",
        "Pokémon",
        choices = sort(unique(base_stats$name))
      ),

      numericInput("cp", "Target CP", value = 1500, min = 10, max = 10000),

      uiOutput("dust_ui"),

      selectInput(
        "status",
        "Status",
        choices = c("Normal", "Lucky", "Shadow", "Purified"),
        selected = "Normal"
      ),

      numericInput("visible_hp", "Visible HP (optional)", value = NA),
      numericInput("atk_iv", "Attack IV (optional)", value = NA, min = 0, max = 15),
      numericInput("def_iv", "Defence IV (optional)", value = NA, min = 0, max = 15),
      numericInput("hp_iv", "HP IV (optional)", value = NA, min = 0, max = 15),

      actionButton("run", "Find IVs", class = "btn-primary")
    ),

    mainPanel(
      h4("Possible matches"),
      tableOutput("results"),
      textOutput("result_count")
    )
  )
)

# ---- Server ----
server <- function(input, output) {

  results <- eventReactive(input$run, {

    CP_Finder(
      pokemon = input$pokemon,
      target_CP = input$cp,
      status = input$status, #if (input$status == "Normal") NULL else input$status,
      target_dust = if (is.na(input$dust)) NULL else input$dust,
      target_visible_hp = if (is.na(input$visible_hp)) NULL else input$visible_hp,
      target_attack_IV = if (is.na(input$atk_iv)) NULL else input$atk_iv,
      target_defence_IV = if (is.na(input$def_iv)) NULL else input$def_iv,
      target_HP_IV = if (is.na(input$hp_iv)) NULL else input$hp_iv
    )

  })

  output$results <- renderTable({
    results()
  })

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
}

shinyApp(ui, server)