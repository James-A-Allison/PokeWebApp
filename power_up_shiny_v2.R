library(tidyverse)
library(shinydashboard)
library(shiny)
library(doFuture)
library(future)
library(progressr)
library(furrr)
library(pokemonGoSim)
library(DT)
library(shinycssloaders)

handlers(global = TRUE)
handlers("shiny")

registerDoFuture()
plan(multisession, workers = 10)  # or however many

pokemon_moves <- readRDS("data/pokemon_moves.rds")
moves <- readRDS("data/moves.rds")
# weather_boosts <- readRDS("data/weather.rds")

pokemon_ids <- readRDS("data/pokemon_ids.rds")
move_ids <- readRDS("data/move_ids.rds")

results_summary <- readRDS("data/results_summary.RDS")
upcoming_raids <- readRDS("data/calendar.RDS") %>%
  filter(To >= Sys.Date()) %>%
  select(raid_boss = `Raid Boss`, tier = Tier)

raid_boss_tier <- c(
  results_summary %>% select(tier) %>% distinct %>% arrange(tier) %>% pull
)

dust_table <-readRDS("data/levels.RDS") %>%
  select(level = Level,
        normal = `Marginal Dust`,
        eternatus = `Marginal Dust`,
        lucky = `Marginal Dust Lucky`,
        shadow = `Marginal Dust Shadow`,
        purified_dust = `Marginal Dust Purified`
      )

base_stats <- readRDS("data/base_stats.rds") %>%
  select(
    pokemon_id = name,
    name,
    form = name,
    base_atk = `Attack Go`,
    base_def = `Defence Go`,
    base_sta = `HP Go`,
    type1 = `Type 1`,
    type2 = `Type 2`
  )

raid_bosses <- readRDS("data/base_stats.rds") %>%
  # filter(`Raid Boss Tier` > 3) %>%
  filter(`Raid Boss Tier` > 3) %>%
  select(Pokemon = name, tier = `Raid Boss Tier`) %>%
  distinct() %>%
  left_join(raid_boss_tiers)

boss_move_combinations <- readRDS("data/boss_move_combinations.RDS")

moves <- readRDS("data/moves_formatted.RDS")

user_pokemon <- readRDS("data/user_pokemon.rds") %>%
  mutate(shadow = if_else(`Dust Status` == "Shadow", TRUE, FALSE)) %>%
  mutate(`Attack IV` = if_else(is.na(`Attack IV`), 0, `Attack IV`)) %>%
  mutate(`Defence IV` = if_else(is.na(`Defence IV`), 0, `Defence IV`)) %>%
  mutate(`HP IV` = if_else(is.na(`HP IV`), 0, `HP IV`)) %>%
  select(uuid,
        pokemon_id = Pokemon,
        dust_status = `Dust Status`,
        level = `Level`,
        iv_atk = `Attack IV`,
        iv_def = `Defence IV`,
        iv_sta = `HP IV`,
        shadow,
        fast_move_id = `Fast Move`,
        charged_move_id = `Charge1`,
        Charge2)

user_pokemon <- readRDS("data/user_pokemon.rds") %>%
  filter(`Can Mega Evolve` == "Yes") %>%
  rename(base_name = Pokemon) %>%
  inner_join(mega_table) %>%
  select(-c(`pokedex number`, base_name)) %>%
  rename(Pokemon = Mega_name) %>%
  mutate(shadow = if_else(`Dust Status` == "Shadow", TRUE, FALSE)) %>%
  mutate(`Attack IV` = if_else(is.na(`Attack IV`), 0, `Attack IV`)) %>%
  mutate(`Defence IV` = if_else(is.na(`Defence IV`), 0, `Defence IV`)) %>%
  mutate(`HP IV` = if_else(is.na(`HP IV`), 0, `HP IV`)) %>%
  select(uuid,
        pokemon_id = Pokemon,
        dust_status = `Dust Status`,
        level = `Level`,
        iv_atk = `Attack IV`,
        iv_def = `Defence IV`,
        iv_sta = `HP IV`,
        shadow,
        fast_move_id = `Fast Move`,
        charged_move_id = `Charge1`,
      Charge2) %>%
  bind_rows(user_pokemon)

user_pokemon <- bind_rows(user_pokemon %>%
  filter(!is.na(Charge2)) %>%
  select(-charged_move_id) %>%
  rename(charged_move_id = Charge2),

  user_pokemon %>%
    # filter(is.na(Charge2)) %>%
    select(-Charge2))



max_level_with_dust_fast <- function(current_level,
                                     dust_limit,
                                     levels_tbl,
                                     dust_col = "normal_dust"
                                    ,max_level = 50) {
  dust_col = tolower(dust_col)
  # Pull chosen dust column once (fast)
  dusts_all <- levels_tbl[[dust_col]]
  levels_all <- levels_tbl$level
  
  available <- levels_all > current_level & levels_all <= max_level
  
  if (!any(available)) {
    return(list(
      final_level     = current_level,
      total_dust_used = 0,
      levels_gained   = 0
    ))
  }

  dusts <- dusts_all[available]
  levels <- levels_all[available]
  
  cumulative <- cumsum(dusts)
  
  affordable_idx <- which(cumulative <= dust_limit)
  
  if (length(affordable_idx) == 0) {
    return(list(
      final_level = current_level,
      total_dust_used = 0,
      levels_gained = 0
    ))
  }
  
  last_idx <- max(affordable_idx)
  
  return(tibble::tibble(
    final_level = levels[last_idx],
    total_dust_used = cumulative[last_idx],
    levels_gained = levels[last_idx] - current_level
  ))
}

ui <- dashboardPage(
  dashboardHeader(),
  dashboardSidebar(),
  dashboardBody(
    selectInput("raid_tier", "Select a raid tier: ", raid_boss_tier),
    selectInput("raid_boss", "Select a raid boss: ", choices = NULL),
    radioButtons(
      "max_level",
      "What is the maximum level to power-up to?",
      choices = c(20,25,30,35,40,45,50),
      selected = 50
    ),
    numericInput("max_dust", "How much stardust are you prepared to use?", value = 50000, min = 100, max = 700000),
    actionButton("run_scenario", "Run calculations"),
    checkboxInput("legendaries", "Include Legendary Pokemon?", value = TRUE),
    checkboxInput("myth", "Include Mythical Pokemon?", value = TRUE),
    dataTableOutput("normal_output"),
    dataTableOutput("mega_output"),

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
    boss_tiers <- data$raid_boss_list %>%
      filter(tier == input$raid_tier)

    updateSelectInput(
      session,
      "raid_boss",
      choices = boss_tiers$raid_boss
    )
  })

  results <- reactiveValues(normal_output = tibble(),
                            mega_output = tibble())

  observeEvent(input$run_scenario, {
    # shinyjs::disable("run_sim")
    showPageSpinner()
    leveled_up_pokemon <- user_pokemon %>%
        rowwise() %>%
        mutate(sim_level_up = list(max_level_with_dust_fast(
          current_level = level,
          dust_limit = input$max_dust,
          levels_tbl = dust_table,
          max_level = input$max_level,
          dust_col = dust_status
        ))) %>%  tidyr::unnest_wider(sim_level_up) %>%
        filter(levels_gained > 0) %>%
        rowwise() %>%
        mutate(
          attacker = list(
            build_attacker(
              pokemon_id      = pokemon_id,
              level           = final_level,
              fast_move_id    = fast_move_id,
              charged_move_id = charged_move_id,
              shadow = shadow,
              base_stats = base_stats
            )
          )
        ) %>%
        ungroup()

bosses <- boss_move_combinations %>%
  inner_join(raid_bosses) %>%
  filter(Pokemon == input$raid_boss) %>%
  rowwise() %>%
  mutate(boss = list(
    build_boss(
      pokemon_id = Pokemon,
      tier = tier,
      fast_move_id = fast_move,
      charged_move_id = charge_move
  ))) %>%
  ungroup

sim_grid <- tidyr::crossing(
  leveled_up_pokemon,
  bosses)  %>%
  mutate(weather = "Extreme")



print(Sys.time())

with_progress({

  p <- progressor(along = seq_len(nrow(sim_grid)))

  sim_list <- future_map(
    seq_len(nrow(sim_grid)),
    function(i) {

      p()

      sim_grid %>%
        slice(i) %>%
        mutate(sim = map2(
          attacker,
          boss,
          ~ simulate_battle_timeline(
              attacker   = .x,
              boss       = .y,
              weather    = weather,
              friendship = "best"
            )
        ))
    }
    , .options = furrr_options(packages = "pokemonGoSim")) %>%
    list_rbind()

})


sim_list <- sim_list %>%
  mutate(
    dps    = map_dbl(sim, "dps"),
    damage = map_dbl(sim, "damage_done"),
    time   = map_dbl(sim, "time"),
    weather = "Extreme"
  ) %>% 
  select(
    uuid,
    pokemon_id,
    raid_boss = Pokemon,
    level,
    fast_move_id,
    charged_move_id,
    boss_fast_move_id = fast_move,
    boss_charged_move_id = charge_move,
    dps,
    damage,
    time,
    weather,
    tier,
    shadow,
    levels_gained,
    total_dust_used, 
    final_level
  )

existing_sims <- readRDS("data/results_summary.RDS") %>%
  filter(raid_boss == input$raid_boss)

leveled_up_mega <- leveled_up_pokemon %>% filter(grepl("Mega |Primal", pokemon_id))
leveled_up_normal <- leveled_up_pokemon %>% filter(!grepl("Mega |Primal", pokemon_id))

baseline_mega_dps <- existing_sims %>%
   filter(grepl("Mega |Primal", pokemon_id)) %>%
   group_by(pokemon_id, raid_boss, level, fast_move_id, charged_move_id) %>%
   summarise(damage = sum(damage), time = sum(time)) %>%
   mutate(dps = damage / time) %>%
   ungroup %>%
   arrange(desc(dps)) %>%
   top_n(n = 6, wt = dps) %>%
   group_by(raid_boss) %>%
   summarise(avg_dps = mean(dps))

baseline_normal_dps <- existing_sims %>%
   filter(!grepl("Mega |Primal", pokemon_id)) %>%
   group_by(pokemon_id, raid_boss, level, fast_move_id, charged_move_id) %>%
   summarise(damage = sum(damage), time = sum(time)) %>%
   mutate(dps = damage / time) %>%
   ungroup %>%
   arrange(desc(dps)) %>%
   top_n(n = 6, wt = dps) %>%
   group_by(raid_boss) %>%
   summarise(avg_dps = mean(dps))

mega_loop <- tibble()

for (i in 1:nrow(leveled_up_mega)) {
  leveled_up_mega_row <- leveled_up_mega %>% slice(i)

  sim_rows <- sim_list %>%
    inner_join(leveled_up_mega_row)

  interim_step <- existing_sims %>%
    anti_join(leveled_up_mega_row) %>%
    bind_rows(sim_rows) %>%
    filter(grepl("Mega |Primal", pokemon_id)) %>%
    group_by(pokemon_id, raid_boss, level, fast_move_id, charged_move_id) %>%
    summarise(damage = sum(damage), time = sum(time)) %>%
    mutate(dps = damage / time) %>%
    ungroup %>%
    arrange(desc(dps))

  dps_rank <- interim_step %>%
    mutate(dps_rank = rank(desc(dps), ties = "min")) %>%
    inner_join(leveled_up_mega_row) %>%
    select(dps_rank) %>% pull

  mega_loop <- interim_step %>%
    top_n(n = 6, wt = dps) %>%
    group_by(raid_boss) %>%
    summarise(leveled_up_avg_dps = mean(dps)) %>%
    crossing(leveled_up_mega_row) %>%
    mutate(dps_rank = dps_rank) %>%
    bind_rows(mega_loop)
}

mega_output <- mega_loop %>%
  left_join(baseline_mega_dps) %>%
  mutate(dps_gain = leveled_up_avg_dps - avg_dps) %>%
  arrange(desc(dps_gain))

normal_loop <- tibble()

for (i in 1:nrow(leveled_up_normal)) {
  leveled_up_normal_row <- leveled_up_normal %>% slice(i)

  sim_rows <- sim_list %>%
    inner_join(leveled_up_normal_row)

  interim_step <- existing_sims %>%
    anti_join(leveled_up_normal_row) %>%
    bind_rows(sim_rows) %>%
    filter(!grepl("Mega |Primal", pokemon_id)) %>%
    group_by(pokemon_id, raid_boss, level, fast_move_id, charged_move_id) %>%
    summarise(damage = sum(damage), time = sum(time)) %>%
    mutate(dps = damage / time) %>%
    ungroup %>%
    arrange(desc(dps))

  dps_rank <- interim_step %>%
    mutate(dps_rank = rank(desc(dps), ties = "min")) %>%
    inner_join(leveled_up_normal_row) %>%
    select(dps_rank) %>% pull

  normal_loop <- interim_step %>%
    top_n(n = 6, wt = dps) %>%
    group_by(raid_boss) %>%
    summarise(leveled_up_avg_dps = mean(dps)) %>%
    crossing(leveled_up_normal_row) %>%
    mutate(dps_rank = dps_rank) %>%
    bind_rows(normal_loop)
}

normal_output <- normal_loop %>%
  left_join(baseline_normal_dps) %>%
  mutate(dps_gain = leveled_up_avg_dps - avg_dps) %>%
  arrange(desc(dps_gain))
  
  hidePageSpinner()

  results$normal_output <- normal_output %>% select(pokemon_id, dust_status, level, 
    final_level, fast_move_id, charged_move_id, dps_gain, leveled_up_avg_dps,
     dps_rank, levels_gained, total_dust_used)
  results$mega_output <- mega_output %>% select(pokemon_id, dust_status, level, 
    final_level, fast_move_id, charged_move_id, dps_gain, leveled_up_avg_dps,
     dps_rank, levels_gained, total_dust_used)

  # shinyjs::enable("run_sim")
  })

  output$normal_output <- renderDT({
    req(results$normal_output)
  })
  
  output$mega_output <- renderDT({
    req(results$mega_output)
  })

}

shinyApp(ui, server)
