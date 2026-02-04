library(tidyverse)
library(googlesheets4)

base_stats <- read_sheet("https://docs.google.com/spreadsheets/d/1cjTin49W2AkW9Z2ndJ59IDZ3o64FGtj2LhYrx-QoxHg/",
                          sheet = "Base Stats")

levels <- read_sheet("https://docs.google.com/spreadsheets/d/1cjTin49W2AkW9Z2ndJ59IDZ3o64FGtj2LhYrx-QoxHg/",
                    sheet = "Levels")

CP_Formula <- function(Attack, Attack_IV, Defence, Defence_IV, HP, HP_IV, CP_Multiplier, Nerf) {
  return(floor((((Attack+Attack_IV)*(Defence+Defence_IV)^0.5*(HP+HP_IV)^0.5*(CP_Multiplier)^2)/10)*Nerf))
}

HP_Formula <- function(Base_HP, HP_IV, CP_Multiplier) {
  return(floor((Base_HP+HP_IV) * CP_Multiplier))
}

CP_Finder <- function(pokemon, CP_Search, status = NULL, dust = NULL, Visible_HP_Search = NULL, HP_IV_Search = NULL, attack_IV_Search = NULL, defence_IV_Search = NULL
  , base_stats_input = base_stats
  , levels_input = levels) {
  
  base_stats_internal <- base_stats %>%
    filter(name == pokemon) %>%
    select(name, Nerf, `HP Go`, `Attack Go`, `Defence Go`) %>%
    mutate(join = 1)

  if(is.null(status)) {
    levels_internal <- levels %>%
        select(`Level`, `CP Multiplier`, Dust = `Marginal Dust`) %>%
        mutate(join = 1)
  } else if (status == "Lucky") {
    levels_internal <- levels %>%
        select(`Level`, `CP Multiplier`, Dust = `Marginal Dust Lucky`) %>%
        mutate(join = 1)
  } else if (status == "Shadow") {
    levels_internal <- levels %>%
        select(`Level`, `CP Multiplier`, Dust = `Marginal Dust Shadow`) %>%
        mutate(join = 1)
  } else if (status == "Purified") {
    levels_internal <- levels %>%
        select(`Level`, `CP Multiplier`, Dust = `Marginal Dust Purified`) %>%
        mutate(join = 1)
  }

df_out <- levels_internal %>%
  left_join(tibble(join = 1, Attack_IV = seq(from = 0, to = 15, by = 1))) %>%
  left_join(tibble(join = 1, Def_IV = seq(from = 0, to = 15, by = 1))) %>%
  left_join(tibble(join = 1, HP_IV = seq(from = 0, to = 15, by = 1))) %>%
  left_join(base_stats_internal) %>%
  select(-join) %>%
  mutate(CP = CP_Formula(
    Attack = `Attack Go`,
    Attack_IV = Attack_IV,
    Defence =  `Defence Go`,
    Defence_IV = Def_IV,
    HP = `HP Go`,
    HP_IV = HP_IV,
    CP_Multiplier = `CP Multiplier`,
    Nerf = Nerf
  )) %>%
  filter(CP == CP_Search) %>%
  mutate(Visible_HP = HP_Formula(
    Base_HP = `HP Go`, HP_IV = HP_IV, CP_Multiplier = `CP Multiplier`)) 
  
  if(!is.null(Visible_HP_Search)) {
    df_out <- df_out %>%
      filter(Visible_HP == Visible_HP_Search)
    }

  if(!is.null(HP_IV_Search)) {
    df_out <- df_out %>%
      filter(HP_IV == HP_IV_Search)
    }
  
  if(!is.null(attack_IV_Search)) {
    df_out <- df_out %>%
      filter(Attack_IV == attack_IV_Search)
    }
  
  if(!is.null(defence_IV_Search)) {
    df_out <- df_out %>%
      filter(Def_IV == defence_IV_Search)
    }
  
  df_out <- df_out %>%
    select(name, CP, Level, Dust, HP = Visible_HP, Attack_IV, Def_IV, HP_IV)
  return(df_out)
}

CP_Finder(pokemon = "Zekrom",
          CP_Search = 3961,
        HP_IV_Search = 15) 


