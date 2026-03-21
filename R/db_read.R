get_user_pokemon <- function(user_id) {

  con <- get_con(read_only = TRUE)
  on.exit(DBI::dbDisconnect(con, shutdown = FALSE))

  dplyr::tbl(con, "user_pokemon") |>
    dplyr::filter(user_id == !!user_id) |>
    dplyr::collect()

}

get_user_pokemon_enriched <- function(user_id) {

  con <- get_con(read_only = TRUE)
  on.exit(DBI::dbDisconnect(con, shutdown = FALSE))

  dplyr::tbl(con, "user_pokemon_enriched") |>
    dplyr::filter(user_id == !!user_id) |>
    dplyr::collect()

}

get_user_pokemon_to_sim <- function(user_id) {

  con <- get_con(read_only = TRUE)
  on.exit(DBI::dbDisconnect(con, shutdown = FALSE))

  dplyr::tbl(con, "user_pokemon_to_sim") |>
    dplyr::filter(user_id == !!user_id) |>
    dplyr::collect()

}

get_pokemon_id <- function() {

  con <- get_con(read_only = TRUE)
  on.exit(DBI::dbDisconnect(con, shutdown = FALSE))

  dplyr::tbl(con, "pokemon") |>
    dplyr::collect()

}

get_base_stats <- function() {

  con <- get_con(read_only = TRUE)
  on.exit(DBI::dbDisconnect(con, shutdown = FALSE))

  dplyr::tbl(con, "base_stats") |>
    dplyr::collect()

}

get_boss_move_combinations <- function() {

  con <- get_con(read_only = TRUE)
  on.exit(DBI::dbDisconnect(con, shutdown = FALSE))

  dplyr::tbl(con, "boss_move_combinations") |>
    dplyr::collect()

}

get_calendar <- function() {

  con <- get_con(read_only = TRUE)
  on.exit(DBI::dbDisconnect(con, shutdown = FALSE))

  dplyr::tbl(con, "calendar") |>
    dplyr::collect()

}

get_levels <- function() {

  con <- get_con(read_only = TRUE)
  on.exit(DBI::dbDisconnect(con, shutdown = FALSE))

  dplyr::tbl(con, "levels") |>
    dplyr::collect()

}

get_mega_table <- function() {

  con <- get_con(read_only = TRUE)
  on.exit(DBI::dbDisconnect(con, shutdown = FALSE))

  dplyr::tbl(con, "mega_table") |>
    dplyr::collect()

}

get_moves <- function() {

  con <- get_con(read_only = TRUE)
  on.exit(DBI::dbDisconnect(con, shutdown = FALSE))

  dplyr::tbl(con, "moves") |>
    dplyr::collect()

}

get_moves_formatted <- function() {

  con <- get_con(read_only = TRUE)
  on.exit(DBI::dbDisconnect(con, shutdown = FALSE))

  dplyr::tbl(con, "moves_formatted") |>
    dplyr::collect()

}

# get_type_effectiveness_db <- function() {

#   con <- get_con(read_only = TRUE)
#   on.exit(DBI::dbDisconnect(con, shutdown = FALSE))

#   dplyr::tbl(con, "type_effectiveness") |>
#     dplyr::collect()

# }

get_user_move_combinations <- function() {

  con <- get_con(read_only = TRUE)
  on.exit(DBI::dbDisconnect(con, shutdown = FALSE))

  dplyr::tbl(con, "user_move_combinations") |>
    dplyr::collect()

}

get_weather <- function() {

  con <- get_con(read_only = TRUE)
  on.exit(DBI::dbDisconnect(con, shutdown = FALSE))

  dplyr::tbl(con, "weather") |>
    dplyr::collect()

}

get_dust_status <- function() {

  con <- get_con(read_only = TRUE)
  on.exit(DBI::dbDisconnect(con, shutdown = FALSE))

  dplyr::tbl(con, "dust_status") |>
    dplyr::collect()

}

get_users <- function() {

  con <- get_con(read_only = TRUE)
  on.exit(DBI::dbDisconnect(con, shutdown = FALSE))

  dplyr::tbl(con, "users") |>
    dplyr::collect()

}

get_user_battle_results <- function(user_id) {

  con <- get_con(read_only = TRUE)
  on.exit(DBI::dbDisconnect(con, shutdown = FALSE))

  dplyr::tbl(con, "user_battle_results") |>
    dplyr::filter(user_id == !!user_id) |>
    dplyr::collect()

}

get_raw_pokemon_moves <- function() {

  con <- get_con(read_only = TRUE)
  on.exit(DBI::dbDisconnect(con, shutdown = FALSE))

  dplyr::tbl(con, "pokemon_moves") |>
    dplyr::collect()

}

get_hypo_matchups <- function(raid_boss, tier, friendship = "best", party_power = 2) {
    con <- get_con(read_only = TRUE)
  on.exit(DBI::dbDisconnect(con, shutdown = FALSE))

  dplyr::tbl(con, "hypothetical_matchups") |>
    dplyr::filter(raid_boss == !!raid_boss,
                  tier == !!tier,
                  friendship == !!friendship,
                  party_power == !!party_power
              ) |>
    dplyr::collect()
}

