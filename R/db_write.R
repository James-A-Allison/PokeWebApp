add_user_pokemon <- function(df) {

  con <- get_con()
  on.exit(DBI::dbDisconnect(con, shutdown = FALSE))

  DBI::dbAppendTable(con, "user_pokemon", df)

}

add_user_battle_results <- function(df) {

  con <- get_con()
  on.exit(DBI::dbDisconnect(con, shutdown = FALSE))

  DBI::dbAppendTable(con, "user_battle_results", df)

}

add_pokemon_move <- function(df) {

  con <- get_con()
  on.exit(DBI::dbDisconnect(con, shutdown = FALSE))

  DBI::dbAppendTable(con, "pokemon_moves", df)

}