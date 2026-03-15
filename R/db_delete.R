remove_user_pokemon <- function(user_id, pokemon_instance_id) {
  
  con <- get_con(read_only = TRUE)
  on.exit(DBI::dbDisconnect(con, shutdown = FALSE))

  query <- "
    DELETE FROM user_pokemon
    WHERE user_id = ? AND pokemon_instance_id = ?
  "
  
  DBI::dbExecute(
    con,
    query,
    params = list(user_id, pokemon_instance_id)
  )
}

remove_move <- function(pokemon_id, move_id) {
  
  con <- get_con(read_only = TRUE)
  on.exit(DBI::dbDisconnect(con, shutdown = FALSE))

  query <- "
    DELETE FROM pokemon_moves
    WHERE pokemon_id = ? AND move_id = ?
  "
  
  DBI::dbExecute(
    con,
    query,
    params = list(pokemon_id, move_id)
  )
}