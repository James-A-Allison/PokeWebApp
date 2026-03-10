add_user_pokemon <- function(df) {

  con <- get_con()
  on.exit(DBI::dbDisconnect(con, shutdown = FALSE))

  DBI::dbAppendTable(con, "user_pokemon", df)

}