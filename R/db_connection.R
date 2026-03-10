get_con <- function(read_only = FALSE) {
  DBI::dbConnect(duckdb::duckdb(), "data/pokemon.db")
}