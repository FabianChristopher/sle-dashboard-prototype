library(DBI)
library(RPostgres)

# Parse DATABASE_URL into components
parse_db_url <- function(url) {
  # Format: postgresql://user:password@host:port/dbname
  # Remove postgresql:// or postgres:// prefix
  url <- sub("^postgres(ql)?://", "", url)
  
  # Split user:pass from host:port/dbname
  parts <- strsplit(url, "@")[[1]]
  user_pass <- strsplit(parts[1], ":")[[1]]
  host_port_db <- parts[2]
  
  # Split host:port from dbname
  host_port <- strsplit(host_port_db, "/")[[1]]
  host_and_port <- strsplit(host_port[1], ":")[[1]]
  
  list(
    user = user_pass[1],
    password = user_pass[2],
    host = host_and_port[1],
    port = as.integer(host_and_port[2]),
    dbname = host_port[2]
  )
}

get_db <- function() {
  tryCatch({
    # Railway provides DATABASE_URL; fallback to individual env vars for local dev
    db_url <- Sys.getenv("DATABASE_URL", "")
    
    if (nchar(db_url) > 0) {
      # Parse DATABASE_URL (format: postgres://user:pass@host:port/dbname)
      params <- parse_db_url(db_url)
      return(DBI::dbConnect(
        RPostgres::Postgres(),
        host = params$host,
        port = params$port,
        dbname = params$dbname,
        user = params$user,
        password = params$password
      ))
    }
    
    # Fallback to individual environment variables (local Docker setup)
    DBI::dbConnect(
      RPostgres::Postgres(),
      host = Sys.getenv("DB_HOST", "localhost"),
      port = as.integer(Sys.getenv("DB_PORT", "5432")),
      dbname = Sys.getenv("DB_NAME", "sle"),
      user = Sys.getenv("DB_USER", "postgres"),
      password = Sys.getenv("DB_PASSWORD", "postgres")
    )
  }, error = function(e) {
    message("Database connection error: ", e$message)
    NULL
  })
}

db_ping <- function(con) {
  tryCatch(
    {
      DBI::dbGetQuery(con, "SELECT 1")
      TRUE
    },
    error = function(e) {
      FALSE
    }
  )
}
