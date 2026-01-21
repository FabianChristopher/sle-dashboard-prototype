library(DBI)
library(RPostgres)

# Parse DATABASE_URL into components
parse_db_url <- function(url) {
  # Format: postgresql://user:password@host:port/dbname
  # Remove postgresql:// or postgres:// prefix
  url <- sub("^postgres(ql)?://", "", url)

  # Separate optional query string (e.g. ?sslmode=require)
  query <- ""
  if (grepl("?", url, fixed = TRUE)) {
    parts_q <- strsplit(url, "?", fixed = TRUE)[[1]]
    url <- parts_q[1]
    query <- parts_q[2]
  }
  
  # Split user:pass from host:port/dbname
  parts <- strsplit(url, "@")[[1]]
  user_pass <- strsplit(parts[1], ":")[[1]]
  host_port_db <- parts[2]
  
  # Split host:port from dbname
  host_port <- strsplit(host_port_db, "/")[[1]]
  host_and_port <- strsplit(host_port[1], ":")[[1]]

  host <- host_and_port[1]
  port <- 5432L
  if (length(host_and_port) >= 2 && nchar(host_and_port[2]) > 0) {
    port <- as.integer(host_and_port[2])
  }

  # Parse query parameters (currently we only care about sslmode)
  sslmode <- NULL
  if (nchar(query) > 0) {
    kvs <- strsplit(query, "&", fixed = TRUE)[[1]]
    for (kv in kvs) {
      pair <- strsplit(kv, "=", fixed = TRUE)[[1]]
      if (length(pair) == 2 && pair[1] == "sslmode") {
        sslmode <- pair[2]
      }
    }
  }

  list(
    user = user_pass[1],
    password = user_pass[2],
    host = host,
    port = port,
    dbname = host_port[2],
    sslmode = sslmode
  )
}

get_db <- function() {
  tryCatch({
    # Railway/Render provide DATABASE_URL; fallback to individual env vars for local dev
    db_url <- Sys.getenv("DATABASE_URL", "")
    
    message("[DB] DATABASE_URL present: ", nchar(db_url) > 0)
    if (nchar(db_url) > 0) {
      # Parse DATABASE_URL (format: postgres://user:pass@host:port/dbname)
      params <- parse_db_url(db_url)

      args <- list(
        drv = RPostgres::Postgres(),
        host = params$host,
        port = params$port,
        dbname = params$dbname,
        user = params$user,
        password = params$password
      )
      if (!is.null(params$sslmode) && nchar(params$sslmode) > 0) {
        args$sslmode <- params$sslmode
      }

      return(do.call(DBI::dbConnect, args))
    }
    
    # Fallback to individual environment variables (local Docker setup)
    message("[DB] Fallback to DB_* env vars")
    message("[DB] DB_HOST=", Sys.getenv("DB_HOST", "localhost"))
    message("[DB] DB_USER=", Sys.getenv("DB_USER", "postgres"))
    DBI::dbConnect(
      RPostgres::Postgres(),
      host = Sys.getenv("DB_HOST", "localhost"),
      port = as.integer(Sys.getenv("DB_PORT", "5432")),
      dbname = Sys.getenv("DB_NAME", "sle"),
      user = Sys.getenv("DB_USER", "postgres"),
      password = Sys.getenv("DB_PASSWORD", "postgres")
    )
  }, error = function(e) {
    message("[DB ERROR] Database connection error: ", e$message)
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
