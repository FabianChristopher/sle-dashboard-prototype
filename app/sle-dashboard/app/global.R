library(DBI)
library(RPostgres)

# Enable detailed error logging
options(shiny.sanitize.errors = FALSE)

# Load environment variables for DB connectivity (only if .env exists - for local dev)
env_file <- file.path("..", ".env")
if (file.exists(env_file)) {
  tryCatch({
    library(dotenv)
    dotenv::load_dot_env(file = env_file)
  }, error = function(e) {
    message("Note: .env file not loaded: ", e$message)
  })
}

# DB helpers.
source("db.R")

# Static assets - handle both local and container paths
assets_path <- if (dir.exists("assets")) {
  normalizePath("assets", mustWork = FALSE)
} else if (dir.exists(file.path("..", "assets"))) {
  normalizePath(file.path("..", "assets"), mustWork = FALSE)
} else {
  "assets"
}

shiny::addResourcePath("assets", assets_path)
