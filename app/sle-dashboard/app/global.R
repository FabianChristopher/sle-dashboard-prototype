# Enable detailed error logging
options(shiny.sanitize.errors = FALSE)

# Load embedded mock data (no database required!)
source("mock_data.R")

# Static assets - handle both local and container paths
assets_path <- if (dir.exists("assets")) {
  normalizePath("assets", mustWork = FALSE)
} else if (dir.exists(file.path("..", "assets"))) {
  normalizePath(file.path("..", "assets"), mustWork = FALSE)
} else {
  "assets"
}

shiny::addResourcePath("assets", assets_path)
