#!/usr/bin/env Rscript

suppressPackageStartupMessages({
  library(DBI)
  library(RPostgres)
  library(dotenv)
  library(readxl)
})

dotenv::load_dot_env(file = ".env")
source("app/db.R")

excel_path <- "../../renal bx pilot-mock patients.xlsx"
if (!file.exists(excel_path)) {
  stop("Excel file not found: ", excel_path)
}

external_ids <- c("PT1", "PT2", "PT3")
sheet_map <- c(
  "patient 1" = "PT1",
  "patient 2" = "PT2",
  "patient 3" = "PT3"
)

clean_str <- function(x) {
  x <- as.character(x)
  x <- trimws(x)
  x[x == ""] <- NA_character_
  x
}

parse_lab_value <- function(value) {
  val <- clean_str(value)
  if (is.na(val)) {
    return(NA_real_)
  }
  if (grepl("\\*10\\^", val)) {
    parts <- strsplit(val, "\\*10\\^")[[1]]
    base <- suppressWarnings(as.numeric(gsub("[^0-9.-]", "", parts[1])))
    exp <- suppressWarnings(as.numeric(gsub("[^0-9.-]", "", parts[2])))
    if (!is.na(base) && !is.na(exp)) {
      return(base * (10 ^ exp))
    }
  }
  suppressWarnings(as.numeric(gsub("[^0-9.-]", "", val)))
}

extract_unit <- function(name) {
  match <- regmatches(name, regexpr("\\(([^)]+)\\)", name))
  if (length(match) == 0) {
    return(NA_character_)
  }
  gsub("^\\(|\\)$", "", match)
}

clean_lab_name <- function(name) {
  trimws(gsub("\\s*\\([^)]*\\)\\s*$", "", name))
}

timepoint_offset <- function(label) {
  label <- tolower(label)
  if (grepl("before", label)) {
    return(-90)
  }
  if (grepl("after", label)) {
    return(90)
  }
  0
}

con <- get_db()
on.exit(DBI::dbDisconnect(con), add = TRUE)

placeholders <- paste0("$", seq_along(external_ids), collapse = ",")
sql <- paste0("DELETE FROM patients WHERE external_id IN (", placeholders, ")")
DBI::dbExecute(con, sql, params = as.list(external_ids))

# The workbook does not include patient demographics (name/sex/dx date), so we only seed
# the external IDs (PT1–PT3). The UI will display only what exists in the DB.
patients_seed <- data.frame(
  external_id = external_ids,
  stringsAsFactors = FALSE
)

DBI::dbAppendTable(con, "patients", patients_seed)

placeholders <- paste0("$", seq_along(external_ids), collapse = ",")
sql <- paste0(
  "SELECT patient_id, external_id FROM patients WHERE external_id IN (",
  placeholders,
  ")"
)
patients_lookup <- DBI::dbGetQuery(con, sql, params = as.list(external_ids))
patient_id_map <- setNames(patients_lookup$patient_id, patients_lookup$external_id)

visits_rows <- list()
patient_index <- 0
for (sheet in names(sheet_map)) {
  patient_index <- patient_index + 1
  external_id <- sheet_map[[sheet]]
  patient_id <- patient_id_map[[external_id]]

  df <- readxl::read_excel(excel_path, sheet = sheet, col_names = FALSE)
  m <- as.matrix(df)
  col1 <- clean_str(m[, 1])
  lab_header_row <- which(col1 == "Laboratory Findings")[1]

  header_vals <- clean_str(m[lab_header_row, ])
  tp_cols <- which(!is.na(header_vals) & seq_along(header_vals) != 1)
  tp_labels <- header_vals[tp_cols]

  base_date <- as.Date("2024-01-01") + (patient_index - 1) * 30
  for (i in seq_along(tp_labels)) {
    visits_rows[[length(visits_rows) + 1]] <- data.frame(
      patient_id = patient_id,
      visit_date = base_date + timepoint_offset(tp_labels[i]),
      visit_type = tp_labels[i],
      notes = NA_character_,
      stringsAsFactors = FALSE
    )
  }
}

visits_df <- do.call(rbind, visits_rows)
DBI::dbAppendTable(con, "visits", visits_df)

placeholders <- paste0("$", seq_along(patient_id_map), collapse = ",")
sql <- paste0(
  "SELECT visit_id, patient_id, visit_type, visit_date FROM visits WHERE patient_id IN (",
  placeholders,
  ")"
)
visits_lookup <- DBI::dbGetQuery(
  con,
  sql,
  params = as.list(unname(patient_id_map))
)
visits_lookup$key <- paste(visits_lookup$patient_id, visits_lookup$visit_type, sep = "|")
visit_id_map <- setNames(visits_lookup$visit_id, visits_lookup$key)
visit_date_map <- setNames(as.Date(visits_lookup$visit_date), visits_lookup$key)

labs_rows <- list()
domains_rows <- list()
patient_index <- 0
for (sheet in names(sheet_map)) {
  patient_index <- patient_index + 1
  external_id <- sheet_map[[sheet]]
  patient_id <- patient_id_map[[external_id]]

  df <- readxl::read_excel(excel_path, sheet = sheet, col_names = FALSE)
  m <- as.matrix(df)
  col1 <- clean_str(m[, 1])
  lab_header_row <- which(col1 == "Laboratory Findings")[1]

  header_vals <- clean_str(m[lab_header_row, ])
  tp_cols <- which(!is.na(header_vals) & seq_along(header_vals) != 1)
  tp_labels <- header_vals[tp_cols]

  r <- lab_header_row + 1
  while (r <= nrow(m)) {
    lab_name_raw <- clean_str(m[r, 1])
    if (is.na(lab_name_raw)) {
      break
    }
    lab_unit <- extract_unit(lab_name_raw)
    lab_name <- clean_lab_name(lab_name_raw)
    for (i in seq_along(tp_cols)) {
      value_raw <- clean_str(m[r, tp_cols[i]])
      if (is.na(value_raw)) {
        next
      }
      key <- paste(patient_id, tp_labels[i], sep = "|")
      labs_rows[[length(labs_rows) + 1]] <- data.frame(
        patient_id = patient_id,
        visit_id = visit_id_map[[key]],
        collected_date = visit_date_map[[key]],
        lab_name = lab_name,
        lab_value = parse_lab_value(value_raw),
        lab_unit = lab_unit,
        reference_range_low = NA_real_,
        reference_range_high = NA_real_,
        abnormal_flag = as.logical(NA),
        stringsAsFactors = FALSE
      )
    }
    r <- r + 1
  }

  domain_header_row <- NA_integer_
  for (i in seq_len(nrow(m))) {
    if (any(clean_str(m[i, ]) == "Y/N (active)", na.rm = TRUE)) {
      domain_header_row <- i
      break
    }
  }
  if (!is.na(domain_header_row)) {
    r <- domain_header_row + 1
    while (r <= nrow(m)) {
      domain_names <- clean_str(m[r, tp_cols])
      active_vals <- clean_str(m[r, tp_cols + 1])
      domain_name <- domain_names[!is.na(domain_names)][1]
      if (is.na(domain_name)) {
        break
      }
      ever_involved <- any(toupper(active_vals) == "Y", na.rm = TRUE)
      for (i in seq_along(tp_labels)) {
        active_val <- active_vals[i]
        active <- !is.na(active_val) && toupper(active_val) == "Y"
        key <- paste(patient_id, tp_labels[i], sep = "|")
        domains_rows[[length(domains_rows) + 1]] <- data.frame(
          patient_id = patient_id,
          visit_id = visit_id_map[[key]],
          assessed_date = visit_date_map[[key]],
          domain_name = trimws(domain_name),
          domain_score = NA_real_,
          active = active,
          ever_involved = ever_involved,
          stringsAsFactors = FALSE
        )
      }
      r <- r + 1
    }
  }
}

if (length(labs_rows) > 0) {
  labs_df <- do.call(rbind, labs_rows)
  DBI::dbAppendTable(con, "labs", labs_df)
}

if (length(domains_rows) > 0) {
  domains_df <- do.call(rbind, domains_rows)
  DBI::dbAppendTable(con, "domains", domains_df)
}

for (table_name in c("patients", "visits", "labs", "domains", "medications")) {
  count <- DBI::dbGetQuery(
    con,
    paste0("SELECT COUNT(*) AS count FROM ", table_name)
  )$count[1]
  message(table_name, ": ", count)
}
