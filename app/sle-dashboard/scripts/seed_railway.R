#!/usr/bin/env Rscript
# This script seeds the Railway Postgres database with mock data
# Run this ONCE after deploying to Railway

suppressPackageStartupMessages({
  library(DBI)
  library(RPostgres)
})

# Connect using DATABASE_URL (Railway provides this)
db_url <- Sys.getenv("DATABASE_URL")

parse_db_url <- function(url) {
  # Parse postgresql://user:pass@host:port/dbname[?sslmode=...]
  url <- sub("^postgres(ql)?://", "", url)

  query <- ""
  if (grepl("\\?", url, fixed = TRUE)) {
    parts_q <- strsplit(url, "\\?", fixed = FALSE)[[1]]
    url <- parts_q[1]
    query <- parts_q[2]
  }

  parts <- strsplit(url, "@")[[1]]
  user_pass <- strsplit(parts[1], ":")[[1]]
  host_port_db <- parts[2]
  host_port <- strsplit(host_port_db, "/")[[1]]
  host_and_port <- strsplit(host_port[1], ":")[[1]]

  host <- host_and_port[1]
  port <- 5432L
  if (length(host_and_port) >= 2 && nchar(host_and_port[2]) > 0) {
    port <- as.integer(host_and_port[2])
  }

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

if (nchar(db_url) == 0) {
  # Fallback to individual env vars
  con <- DBI::dbConnect(
    RPostgres::Postgres(),
    host = Sys.getenv("DB_HOST", "localhost"),
    port = as.integer(Sys.getenv("DB_PORT", "5432")),
    dbname = Sys.getenv("DB_NAME", "railway"),
    user = Sys.getenv("DB_USER", "postgres"),
    password = Sys.getenv("DB_PASSWORD", "")
  )
} else {
  db_params <- parse_db_url(db_url)
  args <- list(
    drv = RPostgres::Postgres(),
    host = db_params$host,
    port = db_params$port,
    dbname = db_params$dbname,
    user = db_params$user,
    password = db_params$password
  )
  if (!is.null(db_params$sslmode) && nchar(db_params$sslmode) > 0) {
    args$sslmode <- db_params$sslmode
  }
  con <- do.call(DBI::dbConnect, args)
}

cat("Connected to database!\n")

# Create schema
schema_sql <- "
-- Patients table
CREATE TABLE IF NOT EXISTS patients (
  patient_id SERIAL PRIMARY KEY,
  external_id TEXT UNIQUE NOT NULL,
  first_name TEXT,
  last_name TEXT,
  sex TEXT,
  diagnosis_date DATE
);

-- Visits table  
CREATE TABLE IF NOT EXISTS visits (
  visit_id SERIAL PRIMARY KEY,
  patient_id INTEGER REFERENCES patients(patient_id) ON DELETE CASCADE,
  visit_date DATE,
  visit_type TEXT,
  notes TEXT
);

-- Labs table
CREATE TABLE IF NOT EXISTS labs (
  lab_id SERIAL PRIMARY KEY,
  patient_id INTEGER REFERENCES patients(patient_id) ON DELETE CASCADE,
  visit_id INTEGER REFERENCES visits(visit_id) ON DELETE CASCADE,
  collected_date DATE,
  lab_name TEXT NOT NULL,
  lab_value NUMERIC,
  lab_unit TEXT,
  reference_range_low NUMERIC,
  reference_range_high NUMERIC,
  abnormal_flag BOOLEAN
);

-- Domains table
CREATE TABLE IF NOT EXISTS domains (
  domain_id SERIAL PRIMARY KEY,
  patient_id INTEGER REFERENCES patients(patient_id) ON DELETE CASCADE,
  visit_id INTEGER REFERENCES visits(visit_id) ON DELETE CASCADE,
  assessed_date DATE,
  domain_name TEXT NOT NULL,
  domain_score NUMERIC,
  active BOOLEAN,
  ever_involved BOOLEAN
);

-- Medications table
CREATE TABLE IF NOT EXISTS medications (
  medication_id SERIAL PRIMARY KEY,
  patient_id INTEGER REFERENCES patients(patient_id) ON DELETE CASCADE,
  medication_name TEXT NOT NULL,
  category TEXT,
  dose TEXT,
  route TEXT,
  frequency TEXT,
  start_date DATE,
  end_date DATE,
  current BOOLEAN DEFAULT TRUE,
  stop_reason TEXT,
  indication TEXT
);
"

cat("Creating schema...\n")
DBI::dbExecute(con, schema_sql)

# Clear existing data
cat("Clearing existing data...\n")
DBI::dbExecute(con, "DELETE FROM medications")
DBI::dbExecute(con, "DELETE FROM domains")
DBI::dbExecute(con, "DELETE FROM labs")
DBI::dbExecute(con, "DELETE FROM visits")
DBI::dbExecute(con, "DELETE FROM patients")

# Insert patients
cat("Inserting patients...\n")
DBI::dbExecute(con, "INSERT INTO patients (external_id) VALUES ('PT1'), ('PT2'), ('PT3')")

# Get patient IDs
patients <- DBI::dbGetQuery(con, "SELECT patient_id, external_id FROM patients")
pt_map <- setNames(patients$patient_id, patients$external_id)

# Insert visits for each patient (3 timepoints each)
cat("Inserting visits...\n")
timepoints <- c("3 Months Before Biopsy", "Biopsy", "3 Months After Biopsy")
for (ext_id in c("PT1", "PT2", "PT3")) {
  pid <- pt_map[[ext_id]]
  base_date <- as.Date("2024-01-01")
  for (i in seq_along(timepoints)) {
    offset <- c(-90, 0, 90)[i]
    DBI::dbExecute(con, 
      "INSERT INTO visits (patient_id, visit_date, visit_type) VALUES ($1, $2, $3)",
      params = list(pid, base_date + offset, timepoints[i])
    )
  }
}

# Get visit IDs
visits <- DBI::dbGetQuery(con, "SELECT visit_id, patient_id, visit_type FROM visits")

# Lab data from Excel (PT1, PT2, PT3)
cat("Inserting labs...\n")
lab_data <- list(
  PT1 = list(
    "3 Months Before Biopsy" = list(
      "Platelets" = list(value = 288000, unit = "K/µL"),
      "WBC" = list(value = 6400, unit = "K/µL"),
      "ALC" = list(value = 1300, unit = "cells/µL"),
      "IgA" = list(value = 362, unit = "mg/dL"),
      "ANC units" = list(value = 4400, unit = "cells/µL"),
      "Anti ds-DNA" = list(value = 4, unit = NA),
      "ESR" = list(value = 26, unit = "mm/hr"),
      "CRP" = list(value = 0.5, unit = "mg/dl"),
      "C3 complement" = list(value = 70.8, unit = "mg/dL"),
      "C4 complement" = list(value = 14.7, unit = "mg/dL"),
      "Urine Protein" = list(value = 36, unit = "mg/dL"),
      "Urine Creatinine" = list(value = 38, unit = "mg/dL"),
      "UPCR" = list(value = 0.95, unit = NA),
      "Albumin" = list(value = 5.2, unit = NA),
      "eGFR" = list(value = 132, unit = NA)
    ),
    "Biopsy" = list(
      "Platelets" = list(value = 232000, unit = "K/µL"),
      "WBC" = list(value = 6900, unit = "K/µL"),
      "ALC" = list(value = 1300, unit = "cells/µL"),
      "IgA" = list(value = 340, unit = "mg/dL"),
      "ANC units" = list(value = 4700, unit = "cells/µL"),
      "Anti ds-DNA" = list(value = 2, unit = NA),
      "ESR" = list(value = 20, unit = "mm/hr"),
      "CRP" = list(value = 0.5, unit = "mg/dl"),
      "C3 complement" = list(value = 84.1, unit = "mg/dL"),
      "C4 complement" = list(value = 33.5, unit = "mg/dL"),
      "Urine Protein" = list(value = 28, unit = "mg/dL"),
      "Urine Creatinine" = list(value = 40, unit = "mg/dL"),
      "UPCR" = list(value = 0.7, unit = NA),
      "Albumin" = list(value = 4.9, unit = NA),
      "eGFR" = list(value = 143, unit = NA)
    ),
    "3 Months After Biopsy" = list(
      "Platelets" = list(value = 256000, unit = "K/µL"),
      "WBC" = list(value = 6600, unit = "K/µL"),
      "ALC" = list(value = 1000, unit = "cells/µL"),
      "IgA" = list(value = 350, unit = "mg/dL"),
      "ANC units" = list(value = 4900, unit = "cells/µL"),
      "Anti ds-DNA" = list(value = 4, unit = NA),
      "ESR" = list(value = 16, unit = "mm/hr"),
      "CRP" = list(value = 0.5, unit = "mg/dl"),
      "C3 complement" = list(value = 84.7, unit = "mg/dL"),
      "C4 complement" = list(value = 28.6, unit = "mg/dL"),
      "Urine Protein" = list(value = 29, unit = "mg/dL"),
      "Urine Creatinine" = list(value = 43, unit = "mg/dL"),
      "UPCR" = list(value = 0.67, unit = NA),
      "Albumin" = list(value = 4.6, unit = NA),
      "eGFR" = list(value = 140, unit = NA)
    )
  ),
  PT2 = list(
    "3 Months Before Biopsy" = list(
      "Platelets" = list(value = 288000, unit = "K/µL"),
      "WBC" = list(value = 6400, unit = "K/µL"),
      "ALC" = list(value = 1300, unit = "cells/µL"),
      "IgA" = list(value = 362, unit = "mg/dL"),
      "ANC units" = list(value = 4400, unit = "cells/µL"),
      "Anti ds-DNA" = list(value = 4, unit = NA),
      "ESR" = list(value = 26, unit = "mm/hr"),
      "CRP" = list(value = 0.5, unit = "mg/dl"),
      "C3 complement" = list(value = 70.8, unit = "mg/dL"),
      "C4 complement" = list(value = 12.4, unit = "mg/dL"),
      "Urine Protein" = list(value = 102, unit = "mg/dL"),
      "Urine Creatinine" = list(value = 49, unit = "mg/dL"),
      "UPCR" = list(value = 2.1, unit = NA),
      "Albumin" = list(value = 3.3, unit = NA),
      "eGFR" = list(value = 80, unit = NA)
    ),
    "Biopsy" = list(
      "Platelets" = list(value = 200000, unit = "K/µL"),
      "WBC" = list(value = 6000, unit = "K/µL"),
      "ALC" = list(value = 1000, unit = "cells/µL"),
      "IgA" = list(value = 413, unit = "mg/dL"),
      "ANC units" = list(value = 3900, unit = "cells/µL"),
      "Anti ds-DNA" = list(value = 4, unit = NA),
      "ESR" = list(value = 42, unit = "mm/hr"),
      "CRP" = list(value = 1, unit = "mg/dl"),
      "C3 complement" = list(value = 64.3, unit = "mg/dL"),
      "C4 complement" = list(value = 10.1, unit = "mg/dL"),
      "Urine Protein" = list(value = 110, unit = "mg/dL"),
      "Urine Creatinine" = list(value = 50, unit = "mg/dL"),
      "UPCR" = list(value = 2.2, unit = NA),
      "Albumin" = list(value = 3.1, unit = NA),
      "eGFR" = list(value = 74, unit = NA)
    ),
    "3 Months After Biopsy" = list(
      "Platelets" = list(value = 180000, unit = "K/µL"),
      "WBC" = list(value = 5200, unit = "K/µL"),
      "ALC" = list(value = 800, unit = "cells/µL"),
      "IgA" = list(value = 500, unit = "mg/dL"),
      "ANC units" = list(value = 3400, unit = "cells/µL"),
      "Anti ds-DNA" = list(value = 4, unit = NA),
      "ESR" = list(value = 54, unit = "mm/hr"),
      "CRP" = list(value = 1, unit = "mg/dl"),
      "C3 complement" = list(value = 59.2, unit = "mg/dL"),
      "C4 complement" = list(value = 11.2, unit = "mg/dL"),
      "Urine Protein" = list(value = 120, unit = "mg/dL"),
      "Urine Creatinine" = list(value = 61, unit = "mg/dL"),
      "UPCR" = list(value = 2, unit = NA),
      "Albumin" = list(value = 3.5, unit = NA),
      "eGFR" = list(value = 78, unit = NA)
    )
  ),
  PT3 = list(
    "3 Months Before Biopsy" = list(
      "Platelets" = list(value = 288000, unit = "K/µL"),
      "WBC" = list(value = 6400, unit = "K/µL"),
      "ALC" = list(value = 1300, unit = "cells/µL"),
      "IgA" = list(value = 223, unit = "mg/dL"),
      "ANC units" = list(value = 4400, unit = "cells/µL"),
      "Anti ds-DNA" = list(value = 4, unit = NA),
      "ESR" = list(value = 14, unit = "mm/hr"),
      "CRP" = list(value = 0.5, unit = "mg/dl"),
      "C3 complement" = list(value = 84.3, unit = "mg/dL"),
      "C4 complement" = list(value = 18.3, unit = "mg/dL"),
      "Urine Protein" = list(value = 30, unit = "mg/dL"),
      "Urine Creatinine" = list(value = 48, unit = "mg/dL"),
      "UPCR" = list(value = 0.63, unit = NA),
      "Albumin" = list(value = 4.8, unit = NA),
      "eGFR" = list(value = 132, unit = NA)
    ),
    "Biopsy" = list(
      "Platelets" = list(value = 300000, unit = "K/µL"),
      "WBC" = list(value = 6900, unit = "K/µL"),
      "ALC" = list(value = 1500, unit = "cells/µL"),
      "IgA" = list(value = 246, unit = "mg/dL"),
      "ANC units" = list(value = 4700, unit = "cells/µL"),
      "Anti ds-DNA" = list(value = 2, unit = NA),
      "ESR" = list(value = 20, unit = "mm/hr"),
      "CRP" = list(value = 0.5, unit = "mg/dl"),
      "C3 complement" = list(value = 88.9, unit = "mg/dL"),
      "C4 complement" = list(value = 24.7, unit = "mg/dL"),
      "Urine Protein" = list(value = 24, unit = "mg/dL"),
      "Urine Creatinine" = list(value = 40, unit = "mg/dL"),
      "UPCR" = list(value = 0.57, unit = NA),
      "Albumin" = list(value = 4.9, unit = NA),
      "eGFR" = list(value = 143, unit = NA)
    ),
    "3 Months After Biopsy" = list(
      "Platelets" = list(value = 356000, unit = "K/µL"),
      "WBC" = list(value = 7000, unit = "K/µL"),
      "ALC" = list(value = 1900, unit = "cells/µL"),
      "IgA" = list(value = 287, unit = "mg/dL"),
      "ANC units" = list(value = 4900, unit = "cells/µL"),
      "Anti ds-DNA" = list(value = 0, unit = NA),
      "ESR" = list(value = 16, unit = "mm/hr"),
      "CRP" = list(value = 0.5, unit = "mg/dl"),
      "C3 complement" = list(value = 86.7, unit = "mg/dL"),
      "C4 complement" = list(value = 28.6, unit = "mg/dL"),
      "Urine Protein" = list(value = 21, unit = "mg/dL"),
      "Urine Creatinine" = list(value = 43, unit = "mg/dL"),
      "UPCR" = list(value = 0.49, unit = NA),
      "Albumin" = list(value = 5, unit = NA),
      "eGFR" = list(value = 140, unit = NA)
    )
  )
)

for (ext_id in names(lab_data)) {
  pid <- pt_map[[ext_id]]
  for (tp in names(lab_data[[ext_id]])) {
    vid <- visits$visit_id[visits$patient_id == pid & visits$visit_type == tp]
    vdate <- as.Date("2024-01-01") + c(-90, 0, 90)[match(tp, timepoints)]
    for (lab_name in names(lab_data[[ext_id]][[tp]])) {
      lab <- lab_data[[ext_id]][[tp]][[lab_name]]
      DBI::dbExecute(con,
        "INSERT INTO labs (patient_id, visit_id, collected_date, lab_name, lab_value, lab_unit) VALUES ($1, $2, $3, $4, $5, $6)",
        params = list(pid, vid, vdate, lab_name, lab$value, lab$unit)
      )
    }
  }
}

# Domain data
cat("Inserting domains...\n")
domain_data <- list(
  PT1 = list(
    "3 Months Before Biopsy" = list(Arthritis = TRUE, Renal = TRUE, Skin = TRUE, `Oral Ulcers` = FALSE, Cardiac = FALSE, Pulm = FALSE, Gastrointestinal = FALSE, Neuro = TRUE),
    "Biopsy" = list(Arthritis = TRUE, Renal = TRUE, Skin = TRUE, `Oral Ulcers` = FALSE, Cardiac = FALSE, Pulm = FALSE, Gastrointestinal = FALSE, Neuro = TRUE),
    "3 Months After Biopsy" = list(Arthritis = TRUE, Renal = FALSE, Skin = TRUE, `Oral Ulcers` = FALSE, Cardiac = FALSE, Pulm = FALSE, Gastrointestinal = FALSE, Neuro = TRUE)
  ),
  PT2 = list(
    "3 Months Before Biopsy" = list(Arthritis = TRUE, Renal = TRUE, Skin = TRUE, `Oral Ulcers` = FALSE, Cardiac = TRUE, Pulm = TRUE, Gastrointestinal = FALSE, Neuro = FALSE),
    "Biopsy" = list(Arthritis = TRUE, Renal = TRUE, Skin = TRUE, `Oral Ulcers` = FALSE, Cardiac = TRUE, Pulm = TRUE, Gastrointestinal = FALSE, Neuro = FALSE),
    "3 Months After Biopsy" = list(Arthritis = TRUE, Renal = TRUE, Skin = TRUE, `Oral Ulcers` = FALSE, Cardiac = TRUE, Pulm = TRUE, Gastrointestinal = FALSE, Neuro = FALSE)
  ),
  PT3 = list(
    "3 Months Before Biopsy" = list(Arthritis = FALSE, Renal = TRUE, Skin = TRUE, `Oral Ulcers` = TRUE, Cardiac = FALSE, Pulm = FALSE, Gastrointestinal = FALSE, Neuro = FALSE),
    "Biopsy" = list(Arthritis = FALSE, Renal = FALSE, Skin = TRUE, `Oral Ulcers` = TRUE, Cardiac = FALSE, Pulm = FALSE, Gastrointestinal = FALSE, Neuro = FALSE),
    "3 Months After Biopsy" = list(Arthritis = FALSE, Renal = FALSE, Skin = TRUE, `Oral Ulcers` = TRUE, Cardiac = FALSE, Pulm = FALSE, Gastrointestinal = FALSE, Neuro = FALSE)
  )
)

for (ext_id in names(domain_data)) {
  pid <- pt_map[[ext_id]]
  for (tp in names(domain_data[[ext_id]])) {
    vid <- visits$visit_id[visits$patient_id == pid & visits$visit_type == tp]
    vdate <- as.Date("2024-01-01") + c(-90, 0, 90)[match(tp, timepoints)]
    for (domain_name in names(domain_data[[ext_id]][[tp]])) {
      active <- domain_data[[ext_id]][[tp]][[domain_name]]
      # Check if ever involved across all timepoints for this patient
      ever <- any(sapply(domain_data[[ext_id]], function(tp_data) tp_data[[domain_name]]))
      DBI::dbExecute(con,
        "INSERT INTO domains (patient_id, visit_id, assessed_date, domain_name, active, ever_involved) VALUES ($1, $2, $3, $4, $5, $6)",
        params = list(pid, vid, vdate, domain_name, active, ever)
      )
    }
  }
}

cat("\n=== Seeding complete! ===\n")
cat("Patients:", DBI::dbGetQuery(con, "SELECT COUNT(*) FROM patients")[[1]], "\n")
cat("Visits:", DBI::dbGetQuery(con, "SELECT COUNT(*) FROM visits")[[1]], "\n")
cat("Labs:", DBI::dbGetQuery(con, "SELECT COUNT(*) FROM labs")[[1]], "\n")
cat("Domains:", DBI::dbGetQuery(con, "SELECT COUNT(*) FROM domains")[[1]], "\n")

DBI::dbDisconnect(con)
cat("Done!\n")
