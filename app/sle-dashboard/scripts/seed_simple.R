#!/usr/bin/env Rscript
library(DBI)
library(RPostgres)

# Connect using DATABASE_URL (preferred for hosted envs), falling back to DB_* vars (local dev).
db_url <- Sys.getenv("DATABASE_URL", "")

parse_db_url <- function(url) {
  # Format: postgresql://user:password@host:port/dbname
  url <- sub("^postgres(ql)?://", "", url)
  parts <- strsplit(url, "@")[[1]]
  user_pass <- strsplit(parts[1], ":")[[1]]
  host_port_db <- parts[2]
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

if (nchar(db_url) > 0) {
  params <- parse_db_url(db_url)
  con <- DBI::dbConnect(
    RPostgres::Postgres(),
    host = params$host,
    port = params$port,
    dbname = params$dbname,
    user = params$user,
    password = params$password
  )
} else {
  con <- DBI::dbConnect(
    RPostgres::Postgres(),
    host = Sys.getenv("DB_HOST", "localhost"),
    port = as.integer(Sys.getenv("DB_PORT", "5432")),
    dbname = Sys.getenv("DB_NAME", "sle"),
    user = Sys.getenv("DB_USER", "postgres"),
    password = Sys.getenv("DB_PASSWORD", "postgres")
  )
}

cat("Connected to database!\n")

# Create tables
DBI::dbExecute(con, "DROP TABLE IF EXISTS medications CASCADE")
DBI::dbExecute(con, "DROP TABLE IF EXISTS domains CASCADE")
DBI::dbExecute(con, "DROP TABLE IF EXISTS labs CASCADE")
DBI::dbExecute(con, "DROP TABLE IF EXISTS visits CASCADE")
DBI::dbExecute(con, "DROP TABLE IF EXISTS patients CASCADE")

DBI::dbExecute(con, "CREATE TABLE patients (patient_id SERIAL PRIMARY KEY, external_id TEXT UNIQUE NOT NULL, first_name TEXT, last_name TEXT, sex TEXT, diagnosis_date DATE)")
DBI::dbExecute(con, "CREATE TABLE visits (visit_id SERIAL PRIMARY KEY, patient_id INTEGER REFERENCES patients(patient_id), visit_date DATE, visit_type TEXT, notes TEXT)")
DBI::dbExecute(con, "CREATE TABLE labs (lab_id SERIAL PRIMARY KEY, patient_id INTEGER REFERENCES patients(patient_id), visit_id INTEGER REFERENCES visits(visit_id), collected_date DATE, lab_name TEXT, lab_value NUMERIC, lab_unit TEXT, reference_range_low NUMERIC, reference_range_high NUMERIC, abnormal_flag BOOLEAN)")
DBI::dbExecute(con, "CREATE TABLE domains (domain_id SERIAL PRIMARY KEY, patient_id INTEGER REFERENCES patients(patient_id), visit_id INTEGER REFERENCES visits(visit_id), assessed_date DATE, domain_name TEXT, domain_score NUMERIC, active BOOLEAN, ever_involved BOOLEAN)")
DBI::dbExecute(con, "CREATE TABLE medications (medication_id SERIAL PRIMARY KEY, patient_id INTEGER REFERENCES patients(patient_id), medication_name TEXT, category TEXT, dose TEXT, route TEXT, frequency TEXT, start_date DATE, end_date DATE, current BOOLEAN DEFAULT TRUE, stop_reason TEXT, indication TEXT)")

cat("Tables created!\n")

# Insert patients
DBI::dbExecute(con, "INSERT INTO patients (external_id) VALUES ('PT1'), ('PT2'), ('PT3')")
patients <- DBI::dbGetQuery(con, "SELECT patient_id, external_id FROM patients")
cat("Patients inserted:", nrow(patients), "\n")

# Insert visits
for (i in 1:nrow(patients)) {
  pid <- patients$patient_id[i]
  DBI::dbExecute(con, sprintf("INSERT INTO visits (patient_id, visit_date, visit_type) VALUES (%d, '2023-10-01', '3 Months Before Biopsy')", pid))
  DBI::dbExecute(con, sprintf("INSERT INTO visits (patient_id, visit_date, visit_type) VALUES (%d, '2024-01-01', 'Biopsy')", pid))
  DBI::dbExecute(con, sprintf("INSERT INTO visits (patient_id, visit_date, visit_type) VALUES (%d, '2024-04-01', '3 Months After Biopsy')", pid))
}
visits <- DBI::dbGetQuery(con, "SELECT * FROM visits")
cat("Visits inserted:", nrow(visits), "\n")

# Lab data
labs_data <- data.frame(
  lab_name = c("Platelets", "WBC", "ALC", "IgA", "ANC units", "Anti ds-DNA", "ESR", "CRP", "C3 complement", "C4 complement", "Urine Protein", "Urine Creatinine", "UPCR", "Albumin", "eGFR"),
  unit = c("K/uL", "K/uL", "cells/uL", "mg/dL", "cells/uL", NA, "mm/hr", "mg/dl", "mg/dL", "mg/dL", "mg/dL", "mg/dL", NA, NA, NA)
)

# PT1 data
pt1_labs <- list(
  "3 Months Before Biopsy" = c(288000, 6400, 1300, 362, 4400, 4, 26, 0.5, 70.8, 14.7, 36, 38, 0.95, 5.2, 132),
  "Biopsy" = c(232000, 6900, 1300, 340, 4700, 2, 20, 0.5, 84.1, 33.5, 28, 40, 0.7, 4.9, 143),
  "3 Months After Biopsy" = c(256000, 6600, 1000, 350, 4900, 4, 16, 0.5, 84.7, 28.6, 29, 43, 0.67, 4.6, 140)
)

pt2_labs <- list(
  "3 Months Before Biopsy" = c(288000, 6400, 1300, 362, 4400, 4, 26, 0.5, 70.8, 12.4, 102, 49, 2.1, 3.3, 80),
  "Biopsy" = c(200000, 6000, 1000, 413, 3900, 4, 42, 1, 64.3, 10.1, 110, 50, 2.2, 3.1, 74),
  "3 Months After Biopsy" = c(180000, 5200, 800, 500, 3400, 4, 54, 1, 59.2, 11.2, 120, 61, 2, 3.5, 78)
)

pt3_labs <- list(
  "3 Months Before Biopsy" = c(288000, 6400, 1300, 223, 4400, 4, 14, 0.5, 84.3, 18.3, 30, 48, 0.63, 4.8, 132),
  "Biopsy" = c(300000, 6900, 1500, 246, 4700, 2, 20, 0.5, 88.9, 24.7, 24, 40, 0.57, 4.9, 143),
  "3 Months After Biopsy" = c(356000, 7000, 1900, 287, 4900, 0, 16, 0.5, 86.7, 28.6, 21, 43, 0.49, 5, 140)
)

all_labs <- list(PT1 = pt1_labs, PT2 = pt2_labs, PT3 = pt3_labs)

for (ext_id in names(all_labs)) {
  pid <- patients$patient_id[patients$external_id == ext_id]
  pt_visits <- visits[visits$patient_id == pid, ]
  
  for (tp in names(all_labs[[ext_id]])) {
    vid <- pt_visits$visit_id[pt_visits$visit_type == tp]
    vdate <- pt_visits$visit_date[pt_visits$visit_type == tp]
    values <- all_labs[[ext_id]][[tp]]
    
    for (j in 1:nrow(labs_data)) {
      unit_val <- if (is.na(labs_data$unit[j])) "NULL" else sprintf("'%s'", labs_data$unit[j])
      sql <- sprintf("INSERT INTO labs (patient_id, visit_id, collected_date, lab_name, lab_value, lab_unit) VALUES (%d, %d, '%s', '%s', %s, %s)",
        pid, vid, vdate, labs_data$lab_name[j], values[j], unit_val)
      DBI::dbExecute(con, sql)
    }
  }
}
cat("Labs inserted!\n")

# Domain data
domains <- c("Arthritis", "Renal", "Skin", "Oral Ulcers", "Cardiac", "Pulm", "Gastrointestinal", "Neuro")

pt1_domains <- list(
  "3 Months Before Biopsy" = c(TRUE, TRUE, TRUE, FALSE, FALSE, FALSE, FALSE, TRUE),
  "Biopsy" = c(TRUE, TRUE, TRUE, FALSE, FALSE, FALSE, FALSE, TRUE),
  "3 Months After Biopsy" = c(TRUE, FALSE, TRUE, FALSE, FALSE, FALSE, FALSE, TRUE)
)

pt2_domains <- list(
  "3 Months Before Biopsy" = c(TRUE, TRUE, TRUE, FALSE, TRUE, TRUE, FALSE, FALSE),
  "Biopsy" = c(TRUE, TRUE, TRUE, FALSE, TRUE, TRUE, FALSE, FALSE),
  "3 Months After Biopsy" = c(TRUE, TRUE, TRUE, FALSE, TRUE, TRUE, FALSE, FALSE)
)

pt3_domains <- list(
  "3 Months Before Biopsy" = c(FALSE, TRUE, TRUE, TRUE, FALSE, FALSE, FALSE, FALSE),
  "Biopsy" = c(FALSE, FALSE, TRUE, TRUE, FALSE, FALSE, FALSE, FALSE),
  "3 Months After Biopsy" = c(FALSE, FALSE, TRUE, TRUE, FALSE, FALSE, FALSE, FALSE)
)

all_domains <- list(PT1 = pt1_domains, PT2 = pt2_domains, PT3 = pt3_domains)

for (ext_id in names(all_domains)) {
  pid <- patients$patient_id[patients$external_id == ext_id]
  pt_visits <- visits[visits$patient_id == pid, ]
  
  for (tp in names(all_domains[[ext_id]])) {
    vid <- pt_visits$visit_id[pt_visits$visit_type == tp]
    vdate <- pt_visits$visit_date[pt_visits$visit_type == tp]
    active_vals <- all_domains[[ext_id]][[tp]]
    
    for (j in seq_along(domains)) {
      ever <- any(sapply(all_domains[[ext_id]], function(x) x[j]))
      sql <- sprintf("INSERT INTO domains (patient_id, visit_id, assessed_date, domain_name, active, ever_involved) VALUES (%d, %d, '%s', '%s', %s, %s)",
        pid, vid, vdate, domains[j], if(active_vals[j]) "TRUE" else "FALSE", if(ever) "TRUE" else "FALSE")
      DBI::dbExecute(con, sql)
    }
  }
}
cat("Domains inserted!\n")

# Verify
cat("\n=== Summary ===\n")
cat("Patients:", DBI::dbGetQuery(con, "SELECT COUNT(*) FROM patients")[[1]], "\n")
cat("Visits:", DBI::dbGetQuery(con, "SELECT COUNT(*) FROM visits")[[1]], "\n")
cat("Labs:", DBI::dbGetQuery(con, "SELECT COUNT(*) FROM labs")[[1]], "\n")
cat("Domains:", DBI::dbGetQuery(con, "SELECT COUNT(*) FROM domains")[[1]], "\n")

DBI::dbDisconnect(con)
cat("\nDone! Database seeded successfully!\n")
