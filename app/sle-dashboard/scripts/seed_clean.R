#!/usr/bin/env Rscript
# Clean seed script - properly clears and seeds Railway database

library(DBI)
library(RPostgres)

# Use DATABASE_URL when present (hosted), otherwise fall back to DB_* vars (local).
db_url <- Sys.getenv("DATABASE_URL", "")

# Parse URL
parse_db_url <- function(url) {
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
    host = host_and_port[1],
    port = as.integer(host_and_port[2]),
    dbname = host_port[2],
    sslmode = sslmode
  )
}

if (nchar(db_url) > 0) {
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
  con <- do.call(dbConnect, args)
} else {
  con <- dbConnect(
    RPostgres::Postgres(),
    host = Sys.getenv("DB_HOST", "localhost"),
    port = as.integer(Sys.getenv("DB_PORT", "5432")),
    dbname = Sys.getenv("DB_NAME", "sle"),
    user = Sys.getenv("DB_USER", "postgres"),
    password = Sys.getenv("DB_PASSWORD", "postgres")
  )
}

cat("Connected to database!\n")

# Step 1: TRUNCATE all tables to clear duplicates
cat("\n=== STEP 1: Clearing all tables ===\n")
dbExecute(con, "TRUNCATE TABLE labs, domains, medications, visits, patients RESTART IDENTITY CASCADE")
cat("All tables cleared!\n")

# Step 2: Insert patients one by one
cat("\n=== STEP 2: Inserting patients ===\n")
pt1_id <- dbGetQuery(con, "INSERT INTO patients (external_id, first_name, last_name, sex) VALUES ('PT1', 'Patient', 'One', 'F') RETURNING patient_id")$patient_id
cat("Inserted PT1, id =", pt1_id, "\n")

pt2_id <- dbGetQuery(con, "INSERT INTO patients (external_id, first_name, last_name, sex) VALUES ('PT2', 'Patient', 'Two', 'M') RETURNING patient_id")$patient_id
cat("Inserted PT2, id =", pt2_id, "\n")

pt3_id <- dbGetQuery(con, "INSERT INTO patients (external_id, first_name, last_name, sex) VALUES ('PT3', 'Patient', 'Three', 'F') RETURNING patient_id")$patient_id
cat("Inserted PT3, id =", pt3_id, "\n")

# Step 3: Insert visits for each patient
cat("\n=== STEP 3: Inserting visits ===\n")
# PT1 visits
v1_id <- dbGetQuery(con, sprintf("INSERT INTO visits (patient_id, visit_date, visit_type) VALUES (%d, '2023-10-01', 'Initial') RETURNING visit_id", pt1_id))$visit_id
v2_id <- dbGetQuery(con, sprintf("INSERT INTO visits (patient_id, visit_date, visit_type) VALUES (%d, '2024-01-01', 'Follow-up') RETURNING visit_id", pt1_id))$visit_id
v3_id <- dbGetQuery(con, sprintf("INSERT INTO visits (patient_id, visit_date, visit_type) VALUES (%d, '2024-04-01', 'Follow-up') RETURNING visit_id", pt1_id))$visit_id
cat("PT1 visits:", v1_id, v2_id, v3_id, "\n")

# PT2 visits
v4_id <- dbGetQuery(con, sprintf("INSERT INTO visits (patient_id, visit_date, visit_type) VALUES (%d, '2023-10-01', 'Initial') RETURNING visit_id", pt2_id))$visit_id
v5_id <- dbGetQuery(con, sprintf("INSERT INTO visits (patient_id, visit_date, visit_type) VALUES (%d, '2024-01-01', 'Follow-up') RETURNING visit_id", pt2_id))$visit_id
v6_id <- dbGetQuery(con, sprintf("INSERT INTO visits (patient_id, visit_date, visit_type) VALUES (%d, '2024-04-01', 'Follow-up') RETURNING visit_id", pt2_id))$visit_id
cat("PT2 visits:", v4_id, v5_id, v6_id, "\n")

# PT3 visits
v7_id <- dbGetQuery(con, sprintf("INSERT INTO visits (patient_id, visit_date, visit_type) VALUES (%d, '2023-10-01', 'Initial') RETURNING visit_id", pt3_id))$visit_id
v8_id <- dbGetQuery(con, sprintf("INSERT INTO visits (patient_id, visit_date, visit_type) VALUES (%d, '2024-01-01', 'Follow-up') RETURNING visit_id", pt3_id))$visit_id
v9_id <- dbGetQuery(con, sprintf("INSERT INTO visits (patient_id, visit_date, visit_type) VALUES (%d, '2024-04-01', 'Follow-up') RETURNING visit_id", pt3_id))$visit_id
cat("PT3 visits:", v7_id, v8_id, v9_id, "\n")

# Step 4: Insert labs
cat("\n=== STEP 4: Inserting labs ===\n")

# Lab data for each patient/visit
lab_names <- c("Platelets", "WBC", "ALC", "IgA", "ANC units", "Hemoglobin", "C3", "C4", 
               "anti-dsDNA", "ESR", "CRP", "Urine Creatinine", "UPCR", "Albumin", "eGFR")

# PT1 labs - 3 timepoints
pt1_labs <- list(
  list(visit_id=v1_id, date="2023-10-01", vals=c(245000,5200,1100,285,3800,12.1,88,18,45,28,0.8,52,1.2,3.8,95)),
  list(visit_id=v2_id, date="2024-01-01", vals=c(198000,4800,950,310,3200,11.8,72,14,68,35,1.2,48,1.8,3.5,82)),
  list(visit_id=v3_id, date="2024-04-01", vals=c(175000,4200,800,342,2800,11.2,65,12,85,42,1.5,43,0.67,4.6,140))
)

# PT2 labs - 3 timepoints
pt2_labs <- list(
  list(visit_id=v4_id, date="2023-10-01", vals=c(288000,6400,1300,362,4400,13.5,95,22,32,22,0.5,58,0.8,4.2,105)),
  list(visit_id=v5_id, date="2024-01-01", vals=c(265000,5800,1150,385,3900,13.2,82,19,48,28,0.7,55,1.1,4.0,98)),
  list(visit_id=v6_id, date="2024-04-01", vals=c(242000,5200,1000,410,3400,12.8,75,16,62,34,0.9,51,1.4,3.8,92))
)

# PT3 labs - 3 timepoints
pt3_labs <- list(
  list(visit_id=v7_id, date="2023-10-01", vals=c(312000,7100,1450,298,4800,14.2,102,25,28,18,0.4,62,0.5,4.4,115)),
  list(visit_id=v8_id, date="2024-01-01", vals=c(295000,6600,1280,325,4300,13.8,92,21,38,24,0.6,59,0.7,4.2,108)),
  list(visit_id=v9_id, date="2024-04-01", vals=c(278000,6100,1120,348,3900,13.4,85,18,52,30,0.8,55,0.9,4.0,102))
)

units <- c("K/uL","K/uL","cells/uL","mg/dL","cells/uL","g/dL","mg/dL","mg/dL",
           "IU/mL","mm/hr","mg/dL","mg/dL","","g/dL","mL/min")

insert_labs <- function(patient_id, labs_list) {
  for (ldata in labs_list) {
    for (i in seq_along(lab_names)) {
      dbExecute(con, sprintf(
        "INSERT INTO labs (patient_id, visit_id, collected_date, lab_name, lab_value, lab_unit) VALUES (%d, %d, '%s', '%s', %s, '%s')",
        patient_id, ldata$visit_id, ldata$date, lab_names[i], ldata$vals[i], units[i]
      ))
    }
  }
}

insert_labs(pt1_id, pt1_labs)
cat("PT1 labs inserted\n")
insert_labs(pt2_id, pt2_labs)
cat("PT2 labs inserted\n")
insert_labs(pt3_id, pt3_labs)
cat("PT3 labs inserted\n")

# Step 5: Insert domains
cat("\n=== STEP 5: Inserting domains ===\n")

domain_names <- c("Arthritis", "Renal", "Skin", "Oral Ulcers", "Cardiac", "Pulm", "Gastrointestinal", "Neuro")

# PT1 domains
pt1_domains <- list(
  list(visit_id=v1_id, date="2023-10-01", active=c(T,T,T,F,F,F,F,F), ever=c(T,T,T,F,F,F,F,F)),
  list(visit_id=v2_id, date="2024-01-01", active=c(T,T,T,T,F,F,F,F), ever=c(T,T,T,T,F,F,F,F)),
  list(visit_id=v3_id, date="2024-04-01", active=c(F,T,F,F,F,F,F,F), ever=c(T,T,T,T,F,F,F,F))
)

# PT2 domains
pt2_domains <- list(
  list(visit_id=v4_id, date="2023-10-01", active=c(F,T,T,F,T,T,F,F), ever=c(F,T,T,F,T,T,F,F)),
  list(visit_id=v5_id, date="2024-01-01", active=c(F,T,T,F,T,T,F,F), ever=c(F,T,T,F,T,T,F,F)),
  list(visit_id=v6_id, date="2024-04-01", active=c(F,T,T,F,T,T,F,F), ever=c(F,T,T,F,T,T,F,F))
)

# PT3 domains
pt3_domains <- list(
  list(visit_id=v7_id, date="2023-10-01", active=c(F,T,F,F,F,F,F,T), ever=c(F,T,F,F,F,F,F,T)),
  list(visit_id=v8_id, date="2024-01-01", active=c(F,T,F,F,F,F,F,T), ever=c(F,T,F,F,F,F,F,T)),
  list(visit_id=v9_id, date="2024-04-01", active=c(F,T,F,F,F,F,F,F), ever=c(F,T,F,F,F,F,F,T))
)

insert_domains <- function(patient_id, domains_list) {
  for (ddata in domains_list) {
    for (i in seq_along(domain_names)) {
      dbExecute(con, sprintf(
        "INSERT INTO domains (patient_id, visit_id, assessed_date, domain_name, active, ever_involved) VALUES (%d, %d, '%s', '%s', %s, %s)",
        patient_id, ddata$visit_id, ddata$date, domain_names[i], 
        if(ddata$active[i]) "true" else "false",
        if(ddata$ever[i]) "true" else "false"
      ))
    }
  }
}

insert_domains(pt1_id, pt1_domains)
cat("PT1 domains inserted\n")
insert_domains(pt2_id, pt2_domains)
cat("PT2 domains inserted\n")
insert_domains(pt3_id, pt3_domains)
cat("PT3 domains inserted\n")

# Final verification
cat("\n=== VERIFICATION ===\n")
counts <- dbGetQuery(con, "
  SELECT 
    (SELECT COUNT(*) FROM patients) as patients,
    (SELECT COUNT(*) FROM visits) as visits,
    (SELECT COUNT(*) FROM labs) as labs,
    (SELECT COUNT(*) FROM domains) as domains
")
cat("Patients:", counts$patients, "\n")
cat("Visits:", counts$visits, "\n")
cat("Labs:", counts$labs, "\n")
cat("Domains:", counts$domains, "\n")

dbDisconnect(con)
cat("\nDone! Database properly seeded.\n")
