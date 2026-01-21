#!/usr/bin/env Rscript
suppressPackageStartupMessages({
  library(DBI)
  library(RPostgres)
  library(dotenv)
})

dotenv::load_dot_env(file = ".env")
con <- dbConnect(
  Postgres(),
  host = Sys.getenv("DB_HOST"),
  port = as.integer(Sys.getenv("DB_PORT")),
  dbname = Sys.getenv("DB_NAME"),
  user = Sys.getenv("DB_USER"),
  password = Sys.getenv("DB_PASSWORD")
)

cat("\n=== PATIENTS ===\n")
print(dbGetQuery(con, "SELECT * FROM patients ORDER BY external_id"))

cat("\n=== VISITS ===\n")
print(dbGetQuery(con, "SELECT p.external_id, v.visit_type, v.visit_date FROM visits v JOIN patients p ON p.patient_id=v.patient_id ORDER BY p.external_id, v.visit_date"))

cat("\n=== LAB NAMES (distinct) ===\n")
print(dbGetQuery(con, "SELECT DISTINCT lab_name, lab_unit FROM labs ORDER BY lab_name"))

cat("\n=== DOMAIN NAMES (distinct) ===\n")
print(dbGetQuery(con, "SELECT DISTINCT domain_name FROM domains ORDER BY domain_name"))

cat("\n=== LAB COUNT per patient ===\n")
print(dbGetQuery(con, "SELECT p.external_id, COUNT(*) as lab_count FROM labs l JOIN patients p ON p.patient_id=l.patient_id GROUP BY p.external_id ORDER BY p.external_id"))

cat("\n=== DOMAIN COUNT per patient ===\n")
print(dbGetQuery(con, "SELECT p.external_id, COUNT(*) as domain_count FROM domains d JOIN patients p ON p.patient_id=d.patient_id GROUP BY p.external_id ORDER BY p.external_id"))

cat("\n=== PT1 LABS SAMPLE (first 20) ===\n")
print(dbGetQuery(con, "SELECT l.lab_name, l.lab_value, l.lab_unit, v.visit_type FROM labs l JOIN patients p ON p.patient_id=l.patient_id JOIN visits v ON v.visit_id=l.visit_id WHERE p.external_id='PT1' ORDER BY l.lab_name, v.visit_date LIMIT 20"))

cat("\n=== PT1 DOMAINS ===\n")
print(dbGetQuery(con, "SELECT d.domain_name, d.active, v.visit_type FROM domains d JOIN patients p ON p.patient_id=d.patient_id JOIN visits v ON v.visit_id=d.visit_id WHERE p.external_id='PT1' ORDER BY d.domain_name, v.visit_date"))

dbDisconnect(con)
