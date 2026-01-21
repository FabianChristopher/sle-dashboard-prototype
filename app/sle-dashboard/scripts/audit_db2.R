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

cat("\n=== PT1 ALL LABS ===\n")
df <- dbGetQuery(con, "SELECT l.lab_name, v.visit_type, l.lab_value FROM labs l JOIN patients p ON p.patient_id=l.patient_id JOIN visits v ON v.visit_id=l.visit_id WHERE p.external_id='PT1' ORDER BY l.lab_name, v.visit_date")
print(df, row.names = FALSE)

cat("\n=== PT2 C3/C4/UPCR ===\n")
df2 <- dbGetQuery(con, "SELECT l.lab_name, v.visit_type, l.lab_value FROM labs l JOIN patients p ON p.patient_id=l.patient_id JOIN visits v ON v.visit_id=l.visit_id WHERE p.external_id='PT2' AND l.lab_name IN ('C3 complement','C4 complement','UPCR') ORDER BY l.lab_name, v.visit_date")
print(df2, row.names = FALSE)

cat("\n=== PT2 DOMAINS ===\n")
df3 <- dbGetQuery(con, "SELECT d.domain_name, v.visit_type, d.active FROM domains d JOIN patients p ON p.patient_id=d.patient_id JOIN visits v ON v.visit_id=d.visit_id WHERE p.external_id='PT2' ORDER BY d.domain_name, v.visit_date")
print(df3, row.names = FALSE)

dbDisconnect(con)
