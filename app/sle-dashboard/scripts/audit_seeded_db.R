#!/usr/bin/env Rscript

suppressPackageStartupMessages({
  library(DBI)
  library(RPostgres)
})

parse_db_url <- function(url) {
  url <- sub("^postgres(ql)?://", "", url)

  query <- ""
  if (grepl("?", url, fixed = TRUE)) {
    parts_q <- strsplit(url, "?", fixed = TRUE)[[1]]
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

get_con <- function() {
  db_url <- Sys.getenv("DATABASE_URL", "")
  if (nchar(db_url) == 0) stop("DATABASE_URL is not set")

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
  do.call(DBI::dbConnect, args)
}

count_tbl <- function(con, tbl) {
  as.numeric(DBI::dbGetQuery(con, paste0("SELECT COUNT(*)::bigint AS n FROM ", tbl))$n[1])
}

fail_if <- function(condition, msg) {
  if (isTRUE(condition)) stop(msg, call. = FALSE)
}

con <- get_con()
on.exit(try(DBI::dbDisconnect(con), silent = TRUE), add = TRUE)

cat("\n=== DB Audit: Seeded Data ===\n")

# Expectations based on scripts/seed_railway.R
expected_patients <- c("PT1", "PT2", "PT3")
expected_timepoints <- c("3 Months Before Biopsy", "Biopsy", "3 Months After Biopsy")
expected_labs <- c(
  "Platelets", "WBC", "ALC", "IgA", "ANC units", "Anti ds-DNA", "ESR", "CRP",
  "C3 complement", "C4 complement", "Urine Protein", "Urine Creatinine", "UPCR",
  "Albumin", "eGFR"
)
expected_domains <- c("Arthritis", "Renal", "Skin", "Oral Ulcers", "Cardiac", "Pulm", "Gastrointestinal", "Neuro")

# 1) Basic table existence
required_tables <- c("patients", "visits", "labs", "domains", "medications")
found_tables <- DBI::dbGetQuery(con, "SELECT tablename FROM pg_tables WHERE schemaname='public'")$tablename
missing_tables <- setdiff(required_tables, found_tables)
fail_if(length(missing_tables) > 0, paste0("Missing tables: ", paste(missing_tables, collapse = ", ")))

# 2) Counts
counts <- data.frame(
  table = required_tables,
  n = sapply(required_tables, function(t) count_tbl(con, t))
)
print(counts, row.names = FALSE)

fail_if(counts$n[counts$table == "patients"] != 3, "Expected 3 patients")
fail_if(counts$n[counts$table == "visits"] != 9, "Expected 9 visits (3 per patient)")
fail_if(counts$n[counts$table == "labs"] != 135, "Expected 135 labs (15 per visit x 9 visits)")
fail_if(counts$n[counts$table == "domains"] != 72, "Expected 72 domains (8 per visit x 9 visits)")

# 3) Patients: external IDs and uniqueness
pt_dupes <- DBI::dbGetQuery(con, "SELECT external_id, COUNT(*) AS n FROM patients GROUP BY external_id HAVING COUNT(*) > 1")
fail_if(nrow(pt_dupes) > 0, "Duplicate patients.external_id found")

pt_ids <- DBI::dbGetQuery(con, "SELECT external_id FROM patients ORDER BY external_id")$external_id
fail_if(!identical(pt_ids, expected_patients), paste0("Unexpected patient external_ids: ", paste(pt_ids, collapse = ", ")))

# 4) Visits: expected timepoints per patient and no duplicates
visit_dupes <- DBI::dbGetQuery(con, "
  SELECT p.external_id, v.visit_type, COUNT(*) AS n
  FROM visits v
  JOIN patients p ON p.patient_id = v.patient_id
  GROUP BY p.external_id, v.visit_type
  HAVING COUNT(*) > 1
")
fail_if(nrow(visit_dupes) > 0, "Duplicate visits found for (patient, visit_type)")

visit_missing <- DBI::dbGetQuery(con, "
  WITH expected AS (
    SELECT p.patient_id, p.external_id, tp.visit_type
    FROM patients p
    CROSS JOIN (VALUES ('3 Months Before Biopsy'), ('Biopsy'), ('3 Months After Biopsy')) AS tp(visit_type)
  )
  SELECT e.external_id, e.visit_type
  FROM expected e
  LEFT JOIN visits v ON v.patient_id = e.patient_id AND v.visit_type = e.visit_type
  WHERE v.visit_id IS NULL
  ORDER BY e.external_id, e.visit_type
")
fail_if(nrow(visit_missing) > 0, "Missing required visits for one or more patients")

# 5) Labs: duplicates, missing expected labs per visit, and referential consistency
lab_dupes <- DBI::dbGetQuery(con, "
  SELECT l.patient_id, l.visit_id, l.lab_name, COUNT(*) AS n
  FROM labs l
  GROUP BY l.patient_id, l.visit_id, l.lab_name
  HAVING COUNT(*) > 1
")
fail_if(nrow(lab_dupes) > 0, "Duplicate labs found for (patient_id, visit_id, lab_name)")

lab_bad_fk <- DBI::dbGetQuery(con, "
  SELECT COUNT(*)::bigint AS n
  FROM labs l
  LEFT JOIN visits v ON v.visit_id = l.visit_id
  WHERE l.visit_id IS NULL OR v.visit_id IS NULL
")$n[1]
fail_if(lab_bad_fk > 0, "Labs contain NULL/invalid visit_id")

lab_mismatch_patient <- DBI::dbGetQuery(con, "
  SELECT COUNT(*)::bigint AS n
  FROM labs l
  JOIN visits v ON v.visit_id = l.visit_id
  WHERE l.patient_id <> v.patient_id
")$n[1]
fail_if(lab_mismatch_patient > 0, "Labs.patient_id does not match Visits.patient_id for same visit_id")

lab_names_in_db <- DBI::dbGetQuery(con, "SELECT DISTINCT lab_name FROM labs ORDER BY lab_name")$lab_name
missing_lab_names <- setdiff(expected_labs, lab_names_in_db)
extra_lab_names <- setdiff(lab_names_in_db, expected_labs)
fail_if(length(missing_lab_names) > 0, paste0("Missing expected lab names: ", paste(missing_lab_names, collapse = ", ")))
fail_if(length(extra_lab_names) > 0, paste0("Unexpected lab names present: ", paste(extra_lab_names, collapse = ", ")))

labs_per_visit <- DBI::dbGetQuery(con, "
  SELECT p.external_id, v.visit_type, COUNT(*)::bigint AS n
  FROM labs l
  JOIN visits v ON v.visit_id = l.visit_id
  JOIN patients p ON p.patient_id = v.patient_id
  GROUP BY p.external_id, v.visit_type
  ORDER BY p.external_id, v.visit_type
")
fail_if(any(labs_per_visit$n != length(expected_labs)), "Not all visits have 15 labs")

# 6) Domains: duplicates, missing expected domains per visit, and referential consistency
domain_dupes <- DBI::dbGetQuery(con, "
  SELECT d.patient_id, d.visit_id, d.domain_name, COUNT(*) AS n
  FROM domains d
  GROUP BY d.patient_id, d.visit_id, d.domain_name
  HAVING COUNT(*) > 1
")
fail_if(nrow(domain_dupes) > 0, "Duplicate domains found for (patient_id, visit_id, domain_name)")

domain_bad_fk <- DBI::dbGetQuery(con, "
  SELECT COUNT(*)::bigint AS n
  FROM domains d
  LEFT JOIN visits v ON v.visit_id = d.visit_id
  WHERE d.visit_id IS NULL OR v.visit_id IS NULL
")$n[1]
fail_if(domain_bad_fk > 0, "Domains contain NULL/invalid visit_id")

domain_mismatch_patient <- DBI::dbGetQuery(con, "
  SELECT COUNT(*)::bigint AS n
  FROM domains d
  JOIN visits v ON v.visit_id = d.visit_id
  WHERE d.patient_id <> v.patient_id
")$n[1]
fail_if(domain_mismatch_patient > 0, "Domains.patient_id does not match Visits.patient_id for same visit_id")

domain_names_in_db <- DBI::dbGetQuery(con, "SELECT DISTINCT domain_name FROM domains ORDER BY domain_name")$domain_name
missing_domain_names <- setdiff(expected_domains, domain_names_in_db)
extra_domain_names <- setdiff(domain_names_in_db, expected_domains)
fail_if(length(missing_domain_names) > 0, paste0("Missing expected domain names: ", paste(missing_domain_names, collapse = ", ")))
fail_if(length(extra_domain_names) > 0, paste0("Unexpected domain names present: ", paste(extra_domain_names, collapse = ", ")))

domains_per_visit <- DBI::dbGetQuery(con, "
  SELECT p.external_id, v.visit_type, COUNT(*)::bigint AS n
  FROM domains d
  JOIN visits v ON v.visit_id = d.visit_id
  JOIN patients p ON p.patient_id = v.patient_id
  GROUP BY p.external_id, v.visit_type
  ORDER BY p.external_id, v.visit_type
")
fail_if(any(domains_per_visit$n != length(expected_domains)), "Not all visits have 8 domains")

# 7) Medications (seed_railway does not insert any)
fail_if(count_tbl(con, "medications") != 0, "Expected medications to be empty after seed_railway")

cat("\nAudit result: OK (no missing/duplicate/invalid records found)\n")
