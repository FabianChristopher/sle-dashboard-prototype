#!/usr/bin/env Rscript

args <- commandArgs(trailingOnly = TRUE)
if (length(args) < 2) {
  stop('Usage: seed_from_excel.R <excel_path> <db_path>')
}

excel_path <- args[[1]]
db_path <- args[[2]]

# TODO: implement Excel ingest and database seeding.
message('Seeding is not implemented yet.')
