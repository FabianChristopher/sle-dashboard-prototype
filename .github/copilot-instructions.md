# Copilot instructions for this repo

At every step, adjust workload to avoid "Request filed:413 Request entity too large" errors.

## Big picture
- This is an R Shiny SLE dashboard prototype with a Postgres backend. The runnable app lives under app/sle-dashboard/ (not the repo root).
- Data flow today: Excel mock data ➜ scripts/load_mock_data.R ➜ Postgres schema in app/sle-dashboard/db/schema.sql ➜ Shiny reads via app/sle-dashboard/app/db.R.
- app/sle-dashboard/app/app.R is a DB connectivity check UI; app/sle-dashboard/app/ui.R and app/sle-dashboard/app/server.R are the intended UI/server split and are currently scaffolded.

## Key workflows
- Local run (DB + app): see app/sle-dashboard/README.md. It uses docker compose with app/sle-dashboard/docker/docker-compose.yml and environment from app/sle-dashboard/.env, then opens http://localhost:3838.
- Load mock data from the Excel sheet: run Rscript scripts/load_mock_data.R from app/sle-dashboard/ (script expects ../../renal bx pilot-mock patients.xlsx).

## Project-specific conventions
- Keep Shiny code under app/sle-dashboard/app/ and DB helpers in app/sle-dashboard/app/db.R (DBI + RPostgres with env vars).
- Schema changes go in app/sle-dashboard/db/schema.sql; tables are patients, visits, labs, domains, medications and are keyed by patient_id/visit_id.
- The Excel workbook sheet names used by the loader are patient 1, patient 2, patient 3 mapped to external IDs PT1–PT3.

## Integration points & dependencies
- Postgres is provisioned via docker compose (postgres:16) and initialized by app/sle-dashboard/db/schema.sql.
- Adminer is exposed on port 8080 for DB inspection via docker compose.
- The app reads DB credentials from app/sle-dashboard/.env (DB_HOST, DB_PORT, DB_NAME, DB_USER, DB_PASSWORD).

## UI/UX reference assets
- UI references are the image files at repo root (e.g., 155482A6-A195-4BE7-AF0D-A31046F52179.jpeg) and the text brief in chatgpt.txt and text.txt.
- The mock data source is renal bx pilot-mock patients.xlsx in the repo root.

## When adding new features
- Prefer updating app/sle-dashboard/app/ui.R and app/sle-dashboard/app/server.R for new UI/server logic, then wire into app/sle-dashboard/app/app.R if you consolidate.
- If you change the Excel layout or sheet names, update scripts/load_mock_data.R accordingly (the loader parses lab timepoints and Y/N domains).
