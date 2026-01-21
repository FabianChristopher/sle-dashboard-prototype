# sle-dashboard

Local run with Docker Compose:

```bash
docker compose -f docker/docker-compose.yml --env-file .env up --build
```

Then open http://localhost:3838

Load mock data (from `app/sle-dashboard/`):

```bash
Rscript scripts/load_mock_data.R
```
