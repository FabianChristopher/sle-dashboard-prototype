FROM rocker/shiny:4.3.2

# Install system dependencies for RPostgres
RUN apt-get update && apt-get install -y \
    libpq-dev \
    && rm -rf /var/lib/apt/lists/*

# Install R packages
RUN R -e "install.packages(c('DBI', 'RPostgres', 'dotenv'), repos='https://cloud.r-project.org/')"

# Remove default shiny-server sample apps
RUN rm -rf /srv/shiny-server/*

# Copy app files directly to shiny-server root
COPY app/ /srv/shiny-server/
COPY assets/ /srv/shiny-server/assets/

# Create a startup script that uses the PORT env var from Railway
RUN echo '#!/bin/bash\n\
PORT=${PORT:-3838}\n\
echo "run_as shiny;\n\
server {\n\
  listen $PORT;\n\
  location / {\n\
    site_dir /srv/shiny-server;\n\
    log_dir /var/log/shiny-server;\n\
    directory_index off;\n\
  }\n\
}" > /etc/shiny-server/shiny-server.conf\n\
exec /usr/bin/shiny-server' > /start.sh && chmod +x /start.sh

# Expose default port
EXPOSE 3838

# Start with the script that configures PORT dynamically
CMD ["/start.sh"]
