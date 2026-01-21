FROM rocker/shiny:4.3.2

WORKDIR /srv/shiny-server/sle-dashboard

# Copy app sources and static assets.
COPY app/ /srv/shiny-server/sle-dashboard/
COPY assets/ /srv/shiny-server/sle-dashboard/assets/

EXPOSE 3838
