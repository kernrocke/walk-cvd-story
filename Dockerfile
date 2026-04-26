FROM rocker/shiny:latest

# Install system dependencies
RUN apt-get update && apt-get install -y \
    libcurl4-openssl-dev \
    libssl-dev \
    libxml2-dev \
    libv8-dev \
    && rm -rf /var/lib/apt/lists/*

# Install required R packages
RUN R -e "install.packages(c('shinydashboard','plotly','dplyr','DT','htmltools'), \
    repos='https://cran.rstudio.com/', dependencies=TRUE)"

# Copy app files to the Shiny server app directory
COPY app.R /srv/shiny-server/app/app.R
COPY www/ /srv/shiny-server/app/www/

# Expose port 7860 (required by Hugging Face Spaces)
EXPOSE 7860

# Configure Shiny to run on port 7860
RUN echo 'run_as shiny;\n\
server {\n\
  listen 7860;\n\
  location / {\n\
    site_dir /srv/shiny-server/app;\n\
    log_dir /var/log/shiny-server;\n\
    directory_index off;\n\
  }\n\
}' > /etc/shiny-server/shiny-server.conf

CMD ["/usr/bin/shiny-server"]
