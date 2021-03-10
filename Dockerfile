# pull from https://hub.docker.com/u/rocker
FROM rocker/shiny-verse:3.6.0


RUN apt-get update && apt-get install -y \
	sudo \
	pandoc \
	pandoc-citeproc \
	libcurl4-gnutls-dev \
    libcairo2-dev \
    libxt-dev \
    libssl-dev \
    libssh2-1-dev \
	nano

# some of these are needed for the r sf package to run
RUN apt-get update && apt-get install -y \
	libudunits2-dev \
	libgdal-dev \
	libgeos-dev \
	libproj-dev 


# install other packages
RUN R -e "install.packages('shinydashboard', repos='http://cran.rstudio.com/')"
RUN R -e "install.packages('htmlwidgets', repos='http://cran.rstudio.com/')"
RUN R -e "install.packages('rhandsontable', repos='http://cran.rstudio.com/')"
RUN R -e "install.packages('rgdal', repos='http://cran.rstudio.com/')"
RUN R -e "install.packages('sp', repos='http://cran.rstudio.com/')"
RUN R -e "install.packages('maps', repos='http://cran.rstudio.com/')"
RUN R -e "install.packages('maptools', repos='http://cran.rstudio.com/')"
RUN R -e "install.packages('grid', repos='http://cran.rstudio.com/')"
RUN R -e "install.packages('gtable', repos='http://cran.rstudio.com/')"
RUN R -e "install.packages('gridExtra', repos='http://cran.rstudio.com/')"
RUN R -e "install.packages('shinyjs', repos='http://cran.rstudio.com/')"
RUN R -e "install.packages('leaflet', repos='http://cran.rstudio.com/')"
RUN R -e "install.packages('imager', repos='http://cran.rstudio.com/')"
RUN R -e "install.packages('shinyEffects', repos='http://cran.rstudio.com/')"
RUN R -e "install.packages('geosphere', repos='http://cran.rstudio.com/')"
RUN R -e "install.packages('lattice', repos='http://cran.rstudio.com/')"

# copy content to run app
COPY InputSpreadsheets /srv/shiny-server/InputSpreadsheets
COPY Scenarios /srv/shiny-server/Scenarios

# Make shiny the owner of the writable directories
RUN sudo mkdir /srv/shiny-server/www
RUN sudo chown shiny:shiny /srv/shiny-server/www
RUN sudo chown shiny:shiny /srv/shiny-server/Scenarios
RUN sudo chown shiny:shiny /srv/shiny-server/InputSpreadsheets

# copy more
COPY README.md /srv/shiny-server/README.md
COPY InputShapefiles /srv/shiny-server/InputShapefiles
COPY TempShapefiles /srv/shiny-server/TempShapefiles
COPY FunctionsEtc /srv/shiny-server/FunctionsEtc
COPY Inputs /srv/shiny-server/Inputs
COPY R /srv/shiny-server/R
COPY ui.R /srv/shiny-server/ui.R
COPY server.R /srv/shiny-server/server.R
COPY function_DecisionSupportTool_V3.0.2.R /srv/shiny-server/function_DecisionSupportTool_V3.0.2.R



