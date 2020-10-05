# get shiny serves plus tidyverse packages image
FROM rocker/shiny:latest


# install R packages required 
# (change it dependeing on the packages you need)
RUN R -e "install.packages('shinythemes', repos='http://cran.rstudio.com/')"
RUN R -e "install.packages('ggplot2', repos='http://cran.rstudio.com/')"
RUN R -e "install.packages('blandr', repos='http://cran.rstudio.com/')"
RUN R -e "install.packages('rhandsontable', repos='http://cran.rstudio.com/')"
RUN R -e "install.packages('mcr', repos='http://cran.rstudio.com/')"

# copy the app to the image
COPY crossvalidation.Rproj /srv/shiny-server/
COPY app.R /srv/shiny-server/
COPY report.Rmd /srv/shiny-server/

# select port
EXPOSE 3838

# allow permission
RUN sudo chown -R shiny:shiny /srv/shiny-server

USER shiny

# run app
CMD ["/usr/bin/shiny-server"]