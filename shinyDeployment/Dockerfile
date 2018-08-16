FROM openanalytics/r-base

MAINTAINER Laure Cougnaud "laure.cougnaud@openanalytics.eu"

# system libraries of general use
RUN apt-get update && apt-get install -y \
    sudo \
    libcurl4-gnutls-dev \
    libcairo2-dev \
    libxt-dev \
    libssl-dev \
    libssh2-1-dev \
    libssl1.0.0 \
    texlive-latex-base
    
# basic shiny functionality
RUN R -e "install.packages(c('shiny'), repos='https://cloud.r-project.org/')"

# install dependencies of the app

# glpgStyle, dependency of patientProfilesVis
RUN R -e "install.packages(c('rmarkdown', 'knitr', 'bookdown'), repos = 'https://cloud.r-project.org/')" # its dependencies
COPY ./packages/glpgStyle_0.2.2.tar.gz /root/
RUN R CMD INSTALL /root/glpgStyle_0.2.2.tar.gz
RUN rm /root/glpgStyle_0.2.2.tar.gz

# dependencies of patientProfilesVis from CRAN
RUN R -e "install.packages(c('ggplot2', 'plyr', 'cowplot', 'haven', 'reshape2', 'knitr', 'grid', 'viridisLite', 'dplyr'), repos = 'https://cloud.r-project.org/')"

# install patientProfilesVis
COPY ./packages/patientProfilesVis_0.0-4.tar.gz /root/
RUN R CMD INSTALL /root/patientProfilesVis_0.0-4.tar.gz
RUN rm /root/patientProfilesVis_0.0-4.tar.gz

# dependencies of patientProfilesVisShiny from CRAN
RUN R -e "install.packages(c('shiny', 'plyr'), repos = 'https://cloud.r-project.org/')"

# install patientProfilesVisShiny
COPY ./packages/patientProfilesVisShiny_0.0-1.tar.gz /root/
RUN R CMD INSTALL /root/patientProfilesVisShiny_0.0-1.tar.gz
RUN rm /root/patientProfilesVisShiny_0.0-1.tar.gz

# copy the app to the image
#RUN mkdir /root/apps/patientProfileVis
#COPY euler /root/euler

#COPY Rprofile.site /usr/lib/R/etc/

EXPOSE 3838

CMD ["R", "-e", "patientProfilesVisShiny::runPatientProfilesVisShiny()"]