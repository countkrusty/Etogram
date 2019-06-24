FROM rocker/shiny:3.5.1

COPY ./packages.R packages.R
RUN Rscript packages.R

EXPOSE 80



