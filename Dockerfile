## Start with the latest tidyvere (verse) image - once development is complete
## needs to be version locked with tags
## verse has pdf related tools built in (i.e latex)
FROM rocker/verse:latest

## Copy files to working directory of server
ADD . /home/rstudio/RtD3

## Set working directory to be this folder
WORKDIR /home/rstudio/RtD3

## Install missing packages.
RUN Rscript -e "devtools::install_dev_deps()"
