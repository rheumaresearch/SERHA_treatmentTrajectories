#######################
# Analysis main       #
#######################

### Directoris SERHA
.wd <- "C:/Users/dsegarra_ext/Documents/treatmentPathways_serha/"
.dat <- paste0(.wd,"data/")
.res0 <- paste0(.wd,"results/")
.res <- paste0(.res0,"CodiSunburst/")

dir.create(.res,F,T)

setwd(.wd)

### Directoris MOMPRUAA
.wdm <- "C:/Users/dsegarra_ext/Documents/treatmentPathways_mompruaa/"
.datm <- paste0(.wdm,"data/")
.resm0 <- paste0(.wdm,"results/")
.resm <- paste0(.resm0,"CodiSunburstM/")

setwd(.wdm)

### Library
library(dplyr) # manipulació
library(sunburstR)
library(plotly)
library(htmlwidgets)
library(DT)
library(htmltools)
library(stringr)
library(purrr)
library(lubridate)
library(IRanges)
library(shiny)
library(tidyr)








#packages
install.packages("DT")
install.packages("dplyr")
install.packages("sunburstR")
install.packages("htmlwidgets")
install.packages("BiocManager")
BiocManager::install("IRanges")
install.packages("shiny")
