# for initial tests, set wd to /ui/
# remove this code later
suppressWarnings({if(!require(poissontris)) {  # TODO change
        cat("Loading via devtools...")
        library(devtools)
        load_all("./../../../poissontris")
      } else {
        library(poissontris)
        cat("Loading the package...")
      }})

library(oaColors)
library(oaPlots)
library(htmltools)
library(svglite)
library(pryr)

if(Sys.info()["sysname"] == "Windows")
  memory.limit(size = 7000)
