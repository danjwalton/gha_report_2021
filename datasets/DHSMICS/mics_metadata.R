required.packages <- c("data.table", "httr", "jsonlite")
lapply(required.packages, require, character.only=T)

setwd("G:/My Drive/Work/GitHub/gha_report_2021")

surveys <- data.table(fromJSON("datasets/DHSMICS/mics_surveys.json"))

surveys <- surveys[dataset.status == "Available"]
surveys$report_url <- data.table(lapply(surveys$reports, function(x) x$addresses[[1]]$url[[1]]))
surveys$reports <- NULL

download.name <- paste0("mics_metadata_", format(Sys.time(), "%Y%m%d"), ".csv")

fwrite(surveys, paste0("datasets/DHSMICS/", download.name))
