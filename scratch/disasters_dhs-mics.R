required.packages <- c("data.table")
lapply(required.packages, require, character.only=T)

setwd("G:/My Drive/Work/GitHub/gha_report_2021")

isos <- fread("datasets/Countrynames/isos.csv", encoding = "UTF-8")
catnat <- fread("datasets/CatNat/catnat_raw.csv", encoding = "UTF-8")
dhs_surveys <- fread(tail(list.files("datasets/DHSMICS/", "dhs_metadata.*[.]csv", full.names = T),1), encoding = "UTF-8")
mics_surveys <- fread(tail(list.files("datasets/DHSMICS/", "mics_metadata.*[.]csv", full.names = T),1), encoding = "UTF-8")

dhs_surveys <- dhs_surveys[,c("Country.", "Contract.Phase.", "surveyyr")]
dhs_surveys <- merge(dhs_surveys, isos[, c("countryname_dhs", "iso3", "iso2")], by.x = "Country.", by.y = "countryname_dhs", all.x = T)

mics_surveys <- mics_surveys[,c("country", "round", "year")]
mics_surveys <- merge(mics_surveys, isos[, c("countryname_mics", "iso3", "iso2")], by.x = "country", by.y = "countryname_mics", all.x = T)

setnames(dhs_surveys, names(mics_surveys))

all_surveys <- rbind(dhs_surveys, mics_surveys)
all_surveys <- unique(all_surveys)

all_surveys[, year := as.numeric(substr(year, nchar(year) - 3, nchar(year)))]

surveys_cast <- dcast(all_surveys[!is.na(iso3)], iso3 + year ~., value.var = "round", fun.aggregate = function(x) paste0(x, collapse = "/"))

catnat[, affected := max(.SD), .SDcols = grep("Nbre", names(catnat), value = T), by = id]

catnat_cast <- dcast(catnat[`Indice événement` %in% c("Cataclysme")], iso3 + Year ~ ., value.var = "Indice événement", fun.aggregate = length)

merged <- merge(surveys_cast, catnat_cast, by.x = c("iso3", "year"), by.y = c("iso3", "Year"), all = T)
