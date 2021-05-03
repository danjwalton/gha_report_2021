required.packages <- c("data.table","jsonlite","httr","readxl","eulerr")
lapply(required.packages, require, character.only=T)

setwd("G:/My Drive/Work/GitHub/gha_report_2021/")

isos <- fread("datasets/Countrynames/isos.csv")

##Poverty##
poverty <- fread("datasets/Poverty/globalproj_long_Apr21.csv")
sub_poverty <- fread("datasets/Poverty/subnational_poverty.csv")
poverty <- poverty[ProjYear == 2020 & Level == "National"]

poverty <- dcast(poverty, CountryCode + PovertyLine ~ variable, value.var = "value")
setnames(poverty, c("iso3", "poverty_line", "value", "NumPoor", "ReqYearPopulation"))

sub_poverty <- sub_poverty[year == 2020, .SD[survey_year == max(survey_year)], by = iso3]
sub_poverty[, urban_rural := tolower(urban_rural)]

sub_poverty[urban_rural %in% c("urban", "urbain", "urbana", "urbano"), urban_rural := "urban"]
sub_poverty[urban_rural %in% c("rural", "rural with road", "non urban", "camp", "rural coastal", "rural interior", "rural y menores de 5 mil habitantes", "rural without road"), urban_rural := "rural"]

sub_poverty[, dhsmics := ifelse(urban_rural != "total", paste0(region, " ", urban_rural), region)]

##Food insecurity##
ipc_decode <- fread("datasets/IPC/ipc_region_p3_decode.csv", encoding = "UTF-8")
ipc <- as.data.table(read_excel("datasets/IPC/All Countries - IPC Analysis 2017-2021.xlsx", skip = 11))
ipc <- ipc[!is.na(Area) & grepl("2021|2020|2019", `Date of Analysis`)]

ipc <- ipc[, c("Country", "Level 1 Name", "Area", "Date of Analysis", "#...8", "Area Phase...10")]
setnames(ipc, c("country", "region", "district", "date", "population", "phase"))

ipc[is.na(region), region := district]
ipc <- ipc[, .(phase = mean(phase, na.rm = T), population = mean(population, na.rm = T)), by = .(country, region)]

ipc <- ipc[phase >= 3]

ipc <- merge(ipc, ipc_decode, by = c("country", "region"), all.x = T)

ipc[country == "Congo, DRC"]$country <- "Congo Democratic Republic"
ipc <- merge(ipc, isos[, c("iso3", "countryname_dhs")], by.x = "country", by.y = "countryname_dhs")

ipc[dhsmics == "zabul rural"]$dhsmics <- "zabul" #no rural data

ipc <- merge(ipc, sub_poverty[, c("iso3", "dhsmics", "poverty_line", "value")], by = c("iso3", "dhsmics"), all.x = T)

ipc <- rbind(ipc[!is.na(value)], merge(ipc[is.na(value)][, -c("poverty_line", "value")], poverty[,c("iso3", "value", "poverty_line")], by = c("iso3")))

ipc <- ipc[, .(`ipc_poor` = sum(population*value), `ipc_nonpoor` = sum(population*(1-value))), by = .(iso3, poverty_line)]
ipc$poverty_line <- as.numeric(ipc$poverty_line)

ipc <- merge(ipc, poverty[iso3 %in% ipc$iso3], by = c("iso3", "poverty_line"))
ipc[, `:=` (nonipc_poor = NumPoor - ipc_poor, nonipc_nonpoor = (ReqYearPopulation-NumPoor)-ipc_nonpoor)][, `:=` (value = NULL, NumPoor = NULL, ReqYearPopulation = NULL)]

ipc <- melt(ipc, id.vars = c("iso3", "poverty_line"))[, c("ipc", "poor") := tstrsplit(variable, "_")][, variable := NULL]
nonipc <- melt(poverty[!(iso3 %in% ipc$iso3)][, .(poor = NumPoor, nonpoor = ReqYearPopulation - NumPoor, ipc = "nonipc"), by = .(iso3, poverty_line)], id.vars = c("iso3", "poverty_line", "ipc"))

setnames(nonipc, c("variable"), c("poor"))

ipc <- rbind(ipc, nonipc)

ipc_agg <- ipc[, .(value = sum(value)), by = .(ipc, poverty_line, poor) ]

fwrite(ipc_agg, "chapter1/IPC Poverty overlap/ipc_pov_overlap.csv")
