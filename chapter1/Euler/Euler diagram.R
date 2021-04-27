required.packages <- c("data.table","jsonlite","httr","readxl")
lapply(required.packages, require, character.only=T)

setwd("G:/My Drive/Work/GitHub/gha_report_2021/")

###This map shows:###
# - Poverty data: World Bank PovcalNet, National sources, IMF World Economic Outlook, DHS/MICS
# - Food insecurity: Food and Agriculture Organization (IPC/CH Classification data)
# - Risk of natural hazard: INFORM Index for Risk Management, EM-DAT-CRED
# - Risk of political instability: OECD SoF, HIIK, INFORM Index for Risk Management
# - Risk of pandemic: INFORM COVID?

isos <- fread("datasets/Countrynames/isos.csv")

##Poverty##
poverty <- fread("datasets/Poverty/globalproj_long_Apr21.csv")
sub_poverty <- fread("datasets/Poverty/subnational_poverty.csv")
poverty <- poverty[ProjYear == 2020 & PovertyLine == 3.2 & Level == "National"]
sub_poverty <- sub_poverty[year == 2020 & poverty_line == 3.2, .SD[survey_year == max(survey_year)], by = iso3]
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

ipc <- merge(ipc, sub_poverty[, c("iso3", "dhsmics", "value")], by = c("iso3", "dhsmics"), all.x = T)

ipc[is.na(value)]$value <- merge(ipc[is.na(value)][, c("iso3")], poverty[variable == "HeadCount",c("CountryCode", "value")], by.x = "iso3", by.y = "CountryCode")$value

ipc[, .(p3_poor = sum(population*value)/1000000, p3_nonpoor = sum(population*(1-value)/1000000)), by = iso3]

##Risk of natural hazards##
inform <- data.table(read_excel("datasets/INFORM/INFORM2021_TREND_2011_2020_v051_ALL.xlsx"))
inform_nh <- inform[INFORMYear == 2020 & IndicatorName == "Natural Hazard"]

##Political risk##


##COVID-19 risk##
inform_covid <- head(tail(data.table(read_excel("datasets/INFORM/INFORM COVID-19 RISK INDEX v014.xlsx", sheet = "INFORM COVID-19 RISK 2020 (a-z)", skip = 1)), -1), -3)
inform_covid <- inform_covid[, .(iso3 = ISO3, covid_risk_class = `COVID-19 RISK CLASS`)]

