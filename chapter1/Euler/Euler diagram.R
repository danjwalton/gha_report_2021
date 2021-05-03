required.packages <- c("data.table","jsonlite","httr","readxl","eulerr")
lapply(required.packages, require, character.only=T)

setwd("G:/My Drive/Work/GitHub/gha_report_2021/")

# - Poverty data: World Bank PovcalNet, National sources, IMF World Economic Outlook, DHS/MICS
# - Food insecurity: Food and Agriculture Organization (IPC/CH Classification data)
# - Risk of natural hazard: INFORM Index for Risk Management, EM-DAT-CRED
# - Risk of political instability: OECD SoF, HIIK, INFORM Index for Risk Management
# - Risk of pandemic: INFORM COVID?

isos <- fread("datasets/Countrynames/isos.csv")

##Poverty##
poverty <- fread("datasets/Poverty/globalproj_long_Apr21.csv")
sub_poverty <- fread("datasets/Poverty/subnational_poverty.csv")
poverty <- poverty[ProjYear == 2020 & PovertyLine == 1.9 & Level == "National"]

poverty <- dcast(poverty, CountryCode ~ variable, value.var = "value")

sub_poverty <- sub_poverty[year == 2020 & poverty_line == 1.9, .SD[survey_year == max(survey_year)], by = iso3]
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

ipc[is.na(value)]$value <- merge(ipc[is.na(value)][, c("iso3")], poverty[,c("CountryCode", "HeadCount")], by.x = "iso3", by.y = "CountryCode")$HeadCount

ipc <- ipc[, .(`ipc_poor` = sum(population*value), `ipc_nonpoor` = sum(population*(1-value))), by = .(iso3)]

ipc <- merge(ipc, poverty[CountryCode %in% ipc$iso3], by.x = "iso3", by.y = "CountryCode")
ipc[, `:=` (nonipc_poor = NumPoor - ipc_poor, nonipc_nonpoor = (ReqYearPopulation-NumPoor)-ipc_nonpoor)][, `:=` (HeadCount = NULL, NumPoor = NULL, ReqYearPopulation = NULL)]

ipc <- melt(ipc, id.vars = "iso3")[, c("ipc", "poor") := tstrsplit(variable, "_")][, variable := NULL]
nonipc <- melt(poverty[!(CountryCode %in% ipc$iso3)][, .(poor = NumPoor, nonpoor = ReqYearPopulation - NumPoor, ipc = "nonipc"), by = CountryCode], id.vars = c("CountryCode", "ipc"))

setnames(nonipc, c("CountryCode", "variable"), c("iso3", "poor"))

euler_dt <- rbind(ipc, nonipc)

##Risk of natural hazards##

emdat <- data.table(read_excel("datasets/EMDAT/emdat_public_2021_03_30_query_uid-dsfpkn.xlsx", skip = 6))
emdat <- emdat[Year == 2020, .(Affected = sum(`Total Affected`, na.rm = T)), by = ISO]

inform <- data.table(read_excel("datasets/INFORM/INFORM2021_TREND_2011_2020_v051_ALL.xlsx"))
inform_nh <- inform[INFORMYear == 2020 & IndicatorName == "Natural Hazard"]

env_isos <- inform_nh[IndicatorScore >= 6.9]$Iso3

euler_dt[, vuln := ifelse(iso3 %in% env_isos, "vuln", "nonvuln")]

##Political risk##
oecd_sof <- as.data.table(read_excel("datasets/OECD SoF/List of Fragile Contexts (2020).xlsx"))

fragile_isos <- oecd_sof$iso3c
exfragile_isos <- oecd_sof[fragility.level == "Extremely Fragile"]$iso3c

euler_dt[, fragile := ifelse(iso3 %in% fragile_isos, "fragile", "nonfragile")]
euler_dt[, exfragile := ifelse(iso3 %in% exfragile_isos, "exfragile", "nonexfragile")]

##COVID-19 risk##
inform_covid <- head(tail(data.table(read_excel("datasets/INFORM/INFORM COVID-19 RISK INDEX v014.xlsx", sheet = "INFORM COVID-19 RISK 2020 (a-z)", skip = 1)), -1), -3)
inform_covid <- inform_covid[, .(iso3 = ISO3, covid_risk_class = `COVID-19 RISK CLASS`)]

covid_isos <- inform_covid[covid_risk_class %in% c("Very High", "High")]$iso3

euler_dt[, covid := ifelse(iso3 %in% covid_isos, "covid", "noncovid")]

##Overall euler##
rm(list = ls()[ls() != "euler_dt"])

euler_dt <- euler_dt[, .(value = sum(value)), by = .(poor, ipc, vuln, fragile, exfragile, covid)]
euler_tf <- euler_dt[, lapply(.SD, function(x) ifelse(grepl("non.*", x), F, T)), by = value]

euler_sel <- function(data = euler_tf, value_col = "value", params = c("poor", "fragile", "covid"), control = list(T, 0), shape = "circle"){
  euler_a <- data[, c(params, value_col), with=F]
  euler_a <- euler_a[, .(values = sum(.SD)), by = params, .SDcols = value_col]
  values <- euler_a$values
  euler_a[, values := NULL]
  fit <- euler(euler_a, weights = values, control = control, shape = shape)
  return(fit)
}

euler1 <- euler_sel(euler_tf[poor == T], params = c("poor", "fragile", "vuln"))
euler2 <- euler_sel(params = c("poor", "ipc", "covid"))
euler3 <- euler_sel(euler_tf[poor == T], params = c("poor", "fragile", "ipc"))

euler4 <- euler_sel(params = c("poor", "fragile", "vuln", "covid"))
euler5 <- euler_sel(params = c("poor", "fragile", "vuln", "covid", "ipc"), shape = "ellipse")
euler6 <- euler_sel(params = c("covid", "exfragile", "poor"))
