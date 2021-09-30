required.packages <- c("data.table","readxl","zoo")
lapply(required.packages, require, character.only=T)

setwd("G:/My Drive/Work/GitHub/gha_report_2021/")

isos <- fread("datasets/Countrynames/isos.csv")

##Poverty##
poverty <- fread("datasets/Poverty/globalproj_long_May21.csv")
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
ipc_decode <- fread("datasets/IPC/ipc_region_decode.csv", encoding = "UTF-8") #Manually created decode list of IPC regions to DHS/MICS regions
ipc <- as.data.table(read_excel("datasets/IPC/All Countries - IPC Analysis 2017-2021.xlsx", skip = 11))

ipc <- ipc[, c("Country", "Level 1 Name", "Area", "Date of Analysis", "#...8", "Area Phase...10", "% of total county Pop...9")]
setnames(ipc, c("country", "region", "district", "date", "population", "phase", "cov_pop"))

ipc[, cov_pop := na.locf(cov_pop)]

ipc <- ipc[!is.na(district) & grepl("2021|2020|2019", date)]

ipc[is.na(region), region := district]

ipc[, region := tolower(gsub("`|’", "'", region))]

ipc[, date := as.Date(paste0("01 ", date), "%d %b %Y")]

ipc[!is.na(phase), nearest_date := .SD[which.min(abs(date - as.Date("2020-06-01")))]$date, country]

ipc <- ipc[date == nearest_date]

#ipc <- ipc[!is.na(phase), .SD[which.min(date - as.Date("2020-06-01"))], by = .(country, region, district)]

ipc <- ipc[, .(phase = mean(phase, na.rm = T), population = sum(population, na.rm = T), cov_pop = mean(cov_pop, na.rm = T)), by = .(country, region)]

ipc[country == "Congo, DRC"]$country <- "Congo Democratic Republic"
ipc <- merge(ipc, isos[, c("iso3", "countryname_dhs")], by.x = "country", by.y = "countryname_dhs", all.x = T)

##Regions covered by IPC
ipc <- merge(ipc, ipc_decode, by = c("country", "region"), all.x = T) #match dhsmics regions

ipc <- merge(ipc, sub_poverty[, c("iso3", "dhsmics", "poverty_line", "value")], by = c("iso3", "dhsmics"), all.x = T) #merge subnational poverty

ipc <- rbind(ipc[!is.na(value)], merge(ipc[is.na(value)][, -c("poverty_line", "value")], poverty[,c("iso3", "value", "poverty_line")], by = c("iso3"))) #Fill missing subnational values with national averages

ipc[, phase := ifelse(phase >= 3, "p3", "p2")] #assign phase score
ipc$poverty_line <- as.numeric(ipc$poverty_line)

#Adjust populations given by IPC to match those from Povcal as IPC's values seem off
ipc <- merge(ipc, poverty[, c("iso3", "poverty_line", "ReqYearPopulation")], by = c("iso3", "poverty_line"))

ipc[, adj_population := population/((sum(population)/cov_pop)/ReqYearPopulation), by = .(iso3, poverty_line)]

ipc <- ipc[, .(poor = sum(adj_population*value), nonpoor = sum(adj_population*(1-value)), cov_pop = mean(cov_pop, na.rm = T)), by = .(iso3, phase, poverty_line)]

##Regions not covered by IPC
ipc_cov <- ipc[, .(ipc_poor = sum(poor), ipc_nonpoor = sum(nonpoor), cov_pop = mean(cov_pop)), by = .(iso3, poverty_line)]

nonipc <- poverty[, c("iso3", "poverty_line", "value", "ReqYearPopulation")]
nonipc <- merge(nonipc, ipc_cov, all.x = T)
nonipc[is.na(nonipc)] <- 0

nonipc[cov_pop != 0, ReqYearPopulation := (ipc_poor + ipc_nonpoor)/cov_pop]

nonipc[, `:=` (poor = value*ReqYearPopulation - ipc_poor, nonpoor = (ReqYearPopulation - value*ReqYearPopulation) - ipc_nonpoor, phase = "nonipc", ipc_poor = NULL, ipc_nonpoor = NULL, value = NULL, ReqYearPopulation = NULL)]

ipc <- rbind(ipc, nonipc)

##Aggregate
ipc_melt <- melt(ipc, id.vars = c("iso3", "phase", "poverty_line", "cov_pop"))

ipc_agg <- ipc_melt[, .(value = sum(value)), by = .(phase, poverty_line, variable)]

fwrite(ipc_agg, "chapter1/IPC Poverty overlap/ipc_pov_overlap.csv")
