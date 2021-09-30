required.packages <- c("data.table")
lapply(required.packages, require, character.only=T)

countrynames <- fread("datasets/Countrynames/isos.csv", encoding = "UTF-8")
pcc <- as.data.table(read_excel("datasets/PCC/Protracted Response, 2000-2021.xlsx", sheet = "Calcs."))
pcc <- pcc[, -c(3:24)]
pcc <- merge(pcc, countrynames[, c("iso2", "iso3")], by.x = "Country ID", by.y = "iso2", all.x = T)

iati <- data.table(fromJSON("https://d-portal.org/q?from=act%2Ccountry&limit=-1&year_min=2020&year_max=2020&%2Ftransaction%2Ftransaction-type%40code=3%2C4&day_start_lteq=2021-01-01&day_end_gt=2020-01-01&country_percent=100")$rows)
iati_loc <- data.table(fromJSON("https://d-portal.org/q?from=act%2Clocation%2Ccountry&limit=-1&year_min=2020&year_max=2020&%2Ftransaction%2Ftransaction-type%40code=3%2C4&day_start_lteq=2021-01-01&day_end_gt=2020-01-01&country_percent=100")$rows)

iati <- iati[aid %in% iati_loc[location_precision == 1]$aid, location_p := TRUE]
iati <- iati[aid %in% iati_loc$aid, location := TRUE]

pcc_iso2 <- pcc[`Protracted Crisis in 2020` == "Yes"]$`Country ID`
iati[country_code %in% pcc_iso2, pcc := TRUE]

crisis_iso2 <- pcc[`2020...45` >= 1]$`Country ID`
iati[country_code %in% crisis_iso2, crisis := TRUE]

iati_agg <- iati[, .(spend = sum(spend, na.rm = T), count = nrow(.SD)), by = .(location_p, location, crisis, pcc)]

sum(iati_agg[location & crisis]$spend) / sum(iati_agg[crisis == T]$spend)
sum(iati_agg[location & pcc]$spend) / sum(iati_agg[pcc == T]$spend)
sum(iati_agg[location & is.na(pcc)]$spend) / sum(iati_agg[is.na(pcc)]$spend)

sum(iati_agg[location_p & crisis]$spend) / sum(iati_agg[crisis == T]$spend)
sum(iati_agg[location_p & pcc]$spend) / sum(iati_agg[pcc == T]$spend)
sum(iati_agg[location_p & is.na(pcc)]$spend) / sum(iati_agg[is.na(pcc)]$spend)

sum(iati_agg[location_p & crisis]$count) / sum(iati_agg[crisis == T]$count)
sum(iati_agg[location_p & pcc]$count) / sum(iati_agg[pcc == T]$count)
sum(iati_agg[location_p & is.na(pcc)]$count) / sum(iati_agg[is.na(pcc)]$count)

sum(iati_agg[location & crisis]$count) / sum(iati_agg[crisis == T]$count)
sum(iati_agg[location & pcc]$count) / sum(iati_agg[pcc == T]$count)
sum(iati_agg[location & is.na(pcc)]$count) / sum(iati_agg[is.na(pcc)]$count)
