required.packages <- c("data.table","jsonlite","httr","readxl")
lapply(required.packages, require, character.only=T)

setwd("G:/My Drive/Work/GitHub/gha_report_2021/")

###This map shows:###
#- The total number of PiN by country
#- The types of crises affecting each country (displacement, conflict, natural/technological hazard)
#- The INFORM severity score
#- The COVID-19 risk score
#- Vaccine rollout
#- Protracted crisis countries
#- HRP requirements
#- RRP requirements

##Total PiN##
acaps <- fread("datasets/ACAPS/ACAPS_PiN.csv")
pin.by.country <- acaps[year == 2020 & country_level == "Yes", .(iso3 = iso3_flat, Total.PiN = max(Total.PiN)), by = .(iso3_flat)]
pin.by.country[, iso3_flat := NULL]

##Crises by country##
countrynames <- fread("datasets/Countrynames/isos.csv", encoding = "UTF-8")

crises.by.country <- acaps[year == 2020, .(iso3 = unlist(strsplit(iso3_flat, ", ")), country = unlist(strsplit(country_flat, ", "))), by = crisis_category]
crises.by.country <- dcast(crises.by.country, iso3 + country ~ crisis_category, value.var = "crisis_category", fun.aggregate = length)

#Physical
em.dat <- data.table(read_excel("datasets/EMDAT/emdat_public_2021_03_30_query_uid-dsfpkn.xlsx", skip = 6))
pop <- fread("datasets/Population/WPP2019_TotalPopulationBySex.csv", encoding = "UTF-8")
pop <- pop[Time == 2020 & Variant == "Medium"][, c("Location", "PopTotal")]
pop <- merge(countrynames[,c("iso3", "countryname", "countryname_un")], pop, by.x = "countryname_un", by.y = "Location", all.y = T)
em.dat <- merge(em.dat[Year == 2020, .(Affected = sum(`Total Affected`, na.rm = T)/1000), by = ISO], pop, by.x = "ISO", by.y = "iso3", all.x = T)
em.dat <- em.dat[, .(share_affected = Affected/PopTotal), by = ISO]

physical <- merge(crises.by.country[,c("iso3", "Physical")], em.dat, by.x = "iso3", by.y = "ISO", all.x = T)
physical_isos <- physical[share_affected > min(physical[Physical > 0]$share_affected, na.rm = T) | Physical > 0]$iso3 #Countries with share of pop affected over the minimum already flagged as physical, plus those already flagged
rm(list = c("pop", "physical", "em.dat"))

#Conflict
hiik <- fread("datasets/HIIK/HIIK_CoBa_2020_dataset.csv", encoding = "UTF-8")
hiik$countries <- gsub(" [(].*| et al[.]| et[.] al|°","", hiik$conflict)
hiik$countries <- gsub(" – ", ", ", hiik$countries)

conflict <- hiik[, .(country = unlist(strsplit(countries, ", "))), by = .(type, intensity_2020)][, .(max_intensity_2020 = max(intensity_2020)), by = .(country, type)]
conflict <- merge(conflict, countrynames[,c("iso3", "countryname_hiik")], by.x = "country", by.y = "countryname_hiik", all.x = T)

max.country.conflict <- conflict[, .SD[which.max(max_intensity_2020)], by = .(country, iso3)]
non.max.country.conflict <- conflict[, .SD[!which.max(max_intensity_2020)], by = .(country, iso3)]

max.country.conflict <- merge(max.country.conflict, crises.by.country[Conflict > 0, c("iso3", "Conflict")], by = "iso3", all = T)
max.country.conflict <- unique(max.country.conflict[max_intensity_2020 >= 4 | Conflict > 0])
non.max.country.conflict <- unique(non.max.country.conflict[max_intensity_2020 >= 4])

conflict <- rbind(max.country.conflict, non.max.country.conflict, fill = T)

international.conflict_isos <- unique(conflict[type %in% c("interstate", "transstate")]$iso3)
national.conflict_isos <- unique(conflict[type %in% c("substate", "intrastate")]$iso3)
conflict_isos <- unique(conflict$iso3)

rm(list = c("hiik", "conflict"))

#Displacement
displacement_isos <- crises.by.country[Displacement > 0]$iso3

#All types
all.crises <- data.table(iso3 = crises.by.country$iso3)[, .(physical_flag = ifelse(iso3 %in% physical_isos, 1, 0), conflict_flag = ifelse(iso3 %in% conflict_isos, 1, 0), displacement_flag = ifelse(iso3 %in% displacement_isos, 1, 0)), by = iso3]

all.crises <- merge(pin.by.country, all.crises, by = "iso3", all = T)

##INFORM severity##
inform <- data.table(read_excel("datasets/INFORM/INFORM2021_TREND_2011_2020_v051_ALL.xlsx"))
inform <- inform[INFORMYear == 2020 & IndicatorName == "INFORM Risk Index"]
inform <- inform[, .(iso3 = Iso3, risk_class = ifelse(IndicatorScore >= 2, ifelse(IndicatorScore >= 3.5, ifelse(IndicatorScore >= 5, ifelse(IndicatorScore >= 6.5, "Very High", "High"), "Medium"), "Low"), "Very Low"))]

##COVID-19 risk##
inform_covid <- head(tail(data.table(read_excel("datasets/INFORM/INFORM COVID-19 RISK INDEX v014.xlsx", sheet = "INFORM COVID-19 RISK 2020 (a-z)", skip = 1)), -1), -3)
inform_covid <- inform_covid[, .(iso3 = ISO3, covid_risk_class = `COVID-19 RISK CLASS`)]

##Vaccine rollout##
vac_share <- fread("datasets/OWID/vaccination_shares.csv")
vac_share <- vac_share[, c("iso3", "vac_share")]

##Protracted crisis countries##


##HRP requirements##
hrps <- fread("datasets/FTS/HRP requirements_RRP_sep.csv", encoding = "UTF-8")
hrps <- hrps[!grepl("Regional|Horn of Africa|Global", plan.name)]
hrps <- merge(hrps, countrynames[,c("iso3", "countryname_fts")], by.x = "plan.name", by.y = "countryname_fts", all.x = T)

hrps <- hrps[, .(covid_funding = `COVID.Funded through this plan`/1000000, covid_requirements = `COVID.Total requirements`/1000000, noncovid_funding = `Non-COVID.Funded through this plan`/1000000, noncovid_requirements = `Non-COVID.Total requirements`/1000000), by = iso3]

##RRP requirements##
rrps <- fread("datasets/UNHCR RRPs/rrp_data.csv")
rrps <- rrps[Year == 2020, lapply(.SD, function(x) gsub("[$]|[(]Blank[)]|,", "", x)), by = Country]
rrps <- merge(rrps, countrynames[, c("iso3", "countryname_unhcr")], by.x = "Country", by.y = "countryname_unhcr", all.x = T)

rrps <- rrps[!is.na(iso3), .(rrp_funding = sum(as.numeric(`Funds Received`)/1000000, na.rm = T), rrp_requirements = sum(as.numeric(`Funds Requested`)/1000000, na.rm = T)), by = iso3]

###Join all

all.crises <- merge(all.crises, inform, all.x = T)
all.crises <- merge(all.crises, inform_covid, all.x = T)
all.crises <- merge(all.crises, vac_share, all.x = T)
#all.crises <- merge(all.crises, protracted_crises, all.x = T)
all.crises <- merge(all.crises, hrps, all.x = T)
all.crises <- merge(all.crises, rrps, all.x = T)
