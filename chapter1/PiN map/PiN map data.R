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

##ACAPS severity##
severity <- acaps[!is.na(`Concentration of conditions`) & year == 2020 & nchar(iso3_flat) == 3, .(severity_max = max(as.numeric(`Concentration of conditions`), na.rm = T)), by = .(iso3_flat)]
pin.by.country <- merge(pin.by.country, severity, by.x = "iso3", by.y = "iso3_flat", all = T)

#Lack of coping and vulnerability
inform <- data.table(read_excel("datasets/INFORM/INFORM2021_TREND_2011_2020_v051_ALL.xlsx"))
lcc_isos <- inform[INFORMYear == 2020 & IndicatorName == "Lack of Coping Capacity Index" & IndicatorScore >= 4.8]$Iso3
vuln_isos <- inform[INFORMYear == 2020 & IndicatorName == "Vulnerability Index" & IndicatorScore >= 6]$Iso3

#Physical
em.dat <- data.table(read_excel("datasets/EMDAT/emdat_public_2021_03_30_query_uid-dsfpkn.xlsx", skip = 6))
pop <- fread("datasets/Population/WPP2019_TotalPopulationBySex.csv", encoding = "UTF-8")
pop <- pop[Time == 2020 & Variant == "Medium"][, c("Location", "PopTotal")]
pop <- merge(countrynames[,c("iso3", "countryname", "countryname_un")], pop, by.x = "countryname_un", by.y = "Location", all.y = T)
em.dat <- merge(em.dat[Year == 2020, .(Affected = sum(`Total Affected`, na.rm = T)/1000), by = ISO], pop, by.x = "ISO", by.y = "iso3", all.x = T)
em.dat <- em.dat[, .(share_affected = Affected/PopTotal), by = ISO]

physical <- merge(crises.by.country[,c("iso3", "Physical")], em.dat, by.x = "iso3", by.y = "ISO", all.x = T)

min_physical_share <- min(physical[Physical > 0 & iso3 %in% lcc_isos]$share_affected, na.rm = T)

physical_isos <- physical[(share_affected > min_physical_share & iso3 %in% lcc_isos) | Physical > 0]$iso3 #Countries with share of pop affected over the minimum already flagged as physical, plus those already flagged
physical_isos <- c(physical_isos, "MWI") #manually add Malawi

rm(list = c("physical", "em.dat"))

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
unrwa <- as.data.table(read_excel("datasets/UNRWA/UNRWA.xlsx", sheet = "UNRWA", range = "A1:H73"))
unhcr <- as.data.table(read_excel("datasets/UNHCR/UNHCR 2015-2020 midyr.xlsx", sheet = "population", skip = 14))
idmc <- as.data.table(read_excel("datasets/IDMC/IDMC 2020.xlsx", sheet = "IDMC 2020 data"))

unrwa <- unrwa[Year == 2020, .(iso2 = `Country Code`, Year = Year, Refugees = value)]
unrwa <- merge(unrwa, countrynames[, c("iso2", "iso3")])[, iso2 := NULL]

unhcr <- unhcr[Year == 2020, .(iso3 = `Country of asylum (ISO)`, Year = Year, Refugees = `Refugees under UNHCR's mandate`, Asylum_seekers = `Asylum-seekers`, IDPs = `IDPs of concern to UNHCR`, Stateless = `Stateless persons`, Others = `Others of concern`)]
unhcr <- unhcr[, lapply(.SD, sum), by = .(iso3, Year)]

idmc <- idmc[Year == 2020, .(Year, ISO3, `Total number of IDPS`)]
setnames(idmc, c("Year", "iso3", "displaced"))

displaced <- rbind(unrwa, unhcr, fill = T)

displaced <- displaced[, .(displaced = sum(.SD, na.rm = T)), by = .(iso3, Year)]

displaced <- rbind(displaced, idmc)

displaced <- displaced[, .(displaced = max(displaced, na.rm = T)), by = .(iso3, Year)]
displaced[is.infinite(displaced), displaced := 0]

displaced <- merge(displaced, pop[, c("iso3", "PopTotal")])
displaced <- displaced[, share_displaced := displaced/(PopTotal*1000)]

min_displaced_share <- min(displaced[iso3 %in% crises.by.country[Displacement > 0 & iso3 %in% lcc_isos & iso3 %in% vuln_isos]$iso3]$share_displaced)

displacement_isos <- displaced[(share_displaced > min_displaced_share & iso3 %in% lcc_isos & iso3 %in% vuln_isos) | iso3 %in% crises.by.country[Displacement > 0]$iso3]$iso3
displacement_isos <- c(displacement_isos, "VEN") #manually add Venezuela

#All types
all.crises <- data.table(iso3 = crises.by.country$iso3)[, .(physical_flag = ifelse(iso3 %in% physical_isos, 1, 0), conflict_flag = ifelse(iso3 %in% conflict_isos, 1, 0), displacement_flag = ifelse(iso3 %in% displacement_isos, 1, 0)), by = iso3]

all.crises <- merge(pin.by.country, all.crises, by = "iso3", all = T)

all.crises <- merge(countrynames[, c("countryname", "iso3")], all.crises, by = "iso3")

##INFORM severity##
inform_risk <- inform[INFORMYear == 2020 & IndicatorName == "INFORM Risk Index"]
inform_risk <- inform_risk[, .(iso3 = Iso3, risk_class = ifelse(IndicatorScore >= 2, ifelse(IndicatorScore >= 3.5, ifelse(IndicatorScore >= 5, ifelse(IndicatorScore >= 6.5, "Very High", "High"), "Medium"), "Low"), "Very Low"))]

# ##COVID-19 risk##
inform_covid <- head(tail(data.table(read_excel("datasets/INFORM/INFORM COVID-19 RISK INDEX v014.xlsx", sheet = "INFORM COVID-19 RISK 2020 (a-z)", skip = 1)), -1), -3)
inform_covid <- inform_covid[, .(iso3 = ISO3, covid_risk_class = `COVID-19 RISK CLASS`)]

# ##Vaccine rollout##
# vac_share <- fread("datasets/OWID/vaccination_shares.csv")
# vac_share <- vac_share[, c("iso3", "vac_share")]

##Protracted crisis countries##
pcc <- as.data.table(read_excel("datasets/PCC/Protracted Response, 2000-2021.xlsx", sheet = "Calcs."))
pcc <- merge(pcc[, c("Country ID", "2020...45", "Protracted Crisis in 2020")], countrynames[, c("iso2", "iso3")], by.x = "Country ID", by.y = "iso2", all.x = T)
pcc <- pcc[, .(iso3 = iso3, protracted_crisis_flag = ifelse(`Protracted Crisis in 2020` == "Yes", 1, 0), consecutive_crisis_years = `2020...45`)]

##HRP requirements##
hrps <- fread("datasets/FTS/HRP requirements_RRP_sep.csv", encoding = "UTF-8")
hrps <- hrps[!grepl("Regional|Horn of Africa|Global", plan.name)]
hrps <- merge(hrps, countrynames[,c("iso3", "countryname_fts")], by.x = "plan.name", by.y = "countryname_fts", all.x = T)

#hrps <- hrps[, .(covid_funding = `COVID.Funded through this plan`/1000000, covid_requirements = `COVID.Total requirements`/1000000, noncovid_funding = `Non-COVID.Funded through this plan`/1000000, noncovid_requirements = `Non-COVID.Total requirements`/1000000), by = iso3]
hrps <- hrps[, .(hrp_funding = (`COVID.Funded through this plan` + `Non-COVID.Funded through this plan`)/1000000, hrp_requirements = (`COVID.Total requirements` + `Non-COVID.Total requirements`)/1000000), by = iso3]

##RRP requirements##
rrps <- fread("datasets/UNHCR RRPs/rrp_data.csv")
rrps <- rrps[Year == 2020, lapply(.SD, function(x) gsub("[$]|[(]Blank[)]|,", "", x)), by = Country]
rrps <- merge(rrps, countrynames[, c("iso3", "countryname_unhcr")], by.x = "Country", by.y = "countryname_unhcr", all.x = T)

rrps <- rrps[!is.na(iso3), .(rrp_funding = sum(as.numeric(`Funds Received`)/1000000, na.rm = T), rrp_requirements = sum(as.numeric(`Funds Requested`)/1000000, na.rm = T)), by = iso3]

##Population
pop <- fread("datasets/Population/WPP2019_TotalPopulationBySex.csv", encoding = "UTF-8")
pop <- pop[Time == 2020 & Variant == "Medium"]

pop <- merge(pop, countrynames[, c("iso3", "countryname_un")], by.x = "Location", by.y = "countryname_un")
all.crises <- merge(all.crises, pop[, c("iso3", "PopTotal")], by = "iso3")[, PopTotal := PopTotal/1000]

###Join all

all.crises <- merge(all.crises, inform_risk, all.x = T)
all.crises <- merge(all.crises, inform_covid, all.x = T)
#all.crises <- merge(all.crises, vac_share, all.x = T)
all.crises <- merge(all.crises, pcc, all.x = T)
all.crises <- merge(all.crises, hrps, all.x = T)
all.crises <- merge(all.crises, rrps, all.x = T)

all.crises[is.na(all.crises)] <- 0

fwrite(all.crises, "chapter1/PiN map/all_crises.csv")
