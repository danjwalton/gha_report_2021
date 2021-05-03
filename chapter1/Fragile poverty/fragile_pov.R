required.packages <- c("data.table","jsonlite","httr","readxl","eulerr")
lapply(required.packages, require, character.only=T)

setwd("G:/My Drive/Work/GitHub/gha_report_2021/")

isos <- fread("datasets/Countrynames/isos.csv")

##Poverty##
poverty <- fread("datasets/Poverty/globalproj_long_Apr21.csv")
poverty <- poverty[PovertyLine == 1.9 & Level == "National"]

poverty <- dcast(poverty, CountryCode + ProjYear ~ variable, value.var = "value")

##Political risk##
#sof_2015 <- fread("datasets/OECD SoF/sof_2015.csv", encoding = "UTF-8")[, year := 2015]
sof_2016 <- fread("datasets/OECD SoF/sof_2016.csv", encoding = "UTF-8")[, year := 2016]
sof_2018 <- fread("datasets/OECD SoF/sof_2018.csv", encoding = "UTF-8")[fragility.level != "Rest of the World"][, year := 2018]
sof_2020 <- as.data.table(read_excel("datasets/OECD SoF/List of Fragile Contexts (2020).xlsx"))[, year := 2020]

sof <- rbind(sof_2016[, c("iso3c", "year")], sof_2018[, c("iso3c", "year")], sof_2020[, c("iso3c", "year")])
sof[, fragile := "fragile"]

con.fragile <- intersect(intersect(sof_2016$iso3c, sof_2018$iso3c), sof_2020$iso3c) #Consistently fragile states

poverty <- merge(poverty, sof, by.x = c("CountryCode", "ProjYear"), by.y = c("iso3c", "year"), all.x = T)

poverty[CountryCode %in% con.fragile, fragile := "con.fragile"]

##COVID-19 risk##
inform_covid <- head(tail(data.table(read_excel("datasets/INFORM/INFORM COVID-19 RISK INDEX v014.xlsx", sheet = "INFORM COVID-19 RISK 2020 (a-z)", skip = 1)), -1), -3)
inform_covid <- inform_covid[, .(iso3 = ISO3, covid_risk_class = `COVID-19 RISK CLASS`)]

covid_isos <- inform_covid[covid_risk_class %in% c("Very High", "High")]$iso3

poverty[CountryCode %in% covid_isos, covid := "covid"]

fragile_pov <- poverty[ProjYear >= 2010 & ProjYear <= 2020, .(NumPoor = sum(NumPoor), ReqYearPopulation = sum(ReqYearPopulation)), by = .(covid, fragile, ProjYear)][, `:=` (HeadCount = NumPoor/ReqYearPopulation, SharePoor = NumPoor/sum(NumPoor), TotalPoor = sum(NumPoor)), by = ProjYear][]

fwrite(fragile_pov, "chapter1/Fragile poverty/fragile_pov.csv")

#Internal
euler3 <- euler(c(expoor = sum(fragile_pov[ProjYear == 2010]$NumPoor),
                  fragile = sum(fragile_pov[ProjYear == 2010 & fragile == "con.fragile"]$NumPoor),
                  "expoor&fragile" = sum(fragile_pov[ProjYear == 2010 & fragile == "con.fragile"]$NumPoor)),
                input = "union")

euler4_covid <- euler(c(
  expoor = sum(fragile_pov[ProjYear == 2020]$NumPoor),
  fragile = sum(fragile_pov[ProjYear == 2020 & fragile == "con.fragile"]$NumPoor),
  covid = sum(fragile_pov[ProjYear == 2020 & covid == "covid"]$NumPoor),
  "expoor&fragile" = sum(fragile_pov[ProjYear == 2020 & fragile == "con.fragile"]$NumPoor),
  "expoor&covid" = sum(fragile_pov[ProjYear == 2020 & covid == "covid"]$NumPoor),
  "covid&fragile" = sum(fragile_pov[ProjYear == 2020 & fragile == "con.fragile" & covid == "covid"]$NumPoor), 
  "expoor&fragile&covid" = sum(fragile_pov[ProjYear == 2020 & fragile == "con.fragile" & covid == "covid"]$NumPoor)
)
, input = "union")

labels3 <- c(expoor = sum(fragile_pov[ProjYear == 2010]$NumPoor), fragile = sum(fragile_pov[ProjYear == 2010 & fragile == "con.fragile"]$NumPoor), "expoor&fragile" = sum(fragile_pov[ProjYear == 2010 & fragile == "con.fragile"]$NumPoor))

labels4_covid <- c(
  expoor = sum(fragile_pov[ProjYear == 2020]$NumPoor),
  fragile = sum(fragile_pov[ProjYear == 2020 & fragile == "con.fragile"]$NumPoor),
  covid = sum(fragile_pov[ProjYear == 2020 & covid == "covid"]$NumPoor),
  "expoor&fragile" = sum(fragile_pov[ProjYear == 2020 & fragile == "con.fragile"]$NumPoor),
  "expoor&covid" = sum(fragile_pov[ProjYear == 2020 & covid == "covid"]$NumPoor),
  "covid&fragile" = sum(fragile_pov[ProjYear == 2020 & fragile == "con.fragile" & covid == "covid"]$NumPoor), 
  "expoor&fragile&covid" = sum(fragile_pov[ProjYear == 2020 & fragile == "con.fragile" & covid == "covid"]$NumPoor)
)

dat_out <- cbind(year = c("2010", "2020"), rbind(as.data.table(t(labels3)), as.data.table(t(labels4_covid)), fill = T))

fwrite(dat_out, "chapter1/Fragile poverty/euler_dat.csv")
           