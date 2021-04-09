required.packages <- c("data.table")
lapply(required.packages, require, character.only=T)

setwd("G:/My Drive/Work/GitHub/gha_report_2021")

vac <- fread("https://github.com/owid/covid-19-data/blob/master/public/data/vaccinations/vaccinations.csv")

utd.vac <- vac[!is.na(people_vaccinated), .SD[which.max(as.Date(date))], by = iso_code]

utd.vac <- utd.vac[, .(iso3 = iso_code, vac_share = people_vaccinated_per_hundred, full_vac_share = people_fully_vaccinated_per_hundred)]

fwrite(utd.vac, "datasets/OWID/vaccination_shares.csv")
