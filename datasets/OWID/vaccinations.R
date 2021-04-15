required.packages <- c("data.table")
lapply(required.packages, require, character.only=T)

setwd("G:/My Drive/Work/GitHub/gha_report_2021")

vac <- fread("https://raw.githubusercontent.com/owid/covid-19-data/master/public/data/vaccinations/vaccinations.csv")

utd.vac <- vac[!is.na(people_vaccinated), .SD[which.max(as.IDate(date))], by = iso_code]
utd.vac <- utd.vac[, .(iso3 = iso_code, vac_share = people_vaccinated_per_hundred, full_vac_share = people_fully_vaccinated_per_hundred)]

trend.vac <- vac[, .(iso3 = iso_code, date = as.IDate(date), vac_share = people_vaccinated_per_hundred, full_vac_share = people_fully_vaccinated_per_hundred)]
pd.vac <- vac[, .(iso3 = iso_code, date = as.IDate(date), vac_pd = daily_vaccinations, vac_pd_pm = daily_vaccinations_per_million)]

fwrite(trend.vac, "datasets/OWID/vaccination_trends.csv")
fwrite(utd.vac, "datasets/OWID/vaccination_shares.csv")
fwrite(pd.vac, "datasets/OWID/vaccination_per_day.csv")
