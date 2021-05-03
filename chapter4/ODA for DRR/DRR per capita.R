required.packages <- c("data.table", "readxl")
lapply(required.packages, require, character.only = T)

setwd("G:/My Drive/Work/GitHub/gha_report_2021/")

crs <- fread("chapter4/ODA for DRR/crs_ddr.csv")

crs_agg <- crs[drr_score == "Major" & hazard_class != "", .(disb = sum(USD_Disbursement_Defl)), by = .(iso3, hazard_class)]

isos <- fread("datasets/Countrynames/isos.csv")
pop <- fread("datasets/Population/WPP2019_TotalPopulationBySex.csv")
pop <- pop[Time == 2020 & Variant == "Medium"][, c("Location", "PopTotal")]

pop <- merge(pop, isos[,c("iso3", "countryname_un")], by.x = "Location", by.y = "countryname_un", all.x = T)

crs_agg <- merge(crs_agg, pop, by = "iso3", all.x = T)
crs_agg[, per_cap := disb/(PopTotal/1000), by = .(iso3, hazard_class)]

crs_avg <- crs_agg[, .(avg_country = mean(disb, na.rm = T), avg_country_pc = mean(per_cap, na.rm = T), avg_pc = sum(disb, na.rm = T)/sum(PopTotal/1000, na.rm = T)), by = hazard_class]

fwrite(crs_avg, "chapter4/ODA for DRR/crs_drr_avg.csv")
