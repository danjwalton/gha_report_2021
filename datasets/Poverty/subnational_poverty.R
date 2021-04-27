required.packages <- c("data.table", "Hmisc", "stringr")
lapply(required.packages, require, character.only=T)

setwd("G:/My Drive/Work/GitHub/gha_report_2021/")

logitapprox <- function(x, y, xout){
  delta <- 10^-16
  y <- y[!is.na(y)]
  x <- x[!is.na(y)]
  y[y == 1] <- 1-delta
  y[y == 0] <- delta
  suppressWarnings(ylogit <- log(y/(1-y)))
  if(length(ylogit[!is.na(ylogit)]) > 1){
    yapprox <- approxExtrap(x, ylogit, xout)$y
    yout <- 1/(1+exp(-yapprox))
    yout[is.nan(yout)] <- 0
  } else {
    yout <- NA
  }
  yout[yout == 1-delta] <- 1
  yout[yout == delta] <- 0
  return(yout)
}

povcuts <- fread("datasets/Poverty/globalproj_long_Apr21.csv")
povyears <- c(1981:2025)
povcuts <- povcuts[variable == "HeadCount" & PovertyLine %in% c(1.9,3.2,5.5),
                   .(RequestYear=povyears,
                     HeadCount=logitapprox(ProjYear, value, povyears)),
                   by=.(CountryCode, DisplayName, region, Level, PovertyLine)]

all_hh <- readRDS("datasets/DHSMICS/subnational_wealth.RData")

all_pov <- list()
pb <- txtProgressBar(0, length(all_hh), style = 3)
for(i in 1:length(all_hh)){
  
  setTxtProgressBar(pb, i)
  hh <- all_hh[[i]]
  iso3 <- hh$iso3
  if(any(!(c("urban_rural", "region") %in% names(hh$data)))) next
  
  povcut <- povcuts[CountryCode == iso3]
  
  regions <- unique(hh$data[, c("region")])
  subregions <- unique(hh$data[, c("urban_rural", "region")])
  for(j in 1:nrow(povcut)){
    povslice <- povcut[j]
    
    pov <- hh$data[, sum(hh_weight[wpercentile <= povslice$HeadCount])/sum(hh_weight), by = .(region)]
    setnames(pov, "V1", paste0(povslice$RequestYear, "_", povslice$PovertyLine))
    
    sub_pov <- hh$data[, sum(hh_weight[wpercentile <= povslice$HeadCount])/sum(hh_weight), by = .(urban_rural, region)]
    setnames(sub_pov, "V1", paste0(povslice$RequestYear, "_", povslice$PovertyLine))
    
    regions <- merge(regions, pov)
    subregions <- merge(subregions, sub_pov)
    
  }
  
  regions$urban_rural <- "Total"
  
  subregions <- rbind(regions, subregions)
  
  subregions <- melt(subregions, id.vars = c("urban_rural", "region"))[, c("year", "poverty_line") := tstrsplit(variable, "_")][, variable := NULL]
  subregions <- data.table(iso3 = hh$iso3, survey_year = hh$year, subregions)
  
  all_pov[[i]] <- subregions
}
close(pb)

all_pov[sapply(all_pov, is.null)] <- NULL
all_pov <- rbindlist(all_pov)

fwrite(all_pov, "datasets/Poverty/subnational_poverty.csv")
