required.packages <- c("data.table","Hmisc", "WDI", "jsonlite")
lapply(required.packages, require, character.only=T)

setwd("G:/My Drive/Work/GitHub/gha_report_2021/")

countrynames <- fread("datasets/Countrynames/isos.csv", encoding = "UTF-8")
pcc <- as.data.table(read_excel("datasets/PCC/Protracted Response, 2000-2021.xlsx", sheet = "Calcs."))
pcc <- pcc[, -c(3:24)]
pcc <- merge(pcc, countrynames[, c("iso2", "iso3")], by.x = "Country ID", by.y = "iso2", all.x = T)

###Projected vaccine rollout is crises countries###

vac.pd <- fread("datasets/OWID/vaccination_per_day.csv")

vac.pd[!is.na(vac_pd_pm), smooth_vac_share := cumsum(vac_pd_pm)/1000000, by = iso3]

logitapprox <- function(x, y, xout){
  delta <- 0
  y <- y[!is.na(y)]
  x <- x[!is.na(y)]
  y[y >= 1] <- 1-delta
  y[y <= 0] <- delta
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
  return(list(xout = xout, yout = yout))
}

proj <- as.IDate(seq.Date(as.Date("2021-01-01"), as.Date("2021-12-31"), by = 1))

vac.logit.proj <- vac.pd[!is.na(smooth_vac_share), .(date = logitapprox(date, smooth_vac_share, proj)$xout, vac_share = logitapprox(date, smooth_vac_share, proj)$yout), by=.(iso3)]
vac.linear.proj <- vac.pd[!is.na(smooth_vac_share), .(date = approxExtrap(date, smooth_vac_share, proj)$x, vac_share = approxExtrap(date, smooth_vac_share, proj)$y), by=.(iso3)]

plot.proj <- function(data = vac.linear.proj, iso){
  plot(data[iso3 == iso]$date, data[iso3 == iso]$vac_share)
}

###
vac.utd <- fread("datasets/OWID/vaccination_shares.csv")

vac.utd <- merge(vac.utd[iso3 %in% countrynames$iso3], pcc[, .(iso3, `Protracted Crisis in 2020`)], all.x = T)
vac.utd[is.na(`Protracted Crisis in 2020`), `Protracted Crisis in 2020` := "No"]

vac.shares <- vac.utd[, .(wavg_share = sum(vac_num)/sum(vac_num/(vac_share/100)), avg_share = mean(vac_share/100)), by = "Protracted Crisis in 2020"]
