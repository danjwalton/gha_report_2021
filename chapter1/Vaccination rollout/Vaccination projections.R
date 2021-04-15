required.packages <- c("data.table","Hmisc", "WDI", "jsonlite")
lapply(required.packages, require, character.only=T)

setwd("G:/My Drive/Work/GitHub/gha_report_2021/")

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
