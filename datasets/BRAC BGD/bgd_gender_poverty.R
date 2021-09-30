required.packages <- c("data.table", "boot","rstudioapi", "WDI")
suppressPackageStartupMessages(lapply(required.packages, require, character.only=T))

setwd(dirname(dirname(dirname(getActiveDocumentContext()$path))))

brac <- fread("datasets/BRAC BGD/BRAC BGD.csv")

z_data <- brac[, .(Respondent_ID, Division, District, `Upazila/pouroshova`, `Union/ward`, Area, Age, Sex, Q12_current_income, Q13_prev_month_income, Q14_current_month_expen, Q15_prev_expense, Q16)]

setnames(z_data, c("Q12_current_income", "Q13_prev_month_income", "Q14_current_month_expen", "Q15_prev_expense", "Q16"), c("new_i", "old_i", "new_c", "old_c", "hh_size"))

cpi_2020 <- 273.26 
cpi_2011 <- (156.59+170.19)/2 #http://203.112.218.65:8008/WebTestApplication/userfiles/Image/National%20Account%20Wing/CPI/CPI_March2014.pdf

cpi <- cpi_2020/cpi_2011
ppp <- 23.41 #https://data.worldbank.org/indicator/PA.NUS.PPP?locations=BD

cpi_ppp <- cpi*ppp

z_cols <- c("new_i", "old_i", "new_c", "old_c")
z_data[, (z_cols) := ((.SD/cpi_ppp)*(12/365.25))/(hh_size^0.5), .SDcols = z_cols]

poverty <- function(z, pl){
  bt <- boot(z, statistic = function(x, i) length(x[i][x[i] <= pl])/length(x[i]), R = 5000)
  bt.ci <- boot.ci(bt, type="bca")
  bt.dt <- data.table(value = bt.ci$t0, lci = bt.ci$bca[,4], uci = bt.ci$bca[,5])
  return(bt.dt)
}

pov_out <- z_data[Sex != "Others", lapply(.SD, function(x) list(poverty(x, pl = 1.9))), .SDcols = z_cols, by = Sex]

pov_out_simple1.9 <- z_data[Sex != "Others", lapply(.SD, function(x) length(x[x <= 1.9])/length(x)), .SDcols = z_cols, by = Sex]
pov_out_simple3.2 <- z_data[Sex != "Others", lapply(.SD, function(x) length(x[x <= 3.2])/length(x)), .SDcols = z_cols, by = Sex]

pov_out_simple <- rbind(data.table(pl = 1.9, pov_out_simple1.9), data.table(pl = 3.2, pov_out_simple3.2))

fwrite(pov_out_simple, "datasets/BRAC BGD/pov_out.csv")