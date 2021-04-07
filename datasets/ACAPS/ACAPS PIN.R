required.packages <- c("data.table","jsonlite","httr","readxl")
lapply(required.packages, require, character.only=T)

setwd("G:/My Drive/Work/GitHub/gha_report_2021/datasets/ACAPS")

gha2020 <- data.table(read_excel("GHA2020 PiN.xlsx", sheet = "Figure 1.3", skip = 15))
income <- fread("income_levels.csv")
population <- fread("population_2020.csv")[, c("iso_code", "population")]

income_pop <- merge(income, population, by.x = "ISO3", by.y = "iso_code")

gha.conflict <- gha2020[`Conflict marker` == "Yes"]$Country
gha.displacement <- gha2020[`Displacement marker` == "Yes"]$Country
gha.physical <- gha2020[`Natural disaster marker` == "Yes"]$Country

#token <- paste0("Token ",content(POST("https://api.acaps.org/api/v1/token-auth/", body=list(username = "...", password = "...")))$token)
#writeLines(token, file("acaps_token.txt"))
token <- readLines(file("acaps_token.txt"))
close(file("project_data/acaps_token.txt"))

get.acaps <- function(database, dataset=NULL, date=NULL, parameters=NULL, token){
  api <- "https://api.acaps.org/api/v1"
  query <- paste(api, database, sep = "/")
  if(!is.null(dataset)) query <- paste(query, dataset, sep = "/")
  if(!is.null(date)) query <- paste(query, date, sep = "/")
  if(!is.null(parameters)) query <- paste(query, parameters, sep = "/")
  result <- data.table()
  while(T){
    response <- fromJSON(content(GET(query, add_headers(Authorization = token)), "text", encoding = "UTF-8"))
    result_temp <- response$results
    result <- rbind(result, result_temp)
    if("next" %in% names(response) & !is.null(response[["next"]])){
      query <- response[["next"]]
      Sys.sleep(1)
    } else {
      break
    }
  }
  return(result)
}

crises <- get.acaps(database = "crises", token = token)

dates <- as.POSIXlt("2019-01-01")
dates$mon <- seq(0,25)
dates <- format.Date(dates, "%b%Y")

pin.out <- data.table()
for(i in 1:length(dates)){
  message(dates[i])
  pin_temp <- get.acaps(database = "inform-severity-index", dataset = "conditions-of-people-affected", date = dates[i], token = token)
  pin_temp$mon <- substr(dates[i], 1, 3)
  pin_temp$year <- substr(dates[i], 4, 8)
  pin.out <- rbind(pin.out, pin_temp, fill = T)
  rm(pin_temp)
}

pin <- pin.out

pin[, `:=` (Total.PiN = sum(c(`# of people in moderate conditions - level 3`, `# of people severe conditions - level 4`, `# of people extreme conditions - level 5`), na.rm = T)
            ,check = sum(c(`% of people in none/minimal conditions - Level 1`, `% of people in stressed conditions - level 2`, `% of people in moderate conditions - level 3`, `% of people severe conditions - level 4`, `% of people extreme conditions - level 5`), na.rm = T))
    , by = .(crisis_id, year, mon)]

pin[, `:=` (country_flat = sapply(country, paste0, collapse = ", "), iso3_flat = sapply(iso3, paste0, collapse = ", "))]

pin.max <- pin[pin[floor(check) <= 110, .I[which.max(Total.PiN)], by = .(year, crisis_id)]$V1]

pin.max <- merge(pin.max, income_pop, by.x = "iso3_flat", by.y = "ISO3", all.x = T)

pin.max <- pin.max[order(year, crisis_id)]

pin.max[, seq := seq(1, .N), by = .(country_flat, year)] #Establish hierarchy of crises based on listed order
pin.max[, full.parent := sum(Total.PiN[seq > 1]) >= Total.PiN[seq == 1], by = .(country_flat, year)] #Examine whether the parent crisis (seq == 1) is likely to contain all children, or is a separate crisis itself

pin.max[, Adjusted.PiN := ifelse(.N > 1 & full.parent == FALSE & seq == 1, Total.PiN[seq == 1] - sum(Total.PiN[seq > 1]), Total.PiN), by = .(country_flat, year)]

crisis.classes <- list(`Physical` = list("Drought", "Flood", "Earthquake", "Tropical cyclone", "Epidemic", "Industrial Accidents", "Hurricane", "Monsoon", "Measles", "Tsunami", "Volcano", "Typhoon", "Tropical storm", "Food security", "Food insecurity"),
                             `Conflict` = list("Conflict", "Other type of violence"),
                             `Displacement` = list("International displacement", "Refugee", "Migration", "Displacement"),
                             `Complex` = list("Complex crisis", "Complex situation", "Complex", "Political and economic crisis", "Socioeconomic crisis", "Multiple crises", "Regional crisis"))

pin.max[, crisis_category := data.table(apply(as.data.table(lapply(crisis.classes, function(x) grepl(paste0(x, collapse = "|"), paste(crisis_name, type_of_crisis), ignore.case = T))), 1, function(x) ifelse(length(x[x]) > 1, "Complex", names(x)[x])))]

pin.selected <- pin.max[!(seq == 1 & full.parent)]
pin.complex <- pin.selected[regional_or_country != "Regional" & crisis_category == "Complex"]

pin.selected <- rbind(pin.complex[country_flat %in% gha.conflict, crisis_category := "Conflict"],
      pin.complex[country_flat %in% gha.displacement][, crisis_category := "Displacement"],
      pin.complex[country_flat %in% gha.physical][, crisis_category := "Physical"],
      pin.complex[!(country_flat %in% gha2020$Country)][, crisis_category := "Complex"],
      pin.selected[crisis_category != "Complex"])

top.level <- rbind(pin.max[country_level == "Yes" & Total.PiN > 0, .(PiN = sum(Total.PiN)), by = .(year, crisis_category)][crisis_category == "Complex"],
      pin.selected[regional_or_country != "Regional" & Total.PiN > 0, .(PiN = sum(Adjusted.PiN)), by = .(year, crisis_category)][crisis_category != "Complex"]
      )[order(crisis_category)]

income.level <- rbind(
  pin.max[country_level == "Yes" & Total.PiN > 0, .(PiN = sum(Total.PiN)/2), by = .(crisis_category, income)][crisis_category == "Complex"],
  pin.selected[regional_or_country != "Regional" & Total.PiN > 0, .(PiN = sum(Adjusted.PiN)/2), by = .(crisis_category, income)][crisis_category != "Complex"]
  )[order(income, crisis_category)]

income.level <- merge(income.level, income_pop[, .(population = sum(population)/1000000), by = income])[, .(PiN = PiN, share_PiN = PiN/population), by = .(income)]

fwrite(pin.max, "ACAPS_PiN.csv")