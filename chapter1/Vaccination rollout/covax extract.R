required.packages <- c("data.table","XML", "httr", "jsonlite", "stringr", "readxl")
lapply(required.packages, require, character.only=T)

setwd("G:/My Drive/Work/GitHub/gha_report_2021/")

countrynames <- fread("datasets/Countrynames/isos.csv", encoding = "UTF-8")
pcc <- as.data.table(read_excel("datasets/PCC/Protracted Response, 2000-2021.xlsx", sheet = "Calcs."))
pcc <- pcc[, -c(3:24)]
pcc <- merge(pcc, countrynames[, c("iso2", "iso3")], by.x = "Country ID", by.y = "iso2", all.x = T)

gavi <- "https://www.gavi.org/covax-vaccine-roll-out"

doc <- htmlParse(GET(gavi))
links <- xpathSApply(doc, "//a/@href")

links <- unique(links[grepl("/covax-vaccine-roll-out/", links)])

broken <- c("ukraine","mauritania","congo-dr","bolivia", "georgia", "north-macedonia","brazil","colombia","honduras","paraguay","indonesia", "jordan")

dt <- list()
for(i in 1:length(links)){
  link <- paste0("https://www.gavi.org", links[i])
  page <- htmlParse(GET(link))
  country <- gsub(".*covax-vaccine-roll-out/", "", link)
  vac_dat <- xpathSApply(page, "//div[@class='-desktop-only']/ul/li|//div[@class='-desktop-only']//td", xmlValue)
  
  if(country %in% broken){
  
    if(country == "ukraine"){
      vacs <- data.table(
        country = country,
        vaccine = gsub(" vaccine|[*]", "", grep("vaccine", vac_dat, value = T)),
        received = as.numeric(gsub("\\D+", "", grep("received", vac_dat, value = T))),
        allocated = as.numeric(gsub("\\D+", "", grep("allocated", vac_dat, value = T)))
      )
    }
    
    if(country == "mauritania"){
      vacs <- data.table(
        country = country,
        vaccine = strsplit(gsub(",|\\d+|Doses received.{0,1}\n\t|Doses allocated.{0,1}\n\t|[*]| vaccine|.*:|.*: ", "", grep("vaccine", vac_dat, value = T)),"[|]")[[1]],
        received = as.numeric(gsub("\\D+", "", gsub("\\(.*\\)", "", strsplit(gsub("Doses received.{0,1}\n\t", "", grep("Doses received", vac_dat, value = T)), "[|]")[[1]]))),
        allocated = as.numeric(gsub("\\D+", "", gsub("\\(.*\\)", "", strsplit(gsub("Doses allocated.{0,1}\n\t", "", grep("Doses allocated", vac_dat, value = T)), "[|]")[[1]])))/2
      )
    } 
    
    if(country == "congo-dr"){
      vacs <- data.table(
        country = country,
        vaccine = strsplit(gsub(",|\\d+|Doses received.{0,1}\n\t|Doses allocated.{0,1}\n\t|[*]| vaccine|.*:|.*: |[.]|million", "", grep("vaccine", vac_dat, value = T)),"\n\t")[[1]],
        received = as.numeric(gsub("\\D+", "", gsub("\\(.*\\)", "", strsplit(gsub("Doses received.{0,1}\n\t", "", grep("Doses received", vac_dat, value = T)), "\n\t")[[1]])))*100000,
        allocated = as.numeric(gsub("\\D+", "", gsub("\\(.*\\)", "", strsplit(gsub("Doses allocated.{0,1}\n\t", "", grep("Doses allocated", vac_dat, value = T)), "\n\t")[[1]])))
      )
    }
    
    if(country %in% c("bolivia", "georgia", "north-macedonia", "jordan")){
      vacs <- data.table(
        country = country,
        vaccine = strsplit(gsub(",|\\d+|Doses received.{0,1}\n\t|Doses allocated.{0,1}\n\t|[*]| vaccine|.*:|.*: ", "", grep("vaccine", vac_dat, value = T)),"\n\t")[[2]],
        received = c(as.numeric(gsub("\\D+", "", gsub("\\(.*\\)", "", strsplit(gsub("Doses received.{0,1}\n\t", "", grep("Doses received", vac_dat, value = T)), "\n\t")[[1]]))),0),
        allocated = as.numeric(gsub("\\D+", "", gsub("\\(.*\\)", "", strsplit(gsub("Doses allocated.{0,1}\n\t", "", grep("Doses allocated", vac_dat, value = T)), "\n\t")[[1]])))
      )
    }
    
    if(country == "brazil"){
      vacs <- data.table(
        country = country,
        vaccine = strsplit(gsub(",|Doses received.{0,1}|Doses allocated.{0,1}|[*]| vaccine|:.*", "", grep("vaccine", vac_dat, value = T)),"[|]")[[1]],
        received = as.numeric(gsub("\\D+", "", gsub("\\(.*\\)", "", strsplit(gsub("Doses received.{0,1}", "", grep("Doses received", vac_dat, value = T)), "-")[[1]][3]))),
        allocated = as.numeric(gsub("\\D+", "", gsub("\\(.*\\)", "", strsplit(gsub("Doses allocated.{0,1}\n\t", "", grep("Doses allocated", vac_dat, value = T)), "[|]")[[1]])))
      )
    } 
    
    if(country == "colombia"){
      vacs <- data.table(
        country = country,
        vaccine = gsub(",|Doses received.{0,1}|Doses allocated.{0,1}|[*]| vaccine|:.*", "", grep("vaccine", vac_dat, value = T))[c(1,2)],
        received = as.numeric(gsub("\\D+", "", gsub(".* - ", "", gsub("Doses received.{0,1}", "", grep("Doses received", vac_dat, value = T))))),
        allocated = as.numeric(gsub("\\D+", "", gsub("\\(.*\\)", "", strsplit(gsub("Doses allocated.{0,1}\n\t", "", grep("Doses allocated", vac_dat, value = T)), "\n\t")[[1]])))
      )
    } 
    
    if(country %in% c("honduras", "paraguay")){
      vacs <- data.table(
        country = country,
        vaccine = gsub(",|Doses received.{0,1}|Doses allocated.{0,1}|[*]| vaccine|:.*", "", grep("vaccine", vac_dat, value = T)),
        received = as.numeric(gsub("\\D+", "", gsub(".* - ", "", gsub("Doses received.{0,1}", "", grep("Doses received", vac_dat, value = T))))),
        allocated = as.numeric(gsub("\\D+", "", gsub("\\(.*\\)", "", strsplit(gsub("Doses allocated.{0,1}\n\t", "", grep("Doses allocated", vac_dat, value = T)), "\n\t")[[1]])))
      )
    } 
    
    if(country == "indonesia"){
      vacs <- data.table(
        country = country,
        vaccine = strsplit(gsub(",|\\d+|Doses received.{0,1}\n\t|Doses allocated.{0,1}\n\t|[*]| vaccine|.*:|.*: |[.]|million", "", grep("vaccine", vac_dat, value = T)),"\n\t")[[1]],
        received = as.numeric(gsub("\\D+", "", gsub("\\(.*\\)", "", strsplit(gsub("Doses received.{0,1}\n\t", "", grep("Doses received", vac_dat, value = T)), "\n\t")[[1]][2])))*100000,
        allocated = as.numeric(gsub("\\D+", "", gsub("\\(.*\\)", "", strsplit(gsub("Doses allocated.{0,1}\n\t", "", grep("Doses allocated", vac_dat, value = T)), "\n\t")[[1]])))
      )
    }
    
  }
  
  else {
    vacs <- data.table(
      country = country,
      vaccine = strsplit(gsub(",|\\d+|Doses received.{0,1}\n\t|Doses allocated.{0,1}\n\t|[*]| vaccine|.*:|.*: ", "", grep("vaccine", vac_dat, value = T)),"\n\t")[[1]],
      received = as.numeric(gsub("\\D+", "", gsub("\\(.*\\)", "", strsplit(gsub("Doses received.{0,1}\n\t", "", grep("Doses received", vac_dat, value = T)), "\n\t")[[1]]))),
      allocated = as.numeric(gsub("\\D+", "", gsub("\\(.*\\)", "", strsplit(gsub("Doses allocated.{0,1}\n\t", "", grep("Doses allocated", vac_dat, value = T)), "\n\t")[[1]])))
    )
    
  }
  
  dt[[i]] <- vacs
}

dt <- rbindlist(dt)

dt[!(country %in% c("guinea-bissau", "timor-leste")), country := gsub("-", " ", country)]
dt[, country := gsub("And", "and", str_to_title(country))]

dt[country == "Congo Dr"]$country <- "Democratic Republic of the Congo"
dt[country == "Cote Divoire"]$country <- "Côte d'Ivoire"
dt[country == "Syria"]$country <- "Syrian Arab Republic"
dt[country == "West Bank and Gaza"]$country <- "West Bank and Gaza Strip"
dt[country == "Lao Pdr"]$country <- "Lao People's Democratic Republic"
dt[country == "Vietnam"]$country <- "Viet Nam"

shares <- dt[, .(received = sum(received), allocated = sum(allocated), share = sum(received)/sum(allocated)), by = country]

shares <- merge(shares, countrynames[, c("countryname_oecd", "iso3")], by.x = "country", by.y = "countryname_oecd")
shares <- merge(shares, pcc[, c("iso3", "Protracted Crisis in 2020")], by = "iso3", all.x = T)
shares[is.na(`Protracted Crisis in 2020`), `Protracted Crisis in 2020` := "No"]

shares[, .(share_avg = mean(share), share_wavg = sum(received)/sum(allocated), received = sum(received), allocated = sum(allocated)), by = `Protracted Crisis in 2020`]

pop <- fread("datasets/Population/WPP2019_TotalPopulationBySex.csv", encoding = "UTF-8")
pop <- pop[Time == 2020 & Variant == "Medium"]
pop <- merge(pop[, .(Location, PopTotal)], countrynames[, c("iso3", "countryname_un")], by.x = "Location", by.y = "countryname_un")

shares <- merge(shares, pop[, .(iso3, PopTotal)], by = "iso3", all.x = T)[, PopTotal := PopTotal*1000]

shares[, `:=` (received_per_cap = received/PopTotal, allocated_per_cap = allocated/PopTotal)]
shares[, .(received_per_cap = sum(received)/sum(PopTotal, na.rm = T), allocated_per_cap = sum(allocated)/sum(PopTotal, na.rm = T)), by = `Protracted Crisis in 2020`]
