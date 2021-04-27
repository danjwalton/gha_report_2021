required.packages <- c("data.table", "Hmisc", "stringr")
lapply(required.packages, require, character.only=T)

setwd("G:/My Drive/Work/GitHub/gha_report_2021/")

isos <- fread("datasets/Countrynames/isos.csv", encoding = "UTF-8")

dhs_surveys <- fread(tail(list.files("datasets/DHSMICS/", "dhs_metadata.*[.]csv", full.names = T),1), encoding = "UTF-8")
mics_surveys <- fread(tail(list.files("datasets/DHSMICS/", "mics_metadata.*[.]csv", full.names = T),1), encoding = "UTF-8")

dhs_surveys[, `:=` (filename = paste0(dhs_cc,"HR",dhs_recode_code,"DT"), survey_type = "dhs")]
dhs_surveys <- dhs_surveys[dhs_cc != "",c("Country.", "Contract.Phase.", "surveyyr", "filename", "survey_type")]
dhs_surveys <- merge(dhs_surveys, isos[, c("countryname_dhs", "iso3", "iso2")], by.x = "Country.", by.y = "countryname_dhs", all.x = T)

mics_surveys[, `:=` (filename = gsub("%20", "_", (gsub(".*/|[.]zip", "", dataset.url))), survey_type = "mics"), by = dataset.url]
mics_surveys <- mics_surveys[,c("country", "round", "year", "filename", "survey_type")]
mics_surveys <- merge(mics_surveys, isos[, c("countryname_mics", "iso3", "iso2")], by.x = "country", by.y = "countryname_mics", all.x = T)

setnames(dhs_surveys, names(mics_surveys))

all_surveys <- rbind(dhs_surveys, mics_surveys)
all_surveys <- unique(all_surveys)

all_surveys[, year := as.numeric(substr(year, nchar(year) - 3, nchar(year)))]

all_hh <- list()
pb <- txtProgressBar(0, nrow(all_surveys), style = 3)
for(i in 1:nrow(all_surveys)){
  survey <- all_surveys[i]
  filename <- survey$filename
  survey_type <- survey$survey_type
  setTxtProgressBar(pb, i)
  
  ##DHS##
  if(survey_type == "dhs"){
    data_path <- "datasets/DHSMICS/dhs_data/"
    country <- tolower(substr(filename,1,2))
    phase <- tolower(substr(filename,5,6))
    
    hr_path <- paste0(data_path,country,"hr",phase,"fl.RData")
    if(!file.exists(hr_path)) next
    load(hr_path)
    hr <- as.data.table(data)
    rm(data)
    
    keep <- c("hhid","hv001","hv002","hv005","hv012","hv024","hv025","hv271")
    hr <- hr[, c(names(hr) %in% keep), with = F]
    if(any(!(keep %in% names(hr)))) next #If missing ANY chosen variable, skip to next
    
    #ids
    setnames(hr, keep, c("hhid", "cluster", "household", "hh_weight", "hh_size", "region", "urban_rural", "wealth"), skip_absent = T)
    hr$hh_weight <- hr$hh_weight/100000
    
    #wealth
    if("wealth" %in% names(hr)){
      hr$wealth <- hr$wealth/100000
      
    } else {
      wi_path <- paste0(data_path, country, "wi", phase,"fl.RData")
      
      if(!file.exists(wi_path)){
        wi_path <- paste0(data_path, country, "wi", as.numeric(phase)-1, "fl.RData")
        
      }
      if(!file.exists(wi_path)) next
      
      load(wi_path)
      wi <- as.data.table(data)
      rm(data)
      wi <- wi[, c("whhid", "wlthindf")]
      hr <- merge(hr, wi, by.x = "hhid", by.y= "whhid")
      rm(wi)
      names(hr)[names(hr) == "wlthindf"] <- "wealth"
      
    }
    
    #region
    regions_key <- attributes(hr)$label.table$HV024
    urban_rural_key <- attributes(hr)$label.table$HV025
    
    hr$region <- names(regions_key)[match(hr$region, regions_key)]
    hr$urban_rural <- names(urban_rural_key)[match(hr$urban_rural, urban_rural_key)]
  }
    
  ##MICS##
  if(survey_type == "mics"){
    data_path <- "datasets/DHSMICS/mics_data/"
    
    set_path <- paste0(data_path, filename, ".RData")
    if(!file.exists(set_path)) next
    load(set_path)
    hr <- as.data.table(hh)
    suppressWarnings(rm(list=c("ch", "hh", "hl", "wm", "mn", "bh", "ph", "pn", "who_z", "fg", "tn", "fs")))
    
    find_name_priority <- function(names, find){
      found <- lapply(find, function(x) (toupper(names)) %in% x)[find %in% toupper(names)]
      if(length(found) >= 1){
        return(names[found[[1]]])
      } else {
        return(NA)
      }
      #Find colname by first appearance in 'find' list
    }
    
    cluster <- find_name_priority(names(hr), c("HH1", "HI1", "H01", "AFID"))
    household <- find_name_priority(names(hr), c("HH2", "HI2", "H02"))
    hh_weight <- find_name_priority(names(hr), c("HHWEIGHT", "WEIGHT"))
    hh_size <- find_name_priority(names(hr), c("HH11", "TOTMEMB", "HIMEM", "HH48", "TOTPOP"))
    region <- find_name_priority(names(hr), c("PROVINCE", "HH7", "HI7", "DISTRICT", "HH7A", "HH1A", "HH72R", "HHDIS"))
    urban_rural <- find_name_priority(names(hr), c("HH6", "HI6", "RESIDE"))
    wealth <- find_name_priority(names(hr), c("WSCORE", "WLTHSCOR", "FAC1_1", "WIND"))
    
    keep <- c(cluster, household, hh_weight, hh_size, region, urban_rural, wealth)
    
    if(any(is.na(keep))) next #If missing ANY chosen variable, skip to next
    
    hr <- hr[, c(names(hr) %in% keep), with = F]
    
    hr[, hhid := paste0(str_pad(get(cluster), 5, pad = "0"), str_pad(get(household), 5, pad = "0"))]
    
    #ids
    setnames(hr, keep, c("cluster", "household", "hh_weight", "hh_size", "region", "urban_rural", "wealth"), skip_absent = T)
  }
    
  hr[, wealth := wealth - min(wealth)]
  hr <- hr[order(wealth)]
  hr[, wpercentile := cumsum(wealth)/sum(wealth)]
  
  hr <- c(as.list(survey), data = list(hr))
  
  all_hh[[i]] <- hr
  
  if(exists("hr")) rm(hr)
}
close(pb)

all_hh[sapply(all_hh, is.null)] <- NULL
saveRDS(all_hh, "datasets/DHSMICS/subnational_wealth.RData")

###